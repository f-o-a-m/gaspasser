module Main (main) where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contracts.OrderStatisticTree as OST
import Control.Monad.Aff (Aff, Milliseconds(..), delay, launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Parallel (class Parallel, parTraverse)
import Data.Argonaut (stringify)
import Data.Array (concat, (!!), reverse, sort, fromFoldable, zip)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.Foldable (sum)
import Data.Enum (enumFromTo)
import Data.Lens ((?~))
import Data.Maybe (fromJust, fromMaybe)
import Data.StrMap as M
import Data.String (Pattern(..), Replacement(..), fromCharArray, replace)
import Data.Traversable (class Traversable, for)
import Data.Tuple (Tuple(..))
import Initial.Deploy as Initial
import Network.Ethereum.Core.BigNumber (unsafeToInt)
import Network.Ethereum.Web3 (ETH, Provider, TransactionReceipt(..), _from, _gas, _to, defaultTransactionOptions, embed, runWeb3)
import Network.Ethereum.Web3.Api (eth_getTransactionReceipt)
import Network.Ethereum.Web3.Solidity (uIntNFromBigNumber)
import Network.Ethereum.Web3.Solidity.Sizes (s256)
import Network.Ethereum.Web3.Types.Types (Web3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS)
import Node.FS.Aff as FS
import Node.Process (PROCESS)
import Node.Process as NP
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List.Lazy (replicateM)
import Statistics.Sample (mean, variance, stddev)

keyLengthLimit :: Int
keyLengthLimit = 257

valueLengthLimit :: Int
valueLengthLimit = 257

-- | Traverse a collection in parallel.
parFor :: forall f m t a b
        . Parallel f m
       => Traversable t
       => t a
       -> (a -> m b)
       -> m (t b)
parFor = flip parTraverse

retryWeb3 :: forall e a
           . String
          -> Provider
          -> Web3 e a
          -> Aff (eth :: ETH | e) a
retryWeb3 actionName provider m = do
  eRes <- runWeb3 provider m
  case eRes of
    Left err -> do
      -- too lazy to fix this
      unsafeCoerceAff $ log ("Will retry due to catching an error doing " <> actionName )-- <> ": " <> (stringify $ unsafeCoerce err))
      liftAff $ delay (Milliseconds 500.0)
      retryWeb3 actionName provider m
    Right res -> pure res

main :: forall e 
      . Eff ( now       :: NOW
            , console   :: CONSOLE
            , eth       :: ETH
            , fs        :: FS
            , process   :: PROCESS
            , exception :: EXCEPTION
            , random    :: RANDOM
            | e
            ) Unit
main = void <<< launchAff $ do
  toInsert' <- liftEff $ replicateM 1000 $ randomInt 0 10000
  let toInsert = fromFoldable toInsert'

  parFor (zip [sort toInsert, reverse <<< sort $ toInsert, toInsert] ["inc", "dec", "rnd"]) $ \(Tuple set order) -> do
    nodeUrl <- liftEff $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
    conf <- buildTestConfig nodeUrl 60 Initial.deployScript
    let primaryAccount = unsafePartial $ fromJust $ conf.accounts !! 0
        txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                                    # _to   ?~ conf.ost.deployAddress
                                                    # _gas  ?~ embed 2000000
    liftAff <<< log $ "Adding order: " <> order
    tots <- for set $ \n -> do
      txHash <- retryWeb3 "insert" conf.provider $ OST.insert txOpts { value: unsafePartial fromJust <<< uIntNFromBigNumber s256 $ embed n }
      delay (Milliseconds 2000.0)
      TransactionReceipt txReceipt <- retryWeb3 ("eth.getTransactionReceipt(\"" <> show txHash <> "\")") conf.provider (eth_getTransactionReceipt txHash)

      let status = txReceipt.status
          gasUsed = unsafeToInt txReceipt.gasUsed
          funCallStr = show n
      --liftAff <<< log $ case status of
      --  Succeeded -> "SUCCESS! " <> funCallStr <> ": " <> show gasUsed <> " / " <> show txHash
      --  Failed    -> "FAIL :(! " <> funCallStr <> ": " <> show gasUsed <> " / " <> show txHash
      pure gasUsed
    let totalGas = sum tots
    liftAff <<< log $ "Total gas for " <> order <> ": " <> show totalGas
      <> "\tMean: " <> show (mean $ toNumber <$> tots) <> " per tx. "
      <> "\tVariance: " <> show (variance $ toNumber <$> tots)
      <> "\tStd. deviation: " <> show (stddev $ toNumber <$> tots)
