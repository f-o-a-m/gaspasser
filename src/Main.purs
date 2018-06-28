module Main (main) where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contracts.Gaspasser.StringAttributeStore as SAS
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
import Data.Array (concat, replicate, (!!), nub)
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Lens ((?~))
import Data.Maybe (fromJust, fromMaybe)
import Data.StrMap as M
import Data.String (Pattern(..), Replacement(..), fromCharArray, replace)
import Data.Traversable (class Traversable, for, for_)
import Data.Tuple (Tuple(..))
import Initial.Deploy as Initial
import Network.Ethereum.Web3 (ETH, Provider, TransactionReceipt(..), TransactionStatus(..), _from, _gas, _to, defaultTransactionOptions, embed, runWeb3)
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
import Partial.Unsafe (unsafePartial)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Data.List.Lazy (replicateM)

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
  nodeUrl <- liftEff $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  conf <- buildTestConfig nodeUrl 60 Initial.deployScript
  let primaryAccount = unsafePartial $ fromJust $ conf.accounts !! 0
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                                  # _to   ?~ conf.ost.deployAddress
                                                  # _gas  ?~ embed 2000000


  toInsert <- liftEff $ replicateM 1000 $ randomInt 0 100000

  for_ toInsert $ \n -> do
    txHash <- retryWeb3 "insert" conf.provider $ OST.insert txOpts { value: unsafePartial fromJust <<< uIntNFromBigNumber s256 $ embed n }
    delay (Milliseconds 500.0)
    liftAff $ log $ "txHash for inserting " <> show n <> " with txHash:  " <> show txHash
  
