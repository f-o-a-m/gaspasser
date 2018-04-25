module Main (main) where

import Prelude

import Chanterelle.Test (buildTestConfig)
import Contracts.Gaspasser.StringAttributeStore as SAS
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
import Data.Array (concat, replicate, (!!))
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
import Network.Ethereum.Web3.Types.Types (Web3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS)
import Node.FS.Aff as FS
import Node.Process (PROCESS)
import Node.Process as NP
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

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
            | e
            ) Unit
main = void <<< launchAff $ do
  nodeUrl <- liftEff $ fromMaybe "http://localhost:8545" <$> NP.lookupEnv "NODE_URL"
  conf <- buildTestConfig nodeUrl 60 Initial.deployScript
  let primaryAccount = unsafePartial $ fromJust $ conf.accounts !! 0
      txOpts contract = defaultTransactionOptions # _from ?~ primaryAccount
                                                  # _to   ?~ contract
                                                  # _gas  ?~ embed 2000000
      targetContracts = [ { contract: "emKecAS"
                          , opts: txOpts conf.emKecAS.deployAddress
                          }
                        , { contract: "emRSAS"
                          , opts: txOpts conf.emRSAS.deployAddress
                          }
                        , { contract: "kecAS"
                          , opts: txOpts conf.kecAS.deployAddress
                          }
                        , { contract: "rsAS"
                          , opts: txOpts conf.rsAS.deployAddress
                          }
                        ]
  results <- for targetContracts $ \{contract, opts} -> Tuple contract <$> do
    for ((enumFromTo 0 keyLengthLimit) :: Array Int) $ \keyLen ->
      parFor ((enumFromTo 0 valueLengthLimit) :: Array Int) $ \valLen -> do
        let key   = fromCharArray $ replicate keyLen 'a'
            value = fromCharArray $ replicate valLen 'a'
            funCallStr = contract <> ".setAttribute(<" <> show keyLen <> ">, <" <> show valLen <> ">)"
        log $ "We're doing " <> funCallStr
        txHash <- retryWeb3 funCallStr conf.provider (SAS.setAttribute opts { key, value })
        log $ "We're waiting for the receipt for " <> funCallStr
        delay (Milliseconds 2000.0)
        TransactionReceipt txReceipt <- retryWeb3 ("eth.getTransactionReceipt(\"" <> show txHash <> "\")") conf.provider (eth_getTransactionReceipt txHash)
        let status = txReceipt.status
            gasUsed = show txReceipt.gasUsed
        liftAff <<< log $ case status of
            Succeeded -> "SUCCESS! " <> funCallStr <> ": " <> gasUsed
            Failed    -> "FAIL :(! " <> funCallStr <> ": " <> gasUsed
        pure { status, gasUsed, keyLen, valLen }

  let rawResults = M.fromFoldable $ map concat <$> results
      rawDataStmt = "var rawData = " <> (stringify $ unsafeCoerce rawResults) <> ";"
  template <- FS.readTextFile UTF8 "results-template.html"
  let substituted = replace (Pattern "var rawData;") (Replacement rawDataStmt) template
  FS.writeTextFile UTF8 "collected-results/results.html" substituted

  for_ results $ \(Tuple contractName res) ->
    FS.writeTextFile UTF8 ("collected-results/" <> contractName <> ".json") (stringify $ unsafeCoerce res)

