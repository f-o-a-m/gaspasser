module Main (main) where

import Prelude

import Chanterelle.Internal.Utils (pollTransactionReceipt)
import Chanterelle.Test (buildTestConfig)
import Contracts.Gaspasser.StringAttributeStore as SAS
import Control.Monad.Aff (launchAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console (CONSOLE, log)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Control.Parallel (parTraverse)
import Data.Argonaut (stringify)
import Data.Array (concat, replicate, (!!))
import Data.Either (Either(..))
import Data.Enum (enumFromTo)
import Data.Lens ((?~))
import Data.Maybe (fromJust, fromMaybe)
import Data.String (fromCharArray)
import Data.Traversable (for, for_)
import Data.Tuple (Tuple(..))
import Initial.Deploy as Initial
import Network.Ethereum.Web3 (ETH, TransactionReceipt(..), TransactionStatus(..), _from, _gas, _to, defaultTransactionOptions, embed, runWeb3)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS)
import Node.FS.Aff as FS
import Node.Process (PROCESS)
import Node.Process as NP
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

keyLengthLimit :: Int
keyLengthLimit = 2

valueLengthLimit :: Int
valueLengthLimit = 2

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
  eRes :: _ <- runWeb3 conf.provider <<< flip parTraverse targetContracts $ \{contract, opts} -> Tuple contract <$> do
    for ((enumFromTo 1 keyLengthLimit) :: Array Int) $ \keyLen -> 
      for ((enumFromTo 1 valueLengthLimit) :: Array Int) $ \valLen -> do
        let key   = fromCharArray $ replicate keyLen 'a'
            value = fromCharArray $ replicate valLen 'a'
        txHash <- SAS.setAttribute opts { key, value }
        TransactionReceipt txReceipt <- pollTransactionReceipt txHash conf.provider
        let status = txReceipt.status
            gasUsed = show txReceipt.gasUsed
        liftAff <<< log $ case status of
            Succeeded -> "SUCCESS! " <> contract <> " @ " <> show keyLen <> "/" <> show valLen <> ": " <> gasUsed
            Failed    -> "FAIL :(! " <> contract <> " @ " <> show keyLen <> "/" <> show valLen <> ": " <> gasUsed
        pure { status, gasUsed, keyLen, valLen }
  case eRes of
    Left err -> liftAff $ log $ "We had a super fail :((((((( " <> unsafeCoerce err
    Right results -> for_ results $ \(Tuple contractName res) -> do
      let flattenedResults = concat res
      FS.writeTextFile UTF8 ("collected-results/" <> contractName <> ".json") (stringify $ unsafeCoerce flattenedResults)
