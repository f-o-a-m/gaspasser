module CompileMain (main) where

import Prelude

import Chanterelle (compileMain)
import Chanterelle.Genesis (runGenesisGenerator)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Network.Ethereum.Web3 (ETH)

main :: forall eff.
      Eff
        ( now :: NOW
        , console :: CONSOLE
        , fs :: FS
        , process :: PROCESS
        , exception :: EXCEPTION
        , now :: NOW
        , eth :: ETH
        | eff
        )
        Unit
main = do
  compileMain
  runGenesisGenerator "./cliquebait.json" "./cliquebait-generated.json"