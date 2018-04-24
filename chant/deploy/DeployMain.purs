module DeployMain (main) where

import Prelude

import Chanterelle (deployMain)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Now (NOW)
import Network.Ethereum.Web3 (ETH)
import Node.FS.Aff (FS)
import Node.Process (PROCESS)
import Initial.Deploy (deployScript) as Initial

main :: forall e. Eff (now :: NOW, console :: CONSOLE, eth :: ETH, fs :: FS, process :: PROCESS, exception :: EXCEPTION | e) Unit
main = deployMain Initial.deployScript