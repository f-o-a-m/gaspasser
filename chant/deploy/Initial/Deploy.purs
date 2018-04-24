module Initial.Deploy (deployScript) where

import Prelude

import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Network.Ethereum.Web3 (defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Core.BigNumber (parseBigNumber, decimal)
import Partial.Unsafe (unsafePartial)

deployScript :: forall eff. DeployM eff Unit --(Record DeployResults)
deployScript = pure unit