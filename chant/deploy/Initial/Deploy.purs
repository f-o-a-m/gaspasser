module Initial.Deploy (deployScript) where

import Prelude (Unit, bind, pure, unit, (#), ($))
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Initial.ContractConfig (embeddedKecASConfig, embeddedRawStrASConfig, kecASConfig, rawStrASConfig)
import Network.Ethereum.Web3 (defaultTransactionOptions, _from, _gas)
import Network.Ethereum.Core.BigNumber (parseBigNumber, hexadecimal)
import Partial.Unsafe (unsafePartial)

deployScript :: forall eff. DeployM eff Unit --(Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber hexadecimal "0x200000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  emKecAS <- deployContract txOpts embeddedKecASConfig
  emRSAS  <- deployContract txOpts embeddedRawStrASConfig
  kecAS   <- deployContract txOpts kecASConfig
  rsAS    <- deployContract txOpts rawStrASConfig

  pure unit