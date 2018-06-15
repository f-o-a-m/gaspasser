module Initial.Deploy (deployScript) where
import Prelude (bind, pure, (#), ($))
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Initial.ContractConfig (Contract, EmbeddedKecAS, EmbeddedRawStrAS, KecAS, NoopAS, RawStrAS, embeddedKecASConfig, embeddedRawStrASConfig, kecASConfig, noopASConfig, rawStrASConfig)
import Network.Ethereum.Core.BigNumber (parseBigNumber, hexadecimal)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

type DeployResults = ( noopAS  :: Contract NoopAS
                     , emKecAS :: Contract EmbeddedKecAS
                     , emRSAS  :: Contract EmbeddedRawStrAS
                     , kecAS   :: Contract KecAS
                     , rsAS    :: Contract RawStrAS
                     )

deployScript :: forall eff. DeployM eff (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber hexadecimal "0x200000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  noopAS  <- deployContract txOpts noopASConfig
  emKecAS <- deployContract txOpts embeddedKecASConfig
  emRSAS  <- deployContract txOpts embeddedRawStrASConfig
  kecAS   <- deployContract txOpts kecASConfig
  rsAS    <- deployContract txOpts rawStrASConfig

  pure { noopAS, emKecAS, emRSAS, kecAS, rsAS }