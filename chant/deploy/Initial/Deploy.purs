module Initial.Deploy (deployScript) where
import Prelude (bind, pure, (#), ($))
import Chanterelle.Deploy (deployContract)
import Chanterelle.Internal.Types (DeployConfig(..), DeployM)
import Control.Monad.Reader.Class (ask)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Initial.ContractConfig (Contract, EmbeddedKecAS, OST, EmbeddedRawStrAS, KecAS, NoopAS, RawStrAS, embeddedKecASConfig, embeddedRawStrASConfig, kecASConfig, noopASConfig, rawStrASConfig, ostConfig)
import Network.Ethereum.Core.BigNumber (parseBigNumber, hexadecimal)
import Network.Ethereum.Web3 (_from, _gas, defaultTransactionOptions)
import Partial.Unsafe (unsafePartial)

type DeployResults = ( ost     :: Contract OST
                     )

deployScript :: forall eff. DeployM eff (Record DeployResults)
deployScript = do
  deployCfg@(DeployConfig {primaryAccount}) <- ask
  let bigGasLimit = unsafePartial fromJust $ parseBigNumber hexadecimal "0x200000"
      txOpts = defaultTransactionOptions # _from ?~ primaryAccount
                                         # _gas ?~ bigGasLimit
  ost     <- deployContract txOpts ostConfig

  pure {ost}
