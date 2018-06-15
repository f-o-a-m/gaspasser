module Initial.ContractConfig where

import Chanterelle.Internal.Types (ContractConfig, NoArgs, noArgs, constructorNoArgs)
import Network.Ethereum.Web3 (Address)

type Contract a = { deployAddress :: Address
                  , deployArgs    :: Record a 
                  }

type NoopAS = NoArgs
noopASConfig :: ContractConfig NoopAS
noopASConfig =
    { filepath: "./abis/Gaspasser/Tests/NoopAS.json"
    , name: "NoopAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }


type EmbeddedKecAS = NoArgs
embeddedKecASConfig :: ContractConfig EmbeddedKecAS
embeddedKecASConfig = 
    { filepath: "./abis/Gaspasser/Tests/EmbeddedKecAS.json"
    , name: "EmbeddedKecAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type EmbeddedRawStrAS = NoArgs
embeddedRawStrASConfig :: ContractConfig EmbeddedRawStrAS
embeddedRawStrASConfig = 
    { filepath: "./abis/Gaspasser/Tests/EmbeddedRawStrAS.json"
    , name: "EmbeddedRawStrAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type KecAS = NoArgs
kecASConfig :: ContractConfig KecAS
kecASConfig = 
    { filepath: "./abis/Gaspasser/Tests/KecAS.json"
    , name: "KecAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type RawStrAS = NoArgs
rawStrASConfig :: ContractConfig RawStrAS
rawStrASConfig = 
    { filepath: "./abis/Gaspasser/Tests/RawStrAS.json"
    , name: "RawStrAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }