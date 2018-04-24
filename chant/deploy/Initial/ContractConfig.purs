module Initial.ContractConfig where

import Chanterelle.Internal.Types (ContractConfig, NoArgs, noArgs, constructorNoArgs)

type EmbeddedKecASArgs = NoArgs
embeddedKecASConfig :: ContractConfig EmbeddedKecASArgs
embeddedKecASConfig = 
    { filepath: "./abis/Gaspasser/Tests/EmbeddedKecAS.json"
    , name: "EmbeddedKecAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type EmbeddedRawStrASArgs = NoArgs
embeddedRawStrASConfig :: ContractConfig EmbeddedRawStrASArgs
embeddedRawStrASConfig = 
    { filepath: "./abis/Gaspasser/Tests/EmbeddedRawStrAS.json"
    , name: "EmbeddedRawStrAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type KecASArgs = NoArgs
kecASConfig :: ContractConfig KecASArgs
kecASConfig = 
    { filepath: "./abis/Gaspasser/Tests/KecAS.json"
    , name: "KecAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }

type RawStrASArgs = NoArgs
rawStrASConfig :: ContractConfig RawStrASArgs
rawStrASConfig = 
    { filepath: "./abis/Gaspasser/Tests/RawStrAS.json"
    , name: "RawStrAS"
    , constructor: constructorNoArgs
    , unvalidatedArgs: noArgs
    }