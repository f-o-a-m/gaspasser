pragma solidity^0.4.11;

// This is AdChain's AttributeStore, but modified to store
// arbitrary string => string mappings which it keccaks! first

library KeccakingAttributeStore {
    struct Data {
        mapping(bytes32 => string) store;
    }

    function getAttribute(Data storage self, string key) public constant returns (string) {
        return self.store[keccak256(key)];
    }

    function setAttribute(Data storage self, string key, string val) public {
        self.store[keccak256(key)] = val;
    }
}