pragma solidity^0.4.11;

// This is AdChain's AttributeStore, but modified to store
// arbitrary string => string mappings which it keccaks! first

library KeccakingAttributeStore {
    struct AttrStore {
        mapping(bytes32 => string) store;
    }

    function getAttr(AttrStore storage self, string key) public constant returns (string) {
        return self.store[keccak256(key)];
    }

    function setAttr(AttrStore storage self, string key, string val) public {
        self.store[keccak256(key)] = val;
    }
}