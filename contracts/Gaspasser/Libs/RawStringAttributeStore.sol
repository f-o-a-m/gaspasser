pragma solidity^0.4.11;

// This is AdChain's AttributeStore, but modified to store
// arbitrary string => string mappings

library RawStringAttributeStore {
    struct AttrStore {
        mapping(string => string) store;
    }

    function getAttr(AttrStore storage self, string key) public constant returns (string) {
        return self.store[key];
    }

    function setAttr(AttrStore storage self, string key, string val) public {
        self.store[key] = val;
    }
}