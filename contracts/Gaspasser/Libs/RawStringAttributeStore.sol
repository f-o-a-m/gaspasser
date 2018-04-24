pragma solidity^0.4.11;

// This is AdChain's AttributeStore, but modified to store
// arbitrary string => string mappings

library RawStringAttributeStore {
    struct Data {
        mapping(string => string) store;
    }

    function getAttribute(Data storage self, string key) public constant returns (string) {
        return self.store[key];
    }

    function setAttribute(Data storage self, string key, string val) public {
        self.store[key] = val;
    }
}