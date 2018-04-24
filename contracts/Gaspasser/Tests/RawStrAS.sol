pragma solidity^0.4.11;

import "Gaspasser/StringAttributeStore.sol";
import "Gaspasser/Libs/RawStringAttributeStore.sol";

contract RawStrAS is StringAttributeStore {
    using RawStringAttributeStore for RawStringAttributeStore.AttrStore;
    RawStringAttributeStore.AttrStore attributes;

    function getAttribute(string key) public constant returns (string) {
        return attributes.getAttr(key);
    }

    function setAttribute(string key, string value) public {
        attributes.setAttr(key, value);
    }
}