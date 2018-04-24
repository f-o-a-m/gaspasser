pragma solidity^0.4.11;

import "Gaspasser/StringAttributeStore.sol";
import "Gaspasser/Libs/KeccakingAttributeStore.sol";

contract KecAS is StringAttributeStore {
    using KeccakingAttributeStore for KeccakingAttributeStore.AttrStore;
    KeccakingAttributeStore.AttrStore attributes;

    function getAttribute(string key) public constant returns (string) {
        return attributes.getAttr(key);
    }

    function setAttribute(string key, string value) public {
        attributes.setAttr(key, value);
    }
}