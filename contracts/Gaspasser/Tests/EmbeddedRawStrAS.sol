pragma solidity^0.4.11;

import "Gaspasser/StringAttributeStore.sol";

contract EmbeddedRawStrAS is StringAttributeStore {
    mapping(string => string) attributes;

    function getAttribute(string key) public constant returns (string) {
        return attributes[key];
    }

    function setAttribute(string key, string value) public {
        attributes[key] = value;
    }
}