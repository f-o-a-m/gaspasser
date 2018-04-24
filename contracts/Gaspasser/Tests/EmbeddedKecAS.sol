pragma solidity^0.4.11;

import "Gaspasser/StringAttributeStore.sol";

contract EmbeddedKecAS is StringAttributeStore {
    mapping(bytes32 => string) attributes;

    function getAttribute(string key) public constant returns (string) {
        return attributes[keccak256(key)];
    }

    function setAttribute(string key, string value) public {
        attributes[keccak256(key)] = value;
    }
}