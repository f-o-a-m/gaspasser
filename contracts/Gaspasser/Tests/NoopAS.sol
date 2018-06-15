pragma solidity^0.4.11;

import "Gaspasser/StringAttributeStore.sol";

contract NoopAS is StringAttributeStore {
    function getAttribute(string key) public constant returns (string) {
        return "";
    }

    function setAttribute(string key, string value) public {
        return;
    }
}