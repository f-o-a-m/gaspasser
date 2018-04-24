pragma solidity^0.4.11;

interface StringAttributeStore {
    function getAttribute(string key) public constant returns (string);
    function setAttribute(string key, string value) public;
}