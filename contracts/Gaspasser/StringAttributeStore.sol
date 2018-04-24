pragma solidity^0.4.11;

interface StringAttributeStore {
    function getAttribute(string key) external constant      returns (string);
    function setAttribute(string key, string value) external returns (string);
}