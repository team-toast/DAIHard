pragma solidity ^0.5.0;

contract test
{
    uint public time;
    uint public item;

    constructor(uint _item)
        public
    {
        time = block.timestamp;
        item = _item;
    }
}