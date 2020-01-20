pragma solidity ^0.5.0;

import "./openzeppelin/token/ERC20/ERC20Detailed.sol";
import "./openzeppelin/token/ERC20/ERC20Mintable.sol";
import "./openzeppelin/token/ERC20/ERC20Burnable.sol";
import "./openzeppelin/GSN/Context.sol";

contract Token is Context, ERC20Detailed, ERC20Mintable, ERC20Burnable
{
    using SafeMath for uint;

    constructor(
            string memory _tokenName,
            string memory _tokenCode,
            address _tokenReceiver,
            uint _amount)
        public
        ERC20Detailed(_tokenName, _tokenCode, 18)
    {
        _mint(_tokenReceiver, _amount.mul(10 ** uint256(decimals())));
    }
}