pragma solidity ^0.5.0;

import "./openzeppelin/token/ERC20/ERC20Detailed.sol";
import "./openzeppelin/token/ERC20/ERC20Mintable.sol";
import "./openzeppelin/token/ERC20/ERC20Burnable.sol";
import "./openzeppelin/GSN/Context.sol";

contract FRY is Context, ERC20Detailed, ERC20Mintable, ERC20Burnable {
    using SafeMath for uint;

    constructor(
        address teamToastMultisig,
        address teamToastTokenReceiver,
        address foundationTokenReceiver,
        address forPublicTokenReceiver)
        public ERC20Detailed("Foundry Logistics Token", "FRY", 18)
    {
        // Out of 1 million tokens,

        // 30% disbursed to Team Toast members
        _mint(teamToastTokenReceiver, uint(300000).mul(10 ** uint256(decimals())));

        // 10% given to the Foundry Foundation
        _mint(foundationTokenReceiver, uint(100000).mul(10 ** uint256(decimals())));

        // 60% set aside to be disbursed via one or more public token events
        _mint(forPublicTokenReceiver, uint(600000).mul(10 ** uint256(decimals())));

        // Team Toast will control a minting token, to be renounced as various Foundry contracts prove stable and self-organizing
        _addMinter(teamToastMultisig);
    }
}