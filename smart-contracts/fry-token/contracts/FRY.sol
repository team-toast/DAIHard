pragma solidity ^0.5.0;

import "./openzeppelin/token/ERC20/ERC20Detailed.sol";
import "./openzeppelin/token/ERC20/ERC20Mintable.sol";
import "./openzeppelin/token/ERC20/ERC20Burnable.sol";
import "./openzeppelin/GSN/Context.sol";

contract FRY is Context, ERC20Detailed, ERC20Mintable, ERC20Burnable
{
    using SafeMath for uint;

    constructor(
            address _teamToastMultisig,
            address _teamToastTokenReceiver,
            address _foundationTokenReceiver,
            address _forPublicTokenReceiver)
        public
        ERC20Detailed("Foundry Logistics Token", "FRY", 18)
    {
        // Out of 1 million tokens,

        // 30% disbursed to Team Toast contributors
        _mint(_teamToastTokenReceiver, uint(300000).mul(10 ** uint256(decimals())));

        // 10% given to the Foundry Foundation
        _mint(_foundationTokenReceiver, uint(100000).mul(10 ** uint256(decimals())));

        // 60% set aside to be disbursed via one or more public token events
        _mint(_forPublicTokenReceiver, uint(600000).mul(10 ** uint256(decimals())));

        // Team Toast will control a minting address via a multisig, to be renounced as various Foundry contracts prove stable and self-organizing
        _addMinter(_teamToastMultisig);
    }
}