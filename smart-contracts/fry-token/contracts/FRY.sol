pragma solidity ^0.5.0;

import "./openzeppelin/token/ERC20/ERC20Detailed.sol";
import "./openzeppelin/token/ERC20/ERC20Mintable.sol";
import "./openzeppelin/token/ERC20/ERC20Burnable.sol";
import "./openzeppelin/GSN/Context.sol";

contract FRY is Context, ERC20Detailed, ERC20Mintable, ERC20Burnable
{
    using SafeMath for uint;

    constructor(
            address _teamToastTokenReceiver,
            address _foundationMultisig,
            address _forPublicTokenReceiver)
        public
        ERC20Detailed("Foundry Logistics Token", "FRY", 18)
    {
        // Out of 100 million tokens,

        // 30% disbursed to Team Toast
        _mint(_teamToastTokenReceiver, uint(30000000).mul(10 ** uint256(decimals())));

        // 10% given to the Foundry Foundation
        _mint(_foundationMultisig, uint(10000000).mul(10 ** uint256(decimals())));

        // 60% set aside to be disbursed via one or more public token events
        _mint(_forPublicTokenReceiver, uint(60000000).mul(10 ** uint256(decimals())));

        // MinterRole constructor makes msg.sender a minter. Remove this role.
        _removeMinter(_msgSender());

        // The Foundation will control a minting address via a multisig, to be renounced as various Foundry contracts prove stable and self-organizing
        _addMinter(_foundationMultisig);
    }
}