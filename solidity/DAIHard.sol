pragma solidity 0.5.6;

/**
 * @title SafeMath
 * @dev Unsigned math operations with safety checks that revert on error.
 * Code yanked from https://github.com/OpenZeppelin/openzeppelin-solidity/blob/master/contracts/math/SafeMath.sol
 */
library SafeMath {
    /**
     * @dev Multiplies two unsigned integers, reverts on overflow.
     */
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        // Gas optimization: this is cheaper than requiring 'a' not being zero, but the
        // benefit is lost if 'b' is also tested.
        // See: https://github.com/OpenZeppelin/openzeppelin-solidity/pull/522
        if (a == 0) {
            return 0;
        }

        uint256 c = a * b;
        require(c / a == b);

        return c;
    }

    /**
     * @dev Integer division of two unsigned integers truncating the quotient, reverts on division by zero.
     */
    function div(uint256 a, uint256 b) internal pure returns (uint256) {
        // Solidity only automatically asserts when dividing by 0
        require(b > 0);
        uint256 c = a / b;
        // assert(a == b * c + a % b); // There is no case in which this doesn't hold

        return c;
    }

    /**
     * @dev Subtracts two unsigned integers, reverts on overflow (i.e. if subtrahend is greater than minuend).
     */
    function sub(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b <= a);
        uint256 c = a - b;

        return c;
    }

    /**
     * @dev Adds two unsigned integers, reverts on overflow.
     */
    function add(uint256 a, uint256 b) internal pure returns (uint256) {
        uint256 c = a + b;
        require(c >= a);

        return c;
    }

    /**
     * @dev Divides two unsigned integers and returns the remainder (unsigned integer modulo),
     * reverts when dividing by zero.
     */
    function mod(uint256 a, uint256 b) internal pure returns (uint256) {
        require(b != 0);
        return a % b;
    }
}

contract ERC20Interface {
    function totalSupply() public view returns (uint);
    function balanceOf(address tokenOwner) public view returns (uint balance);
    function allowance(address tokenOwner, address spender) public view returns (uint remaining);
    function transfer(address to, uint tokens) public returns (bool success);
    function approve(address spender, uint tokens) public returns (bool success);
    function transferFrom(address from, address to, uint tokens) public returns (bool success);

    uint8 public decimals;

    event Transfer(address indexed from, address indexed to, uint tokens);
    event Approval(address indexed tokenOwner, address indexed spender, uint tokens);
}

contract DAIHardFactory {
    using SafeMath for uint;

    event NewTrade(uint id, address tradeAddress, bool indexed initiatorIsCustodian);

    ERC20Interface public daiContract;
    address payable public founderFeeAddress;

    constructor(ERC20Interface _daiContract, address payable _founderFeeAddress)
    public {
        daiContract = _daiContract;
        founderFeeAddress = _founderFeeAddress;
    }

    struct CreationInfo {
        address address_;
        uint blocknum;
    }

    CreationInfo[] public createdTrades;

    function getDevFee(uint tradeAmount)
    public
    pure
    returns (uint founderFee) {
        return tradeAmount / 100;
    }

    /*
    The Solidity compiler can't handle much stack depth,
    so we have to pack some args together in annoying ways...
    Hence the 'uintArgs'. Here is its layout:

    0 - tradeAmount
    1 - beneficiaryDeposit
    2 - abortPunishment
    3 - pokeReward
    4 - autorecallInterval
    5 - autoabortInterval
    6 - autoreleaseInterval
    */

    function openDAIHardTrade(address payable _initiator, bool initiatorIsCustodian, uint[7] calldata uintArgs, string calldata _totalPrice, string calldata _fiatTransferMethods, string calldata _commPubkey)
    external
    returns (DAIHardTrade) {
        uint initialTransfer;
        uint[7] memory newUintArgs; // Note that this structure is not the same as the above comment describes. See below in DAIHardTrade.open.

        if (initiatorIsCustodian) {
            initialTransfer = uintArgs[0].add(uintArgs[3].add(getDevFee(uintArgs[0])));

            newUintArgs = [uintArgs[1], uintArgs[2], uintArgs[3], getDevFee(uintArgs[0]), uintArgs[4], uintArgs[5], uintArgs[6]];
        }
        else {
            initialTransfer = uintArgs[1].add(uintArgs[3].add(getDevFee(uintArgs[0])));

            newUintArgs = [uintArgs[0], uintArgs[2], uintArgs[3], getDevFee(uintArgs[0]), uintArgs[4], uintArgs[5], uintArgs[6]];
        }

        //create the new trade and add its creationInfo to createdTrades
        DAIHardTrade newTrade = new DAIHardTrade(daiContract, founderFeeAddress);
        createdTrades.push(CreationInfo(address(newTrade), block.number));
        emit NewTrade(createdTrades.length - 1, address(newTrade), initiatorIsCustodian);

        //transfer DAI to the trade and open it
        require(daiContract.transferFrom(msg.sender, address(newTrade), initialTransfer), "Token transfer failed. Did you call approve() on the DAI contract?");
        newTrade.open(_initiator, initiatorIsCustodian, newUintArgs, _totalPrice, _fiatTransferMethods, _commPubkey);

        return newTrade;
    }

    function numTrades()
    external
    view
    returns (uint num) {
        return createdTrades.length;
    }
}

contract DAIHardTrade {
    using SafeMath for uint;

    enum Phase {Created, Open, Committed, Claimed, Closed}
    Phase public phase;

    modifier inPhase(Phase p) {
        require(phase == p, "inPhase check failed.");
        _;
    }

    enum ClosedReason {NotClosed, Recalled, Aborted, Released, Burned}
    ClosedReason public closedReason;

    uint[5] public phaseStartTimestamps;
    uint[5] public phaseStartBlocknums;

    function changePhase(Phase p)
    internal {
        phase = p;
        phaseStartTimestamps[uint(p)] = block.timestamp;
        phaseStartBlocknums[uint(p)] = block.number;
    }


    address payable public initiator;
    address payable public responder;

    // The contract only has two parties, but depending on how it's opened,
    // the initiator for example might be either the custodian OR the beneficiary,
    // so we need four 'role' variables to capture each possible combination.

    bool public initiatorIsCustodian;
    address payable public custodian;
    address payable public beneficiary;

    modifier onlyInitiator() {
        require(msg.sender == initiator, "msg.sender is not Initiator.");
        _;
    }
    modifier onlyResponder() {
        require(msg.sender == responder, "msg.sender is not Responder.");
        _;
    }
    modifier onlyCustodian() {
        require (msg.sender == custodian, "msg.sender is not Custodian.");
        _;
    }
    modifier onlyBeneficiary() {
        require (msg.sender == beneficiary, "msg.sender is not Beneficiary.");
        _;
    }
    modifier onlyContractParty() { // Must be one of the two parties involved in the contract
        // Note this still covers the case in which responder still is 0x0, as msg.sender can never be 0x0.
        require(msg.sender == initiator || msg.sender == responder, "msg.sender is not a party in this contract.");
        _;
    }

    ERC20Interface daiContract;
    address payable founderFeeAddress;

    constructor(ERC20Interface _daiContract, address payable _founderFeeAddress)
    public {
        // If gas was not an issue we would leave the next three lines in for explicit clarity,
        // but technically they are a waste of gas, because we're simply setting them to the null values
        // (which happens automatically anyway when the contract is instantiated)
        
        // changePhase(Phase.Created);
        // closedReason = ClosedReason.NotClosed;
        // pokeRewardSent = false;

        daiContract = _daiContract;
        founderFeeAddress = _founderFeeAddress;
    }

    uint public tradeAmount;
    uint public beneficiaryDeposit;
    uint public abortPunishment;

    uint public autorecallInterval;
    uint public autoabortInterval;
    uint public autoreleaseInterval;

    uint public pokeReward;
    uint public founderFee;

    bool public pokeRewardSent;

    string public price;

    /* ---------------------- CREATED PHASE -----------------------

    The only reason for this phase is so the Factory can have
    somewhere to send the DAI before the Trade is initiated with
    all the settings, and moved to the Open phase.

    The Factory creates the DAIHardTrade and moves it past this state
    in a single call, so any DAIHardTrade made by the factory should
    never be "seen" in this state.

    ------------------------------------------------------------ */

    event Opened(string fiatTransferMethods, string commPubkey);

    /*
    uintArgs:
    0 - responderDeposit
    1 - abortPunishment
    2 - pokeReward
    3 - founderFee
    4 - autorecallInterval
    5 - autoabortInterval
    6 - autoreleaseInterval
    */

    function open(address payable _initiator, bool _initiatorIsCustodian, uint[7] memory uintArgs, string memory _price, string memory fiatTransferMethods, string memory commPubkey)
    public
    inPhase(Phase.Created) {
        require(getBalance() > 0, "You can't open a trade without first depositing DAI.");

        uint responderDeposit = uintArgs[0];
        abortPunishment - uintArgs[1];
        pokeReward = uintArgs[2];
        founderFee = uintArgs[3];

        autorecallInterval = uintArgs[4];
        autoabortInterval = uintArgs[5];
        autoreleaseInterval = uintArgs[6];

        initiator = _initiator;
        initiatorIsCustodian = _initiatorIsCustodian;
        if (initiatorIsCustodian) {
            custodian = initiator;
            tradeAmount = getBalance().sub(pokeReward.add(founderFee));
            beneficiaryDeposit = responderDeposit;
        }
        else {
            beneficiary = initiator;
            tradeAmount = responderDeposit;
            beneficiaryDeposit = getBalance().sub(pokeReward.add(founderFee));
        }

        require(beneficiaryDeposit <= tradeAmount, "A beneficiaryDeposit greater than tradeAmount is not allowed.");
        require(abortPunishment <= beneficiaryDeposit, "An abortPunishment greater than beneficiaryDeposit is not allowed.");

        price = _price;

        changePhase(Phase.Open);
        emit Opened(fiatTransferMethods, commPubkey);
    }

    /* ---------------------- OPEN PHASE --------------------------

    In the Open phase, the Initiator waits for a Responder.
    We move to the Commited phase once someone becomes the Responder
    by executing commit() and including msg.value = getResponderDeposit.

    At any time, the Initiator can cancel the whole thing by calling recall().

    After autorecallInterval has passed, the only state change allowed is to recall(),
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Recalled();
    event Committed(address responder, string commPubkey);


    function recall()
    external
    inPhase(Phase.Open)
    onlyInitiator() {
       internalRecall();
    }

    function internalRecall()
    internal {
        require(daiContract.transfer(initiator, getBalance()), "Recall of DAI to initiator failed!");

        changePhase(Phase.Closed);
        closedReason = ClosedReason.Recalled;

        emit Recalled();
    }

    function autorecallAvailable()
    public
    view
    inPhase(Phase.Open)
    returns(bool available) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Open)].add(autorecallInterval));
    }

    function commit(address payable _responder, string calldata commPubkey)
    external
    inPhase(Phase.Open) {
        require(daiContract.transferFrom(msg.sender, address(this), getResponderDeposit()), "Can't transfer the required deposit from the DAI contract. Did you call approve first?");
        require(!autorecallAvailable(), "autorecallInterval has passed; this offer has expired.");

        responder = _responder;

        if (initiatorIsCustodian) {
            beneficiary = responder;
        }
        else {
            custodian = responder;
        }

        changePhase(Phase.Committed);
        emit Committed(responder, commPubkey);
    }

    /* ---------------------- COMMITTED PHASE ---------------------

    In the Committed phase, the Beneficiary is expected to deposit fiat for the DAI,
    then call claim().

    Otherwise, the Beneficiary can call abort(), which cancels the contract,
    incurs a small penalty on both parties, and returns the remainder to each party.

    After autoabortInterval has passed, the only state change allowed is to abort(),
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Claimed();
    event Aborted();

    function abort()
    external
    inPhase(Phase.Committed)
    onlyBeneficiary() {
        internalAbort();
    }

    function internalAbort()
    internal {
        // Punish both parties equally by burning abortPunishment.
        // Instead of burning abortPunishment twice, just burn it all in one call (saves gas).
        require(daiContract.transfer(address(0x0), abortPunishment*2), "Token burn failed!");
        // Security note: The above line risks overflow, but only if abortPunishment >= (maxUint/2).
        // This should never happen, as abortPunishment <= beneficiaryDeposit <= tradeAmount (as required in open()),
        // which is ultimately limited by the amount of DAI the user deposited (which must be far less than maxUint/2).
        // See the note below about avoiding assert() or require() to test this.

        // Send back deposits minus burned amounts.
        require(daiContract.transfer(beneficiary, beneficiaryDeposit.sub(abortPunishment)), "Token transfer to Beneficiary failed!");
        require(daiContract.transfer(custodian, tradeAmount.sub(abortPunishment)), "Token transfer to Custodian failed!");

        uint sendBackToInitiator = founderFee;
        //If there was a pokeReward left, it should be sent back to the initiator
        if (!pokeRewardSent) {
            sendBackToInitiator = sendBackToInitiator.add(pokeReward);
        }
        
        require(daiContract.transfer(initiator, sendBackToInitiator), "Token refund of founderFee+pokeReward to Initiator failed!");
        
        // There may be a wei or two left over in the contract due to integer division. Not a big deal.
        // We could assert() or require() to test this, but then we'd risk locking the contract completely
        // if a state somehow comes that always fails the assert.
        // Better to let the function run, as at least then, in the case of a flaw, the abort mechanism "sort of" works,
        // and will return some amount to the users.

        changePhase(Phase.Closed);
        closedReason = ClosedReason.Aborted;

        emit Aborted();
    }

    function autoabortAvailable()
    public
    view
    inPhase(Phase.Committed)
    returns(bool passed) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Committed)].add(autoabortInterval));
    }

    function claim()
    external
    inPhase(Phase.Committed)
    onlyBeneficiary() {
        require(!autoabortAvailable(), "The deposit deadline has passed!");

        changePhase(Phase.Claimed);
        emit Claimed();
    }

    /* ---------------------- CLAIMED PHASE -----------------------

    In the Claimed phase, the Custodian can call release() or burn(),
    and is expected to call burn() only if the Beneficiary did not transfer
    the amount of money described in totalPrice.

    After autoreleaseInterval has passed, the only state change allowed is to release,
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Released();
    event Burned();

    function release()
    external
    inPhase(Phase.Claimed)
    onlyCustodian() {
        internalRelease();
    }

    function internalRelease()
    internal {
        //If the pokeReward has not been sent, refund it to the initiator
        if (!pokeRewardSent) {
            require(daiContract.transfer(initiator, pokeReward), "Refund of pokeReward to Initiator failed!");
        }

        //Upon successful resolution of trade, the founderFee is sent to the founders of DAIHard.
        require(daiContract.transfer(founderFeeAddress, founderFee), "Token transfer to founderFeeAddress failed!");

        //Release the remaining balance to the beneficiary.
        require(daiContract.transfer(beneficiary, getBalance()), "Final release transfer to beneficiary failed!");

        changePhase(Phase.Closed);
        closedReason = ClosedReason.Released;

        emit Released();
    }

    function autoreleaseAvailable()
    public
    view
    inPhase(Phase.Claimed)
    returns(bool available) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Claimed)].add(autoreleaseInterval));
    }

    function burn()
    external
    inPhase(Phase.Claimed)
    onlyCustodian() {
        require(!autoreleaseAvailable());

        internalBurn();
    }

    function internalBurn()
    internal {
        require(daiContract.transfer(address(0x0), getBalance()), "Final DAI burn failed!");

        changePhase(Phase.Closed);
        closedReason = ClosedReason.Burned;

        emit Burned();
    }

    /* ---------------------- OTHER METHODS ----------------------- */



    function getResponderDeposit()
    public
    view
    returns(uint responderDeposit) {
        if (initiatorIsCustodian) {
            return beneficiaryDeposit;
        }
        else {
            return tradeAmount;
        }
    }

    function getState()
    external
    view
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
        return (getBalance(), this.phase(), phaseStartTimestamps[uint(this.phase())], this.responder(), this.closedReason());
    }

    function getBalance()
    public
    view
    returns(uint) {
        return daiContract.balanceOf(address(this));
    }

    function getParameters()
    external
    view
    returns (address initiator, bool initiatorIsCustodian, uint tradeAmount, string memory totalPrice, uint beneficiaryDeposit, uint autorecallInterval, uint autoabortInterval, uint autoreleaseInterval, uint pokeReward)
    {
        return (this.initiator(), this.initiatorIsCustodian(), this.tradeAmount(), this.price(), this.beneficiaryDeposit(), this.autorecallInterval(), this.autoabortInterval(), this.autoreleaseInterval(), this.pokeReward());
    }

    function getPhaseStartInfo()
    external
    view
    returns (uint, uint, uint, uint, uint, uint, uint, uint, uint, uint)
    {
        return (phaseStartBlocknums[0], phaseStartBlocknums[1], phaseStartBlocknums[2], phaseStartBlocknums[3], phaseStartBlocknums[4], phaseStartTimestamps[0], phaseStartTimestamps[1], phaseStartTimestamps[2], phaseStartTimestamps[3], phaseStartTimestamps[4]);
    }

    // Poke function lets anyone move the contract along,
    // if it's due for some state transition.

    event Poke();

    function pokeNeeded()
    public
    view
    returns (bool needed) {
        return (  (phase == Phase.Open      && autorecallAvailable() )
               || (phase == Phase.Committed && autoabortAvailable()  )
               || (phase == Phase.Claimed   && autoreleaseAvailable())
               );
    }

    function poke()
    external 
    returns (bool moved) {
        if (pokeNeeded()) {
            daiContract.transfer(msg.sender, pokeReward);
            pokeRewardSent = true;
            emit Poke();
        }
        else return false;

        if (phase == Phase.Open) {
            if (autorecallAvailable()) {
                internalRecall();
                return true;
            }
        }
        else if (phase == Phase.Committed) {
            if (autoabortAvailable()) {
                internalAbort();
                return true;
            }
        }
        else if (phase == Phase.Claimed) {
            if (autoreleaseAvailable()) {
                internalRelease();
                return true;
            }
        }
    }

    // StatementLogs allow a starting point for any necessary communication,
    // and can be used anytime by either party after a Responder commits (even in the Closed phase).


    event InitiatorStatementLog(string encryptedForInitiator, string encryptedForResponder);
    event ResponderStatementLog(string encryptedForInitiator, string encryptedForResponder);

    function initiatorStatement(string memory encryptedForInitiator, string memory encryptedForResponder)
    public
    onlyInitiator() {
        require(phase >= Phase.Committed);
        emit InitiatorStatementLog(encryptedForInitiator, encryptedForResponder);
    }

    function responderStatement(string memory encryptedForInitiator, string memory encryptedForResponder)
    public
    onlyResponder() {
        require(phase >= Phase.Committed);
        emit ResponderStatementLog(encryptedForInitiator, encryptedForResponder);
    }
}