pragma solidity 0.5.2;

import "ERC20Interface.sol";

contract ToastytradeFactory {
    event NewToastytradeSell(uint id, address toastytradeSellAddress);

    ERC20Interface public tokenContract;

    struct CreationInfo {
        address address_;
        uint blocknum;
    }

    CreationInfo[] public createdSells;

    constructor(ERC20Interface _tokenContract)
    public {
        tokenContract = _tokenContract;
    }

    function createToastytradeSell(address payable _initiator, uint sellAmount, uint price, uint _responderDeposit, uint _autorecallInterval, uint _depositDeadlineInterval, uint _autoreleaseInterval, string calldata _logisticsString)
    external
    returns (ToastytradeSell) {
        ToastytradeSell newTT = new ToastytradeSell(tokenContract);

        createdSells.push(CreationInfo(address(newTT), block.number));

        require(tokenContract.transferFrom(msg.sender, address(newTT), sellAmount), "Token transfer failed. Did you call approve()?");

        newTT.open(_initiator, price, _responderDeposit, _autorecallInterval, _depositDeadlineInterval, _autoreleaseInterval, _logisticsString);

        emit NewToastytradeSell(createdSells.length-1, address(newTT));

        return newTT;
    }

    function getNumToastytradeSells()
    external
    view
    returns (uint num) {
        return createdSells.length;
    }
}

contract ToastytradeSell {
    enum Phase {Created, Open, Committed, Claimed, Closed}
    Phase public phase;

    modifier inPhase(Phase p) {
        require(phase == p, "inPhase check failed.");
        _;
    }

    event PhaseChange(Phase newPhase);
    uint[5] public phaseStartTimestamps;

    function changePhase(Phase p)
    internal {
        phase = p;
        phaseStartTimestamps[uint(p)] = block.timestamp;
        emit PhaseChange(p);
    }


    address payable public initiator;
    address payable public responder;

    modifier onlyInitiator() {
        require(msg.sender == initiator, "msg.sender is not Initiator.");
        _;
    }
    modifier onlyResponder() {
        require(msg.sender == responder, "msg.sender is not Responder.");
        _;
    }


    uint public sellAmount;
    uint public price;
    uint public responderDeposit;

    uint public autorecallInterval;
    uint public depositDeadlineInterval;
    uint public autoreleaseInterval;
    string public logisticsString;

    ERC20Interface tokenContract;

    constructor(ERC20Interface _tokenContract)
    public {
        changePhase(Phase.Created);

        tokenContract = _tokenContract;
    }

    function open(address payable _initiator, uint _price, uint _responderDeposit, uint _autorecallInterval, uint _depositDeadlineInterval, uint _autoreleaseInterval, string memory _logisticsString)
    public {
        changePhase(Phase.Open);

        initiator = _initiator;
        sellAmount = getBalance();
        price = _price;
        responderDeposit = _responderDeposit;
        autorecallInterval = _autorecallInterval;
        depositDeadlineInterval = _depositDeadlineInterval;
        autoreleaseInterval = _autoreleaseInterval;

        logisticsString = _logisticsString;
    }

    /* ---------------------- OPEN PHASE --------------------------

    In the Open phase, the Initiator waits for a Responder.
    We move to the Commited phase once someone becomes the Responder
    by executing commit() and including msg.value = responderDeposit.

    At any time, the Initiator can cancel the whole thing by calling recall().

    After autorecallInterval has passed, the only state change allowed is to recall(),
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Recalled();
    event Committed(address responder);

    function recall()
    external
    inPhase(Phase.Open)
    onlyInitiator() {
       internalRecall();
    }

    function internalRecall()
    internal {
        assert(tokenContract.transfer(initiator, getBalance()));

        emit Recalled();

        changePhase(Phase.Closed);
    }

    function autorecallAvailable()
    public
    view
    inPhase(Phase.Open)
    returns(bool available) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Open)] + autorecallInterval);
    }

    function commit(string calldata note)
    external
    inPhase(Phase.Open) {
        require(tokenContract.transferFrom(msg.sender, address(this), responderDeposit), "Can't transfer the required deposit from the token contract. Did you call approve first?");
        require(!autorecallAvailable(), "autorecallInterval has passed; this offer has expired.");

        responder = msg.sender;

        emit Committed(responder);

        changePhase(Phase.Committed);

        if (bytes(note).length > 0) {
          emit ResponderStatementLog(note);
        }
    }

    /* ---------------------- COMMITTED PHASE ---------------------

    In the Committed phase, the Responder is expected to deposit fiat for the token,
    then call claim().

    If the Responder hasn't called claim() befre depositDeadline passes,
    the only state change allowed is to abort via poke(),
    which burns the Responder's deposit and returns the Initiator's tokens.

    ------------------------------------------------------------ */

    event AbortedFromDepositDeadlinePassed();

    function depositDeadlinePassed()
    public
    view
    inPhase(Phase.Committed)
    returns(bool passed) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Committed)] + depositDeadlineInterval);
    }

    function internalAbort()
    internal {
        //burn Responder's deposit
        require(tokenContract.transfer(address(0x0), responderDeposit), "Token burn failed!");

        //return Initiator's tokens
        require(tokenContract.transfer(initiator, getBalance()), "Token transfer to Initiator failed!");

        emit AbortedFromDepositDeadlinePassed();

        changePhase(Phase.Closed);
    }

    function claim(string calldata note)
    external
    inPhase(Phase.Committed)
    onlyResponder() {
        require(!depositDeadlinePassed(), "The deposit deadline has passed!");

        changePhase(Phase.Claimed);

        if (bytes(note).length > 0) {
            emit ResponderStatementLog(note);
        }
    }

    /* ---------------------- CLAIMED PHASE -----------------------

    In the Claimed phase, the Initiator can call release() or burn(),
    and is expected to call burn() only if the Responder did not deliver
    the money or work originally requested.

    After autoreleaseInterval has passed, the only state change allowed is to release,
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Released();
    event Burned();

    function autoreleaseAvailable()
    public
    view
    inPhase(Phase.Claimed)
    returns(bool available) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Claimed)] + autoreleaseInterval);
    }

    function release()
    external
    inPhase(Phase.Claimed)
    onlyInitiator() {
        internalRelease();
    }

    function internalRelease()
    internal {
        assert(tokenContract.transfer(responder, getBalance()));

        emit Released();

        changePhase(Phase.Closed);
    }

    function burn(string calldata note)
    external
    inPhase(Phase.Claimed)
    onlyInitiator() {
        require(!autoreleaseAvailable());

        internalBurn();

        if (bytes(note).length > 0) {
            emit InitiatorStatementLog(note);
        }
    }

    function internalBurn()
    internal {
        assert(tokenContract.transfer(address(0x0), getBalance()));

        emit Burned();

        changePhase(Phase.Closed);
    }

    /* ---------------------- OTHER METHODS ----------------------- */

    function getState()
    external
    view
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder) {
        return (getBalance(), this.phase(), phaseStartTimestamps[uint(phase)], this.responder());
    }

    function getBalance()
    public
    view
    returns(uint) {
        return tokenContract.balanceOf(address(this));
    }

    function getParameters()
    external
    view
    returns (address initiator, uint sellAmount, uint price, uint responderDeposit, uint autorecallInterval, uint depositDeadlineInterval, uint autoreleaseInterval, string memory logisticsString)
    {
        return (this.initiator(), this.sellAmount(), this.price(), this.responderDeposit(), this.autorecallInterval(), this.depositDeadlineInterval(), this.autoreleaseInterval(), this.logisticsString());
    }

    // Poke function lets anyone move the contract along,
    // if it's due for some state transition.

    function poke()
    external {
        if (phase == Phase.Open) {
            if (autorecallAvailable()) {
                internalRecall();
            }
        }
        else if (phase == Phase.Committed) {
            if (depositDeadlinePassed()) {
                internalAbort();
            }
        }
        else if (phase == Phase.Claimed) {
            if (autoreleaseAvailable()) {
                internalRelease();
            }
        }
    }

    // StatementLogs allow a starting point for any necessary communication,
    // and can be used in any phase, including the Closed phase.

    event InitiatorStatementLog(string statement);
    event ResponderStatementLog(string statement);

    function initiatorStatement(string memory statement)
    public
    onlyInitiator() {
        emit InitiatorStatementLog(statement);
    }

    function responderStatement(string memory statement)
    public
    onlyResponder() {
        emit ResponderStatementLog(statement);
    }
}
