pragma solidity 0.5.4;

import "ERC20Interface.sol";

contract ToastytradeFactory {
    event NewToastytrade(uint id, address toastytradeAddress, bool indexed initiatorIsPayer);

    ERC20Interface public tokenContract;
    address payable public devFeeAddress;

    constructor(ERC20Interface _tokenContract, address payable _devFeeAddress)
    public {
        tokenContract = _tokenContract;
        devFeeAddress = _devFeeAddress;
    }

    struct CreationInfo {
        address address_;
        uint blocknum;
    }

    CreationInfo[] public createdTrades;

    function getDevFee(uint tradeAmount)
    public
    pure
    returns (uint devFee) {
        return tradeAmount/100;
    }

    /*
    The Solidity compiler can't handle much stack depth,
    so we have to pack some args together in annoying ways...
    Hence the 'uintArgs'. Here is their layout:
    0 - tokenAmount
    1 - buyerDeposit
    2 - pokeReward
    3 - autorecallInterval
    4 - autoabortInterval
    5 - autoreleaseInterval
    */

    function openToastytrade(address payable _initiator, bool initiatorIsBuyer, uint[6] calldata uintArgs, string calldata _totalPrice, string calldata _fiatTransferMethods, string calldata _commPubkey)
    external
    returns (Toastytrade) {
        Toastytrade newTT = new Toastytrade(tokenContract, devFeeAddress);

        createdTrades.push(CreationInfo(address(newTT), block.number));

        uint transferAmount;
        uint[6] memory newUintArgs; // Note that this structure is not the same as the above comment describes. See below in Toastytrade.open.

        uint devFee = getDevFee(uintArgs[0]);

        if (initiatorIsBuyer) {
            transferAmount = uintArgs[1] + uintArgs[2] + devFee;
            newUintArgs = [uintArgs[0], uintArgs[2], devFee, uintArgs[3], uintArgs[4], uintArgs[5]];
        }
        else {
            transferAmount = uintArgs[0] + uintArgs[2] + devFee;
            newUintArgs = [uintArgs[1], uintArgs[2], devFee, uintArgs[3], uintArgs[4], uintArgs[5]];
        }

        require(tokenContract.transferFrom(msg.sender, address(newTT), transferAmount), "Token transfer failed. Did you call approve() on the token contract?");

        newTT.open(_initiator, initiatorIsBuyer, newUintArgs, _totalPrice, _fiatTransferMethods, _commPubkey);

        emit NewToastytrade(createdTrades.length - 1, address(newTT), initiatorIsBuyer);
    }

    function getNumToastytradeSells()
    external
    view
    returns (uint num) {
        return createdTrades.length;
    }
}

contract Toastytrade {
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

    //The contract only has two parties, but depending on how it's opened,
    //the initiator for example might be either the buyer OR the seller.

    bool public initiatorIsBuyer;
    address payable public buyer;
    address payable public seller;

    modifier onlyInitiator() {
        require(msg.sender == initiator, "msg.sender is not Initiator.");
        _;
    }
    modifier onlyResponder() {
        require(msg.sender == responder, "msg.sender is not Responder.");
        _;
    }
    modifier onlyBuyer() {
        require (msg.sender == buyer, "msg.sender is not Buyer.");
        _;
    }
    modifier onlySeller() {
        require (msg.sender == seller, "msg.sender is not Seller.");
        _;
    }
    modifier onlyContractParty() { // Must be one of the two parties involved in the contract
        require(msg.sender == initiator || msg.sender == responder, "msg.sender is not a party in this contract.");
        _;
    }

    ERC20Interface tokenContract;
    address payable devFeeAddress;

    constructor(ERC20Interface _tokenContract, address payable _devFeeAddress)
    public {
        changePhase(Phase.Created);

        tokenContract = _tokenContract;
        devFeeAddress = _devFeeAddress;

        pokeRewardSent = false;
    }

    uint public tokenAmount;
    string public totalPrice;
    uint public buyerDeposit;

    uint public responderDeposit; // This will be equal to either tokenAmount or buyerDeposit, depending on initiatorIsBuyer

    uint public autorecallInterval;
    uint public autoabortInterval;
    uint public autoreleaseInterval;

    string public fiatTransferMethods;

    string public initiatorCommPubkey;
    string public responderCommPubkey;

    uint public pokeReward;
    uint public devFee;

    bool public pokeRewardSent;

    /*
    uintArgs:
    0 - responderDeposit
    1 - pokeReward
    2 - devFee
    3 - autorecallInterval
    4 - autoabortInterval
    5 - autoreleaseInterval
    */

    function open(address payable _initiator, bool _initiatorIsBuyer, uint[6] memory uintArgs, string memory _totalPrice, string memory _fiatTransferMethods, string memory commPubkey)
    public {
        require(getBalance() > 0, "You can't open a toastytrade without first depositing tokens.");

        changePhase(Phase.Open);

        responderDeposit = uintArgs[0];
        pokeReward = uintArgs[1];
        devFee = uintArgs[2];

        autorecallInterval = uintArgs[3];
        autoabortInterval = uintArgs[4];
        autoreleaseInterval = uintArgs[5];

        initiator = _initiator;
        initiatorIsBuyer = _initiatorIsBuyer;
        if (initiatorIsBuyer) {
            buyer = initiator;
            tokenAmount = responderDeposit;
            buyerDeposit = getBalance() - (pokeReward + devFee);
        }
        else {
            seller = initiator;
            tokenAmount = getBalance() - (pokeReward + devFee);
            buyerDeposit = responderDeposit;
        }

        totalPrice = _totalPrice;

        fiatTransferMethods = _fiatTransferMethods;
        initiatorCommPubkey = commPubkey;
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
        require(tokenContract.transfer(initiator, getBalance()), "Recall of tokens to initiator failed!");

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

    function commit(string calldata commPubkey)
    external
    inPhase(Phase.Open) {
        require(tokenContract.transferFrom(msg.sender, address(this), responderDeposit), "Can't transfer the required deposit from the token contract. Did you call approve first?");
        require(!autorecallAvailable(), "autorecallInterval has passed; this offer has expired.");

        responder = msg.sender;

        if (initiatorIsBuyer) {
            seller = responder;
        }
        else {
            buyer = responder;
        }

        responderCommPubkey = commPubkey;

        emit Committed(responder);

        changePhase(Phase.Committed);
    }

    /* ---------------------- COMMITTED PHASE ---------------------

    In the Committed phase, the Buyer is expected to deposit fiat for the token,
    then call claim().

    Otherwise, the Buyer can call abort(), which cancels the contract,
    incurs a small penalty on both parties, and returns the remainder to each party.

    After autoabortInterval has passed, the only state change allowed is to abort(),
    which can be triggered by anyone via poke().

    ------------------------------------------------------------ */

    event Aborted();

    function abort()
    external
    inPhase(Phase.Committed)
    onlyBuyer() {
        internalAbort();
    }

    function internalAbort()
    internal {
        //Punishment amount is 1/2 the buyerDeposit for now,
        //but in a future version this might be set by the Initiator.
        uint burnAmount = buyerDeposit / 2;

        //Punish both parties equally by burning burnAmount.
        //Instead of burning burnAmount twice, just burn it all in one call.
        require(tokenContract.transfer(address(0x0), burnAmount*2), "Token burn failed!");

        //Send back deposits minus burned amounts.
        require(tokenContract.transfer(buyer, buyerDeposit - burnAmount), "Token transfer to Buyer failed!");
        require(tokenContract.transfer(seller, tokenAmount - burnAmount), "Token transfer to Seller failed!");

        uint sendBackToInitiator = devFee;
        //If there was a pokeReward left, it should be sent back to the initiator
        if (!pokeRewardSent) {
            sendBackToInitiator += pokeReward;
        }
        
        require(tokenContract.transfer(initiator, sendBackToInitiator), "Token refund of devFee+pokeReward to Initiator failed!");
        
        //There may be a wei or two left over in the contract due to integer division. Not a big deal.

        emit Aborted();

        changePhase(Phase.Closed);
    }

    function autoabortAvailable()
    public
    view
    inPhase(Phase.Committed)
    returns(bool passed) {
        return (block.timestamp >= phaseStartTimestamps[uint(Phase.Committed)] + autoabortInterval);
    }

    function claim()
    external
    inPhase(Phase.Committed)
    onlyBuyer() {
        require(!autoabortAvailable(), "The deposit deadline has passed!");

        changePhase(Phase.Claimed);
    }

    /* ---------------------- CLAIMED PHASE -----------------------

    In the Claimed phase, the Seller can call release() or burn(),
    and is expected to call burn() only if the Buyer did not transfer
    the amount of money described in totalPrice.

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
    onlySeller() {
        internalRelease();
    }

    function internalRelease()
    internal {
        //If the pokeReward has not been sent, refund it to the initiator
        if (!pokeRewardSent) {
            require(tokenContract.transfer(initiator, pokeReward), "Refund of pokeReward to Initiator failed!");
        }

        //Upon successful trade, the devFee is sent to the developers of Toastytrade.
        require(tokenContract.transfer(devFeeAddress, devFee), "Token transfer to devFeeAddress failed!");

        //Release the remaining balance to the buyer.
        require(tokenContract.transfer(buyer, getBalance()), "Final release transfer to buyer failed!");

        emit Released();

        changePhase(Phase.Closed);
    }

    function burn()
    external
    inPhase(Phase.Claimed)
    onlySeller() {
        require(!autoreleaseAvailable());

        internalBurn();
    }

    function internalBurn()
    internal {
        require(tokenContract.transfer(address(0x0), getBalance()), "Final token burn failed!");

        emit Burned();

        changePhase(Phase.Closed);
    }

    /* ---------------------- OTHER METHODS ----------------------- */

    function getState()
    external
    view
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, string memory responderCommPubkey) {
        return (getBalance(), this.phase(), phaseStartTimestamps[uint(phase)], this.responder(), this.responderCommPubkey());
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
    returns (address initiator, bool initiatorIsBuyer, uint tokenAmount, string memory totalPrice, uint buyerDeposit, uint autorecallInterval, uint autoabortInterval, uint autoreleaseInterval, string memory fiatTransferMethods, string memory initiatorCommPubkey, uint pokeReward)
    {
        return (this.initiator(), this.initiatorIsBuyer(), this.tokenAmount(), this.totalPrice(), this.buyerDeposit(), this.autorecallInterval(), this.autoabortInterval(), this.autoreleaseInterval(), this.fiatTransferMethods(), this.initiatorCommPubkey(), this.pokeReward());
    }

    // Poke function lets anyone move the contract along,
    // if it's due for some state transition.

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
            tokenContract.transfer(msg.sender, pokeReward);
            pokeRewardSent = true;
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
    // and can be used anytime by either party after a Responder commits (including in the Closed phase).


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