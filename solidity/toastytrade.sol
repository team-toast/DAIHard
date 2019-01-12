pragma solidity 0.5.2;

import "ERC20Interface.sol";

contract ToastytradeFactory {
  event NewToastytradeSell(address toastytradeSellAddress);

  ERC20Interface public tokenContract;

  constructor(ERC20Interface _tokenContract)
  public {
    tokenContract = _tokenContract;
  }

  function createToastytradeSell(address payable _seller, uint value, uint _buyerDeposit, uint _autorecallInterval, uint _depositDeadlineInterval, uint _autoreleaseInterval, string calldata _logisticsString)
  external
  returns (ToastytradeSell) {
    ToastytradeSell newTT = new ToastytradeSell(tokenContract, _seller, _buyerDeposit, _autorecallInterval, _depositDeadlineInterval, _autoreleaseInterval, _logisticsString);

    require(tokenContract.transferFrom(msg.sender, address(newTT), value), "Token transfer failed. Did you call approve()?");

    emit NewToastytradeSell(address(newTT));

    return newTT;
  }
}

contract ToastytradeSell {
  enum Phase {Open, Committed, Claimed, Closed}
  Phase public phase;

  modifier inPhase(Phase p) {
    require(phase == p, "inPhase check failed.");
    _;
  }

  event PhaseChange(Phase newPhase);
  uint[4] public phaseStartTimestamps;

  function changePhase(Phase p)
  internal {
    phase = p;
    phaseStartTimestamps[uint(p)] = block.timestamp;
    emit PhaseChange(p);
  }


  address payable public seller;
  address payable public buyer;

  modifier onlySeller() {
    require(msg.sender == seller, "msg.sender is not Seller.");
    _;
  }
  modifier onlyBuyer() {
    require(msg.sender == buyer, "msg.sender is not Buyer.");
    _;
  }


  uint public buyerDeposit;

  uint public autorecallInterval;
  uint public depositDeadlineInterval;
  uint public autoreleaseInterval;
  string public logisticsString;

  ERC20Interface tokenContract;

  constructor(ERC20Interface _tokenContract, address payable _Seller, uint _buyerDeposit, uint _autorecallInterval, uint _depositDeadlineInterval, uint _autoreleaseInterval, string memory _logisticsString)
  public {
    changePhase(Phase.Open);

    seller = _Seller;
    buyerDeposit = _buyerDeposit;
    autorecallInterval = _autorecallInterval;
    depositDeadlineInterval = _depositDeadlineInterval;
    autoreleaseInterval = _autoreleaseInterval;

    tokenContract = _tokenContract;

    logisticsString = _logisticsString;
  }

  /* ---------------------- OPEN PHASE --------------------------

  In the Open phase, the Seller waits for a Buyer.
  We move to the Commited phase once someone becomes the Buyer
  by executing commit() and including msg.value = buyerDeposit.

  At any time, the Seller can cancel the whole thing by calling recall().

  After autorecallInterval has passed, the only state change allowed is to recall(),
  which can be triggered by anyone via poke().

  ------------------------------------------------------------ */

  event Recalled();
  event Committed(address buyer);

  function recall()
  external
  inPhase(Phase.Open)
  onlySeller() {
    internalRecall();
  }

  function internalRecall()
  internal {
    assert(tokenContract.transfer(seller, getBalance()));

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
    require(tokenContract.transferFrom(msg.sender, address(this), buyerDeposit), "Can't transfer the required deposit from the token contract. Did you call approve first?");
    require(!autorecallAvailable(), "autorecallInterval has passed; this offer has expired.");

    buyer = msg.sender;

    emit Committed(buyer);

    changePhase(Phase.Committed);

    if (bytes(note).length > 0) {
      emit BuyerStatementLog(note);
    }
  }

  /* ---------------------- COMMITTED PHASE ---------------------

  In the Committed phase, the Buyer is expected to deposit fiat for the token,
  then call claim().

  If the Buyer hasn't called claim() befre depositDeadline passes,
  the only state change allowed is to abort via poke(),
  which burns the Buyer's deposit and returns the Seller's tokens.

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
    //burn Buyer's deposit
    require(tokenContract.transfer(address(0x0), buyerDeposit), "Token burn failed!");

    //return Seller's tokens
    require(tokenContract.transfer(seller, getBalance()), "Token transfer to Seller failed!");

    emit AbortedFromDepositDeadlinePassed();

    changePhase(Phase.Closed);
  }

  function claim(string calldata note)
  external
  inPhase(Phase.Committed)
  onlyBuyer() {
    require(!depositDeadlinePassed(), "The deposit deadline has passed!");

    changePhase(Phase.Claimed);

    if (bytes(note).length > 0) {
      emit BuyerStatementLog(note);
    }
  }

  /* ---------------------- CLAIMED PHASE -----------------------

  In the Claimed phase, the Seller can call release() or burn(),
  and is expected to call burn() only if the Buyer did not deliver
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
  onlySeller() {
    internalRelease();
  }

  function internalRelease()
  internal {
    assert(tokenContract.transfer(buyer, getBalance()));

    emit Released();

    changePhase(Phase.Closed);
  }

  function burn(string calldata note)
  external
  inPhase(Phase.Claimed)
  onlySeller() {
    require(!autoreleaseAvailable());

    internalBurn();

    if (bytes(note).length > 0) {
      emit SellerStatementLog(note);
    }
  }

  function internalBurn()
  internal {
    assert(tokenContract.transfer(address(0x0), getBalance()));

    emit Burned();

    changePhase(Phase.Closed);
  }

  /* ---------------------- OTHER METHODS ----------------------- */

  function getFullState()
  external
  view
  returns(uint balance, Phase phase, uint phaseStartTimestamp, address seller, address buyer, uint buyerDeposit, uint autorecallInterval, uint depositDeadlineInterval, uint autoreleaseInterval) {
    return (address(this).balance, this.phase(), phaseStartTimestamps[uint(phase)], this.seller(), this.buyer(), this.buyerDeposit(), this.autorecallInterval(), this.depositDeadlineInterval(), this.autoreleaseInterval());
  }

  function getBalance()
  public
  view
  returns(uint) {
    return tokenContract.balanceOf(address(this));
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

  event SellerStatementLog(string statement);
  event BuyerStatementLog(string statement);

  function sellerStatement(string memory statement)
  public
  onlySeller() {
    emit SellerStatementLog(statement);
  }

  function buyerStatement(string memory statement)
  public
  onlyBuyer() {
    emit BuyerStatementLog(statement);
  }
}
