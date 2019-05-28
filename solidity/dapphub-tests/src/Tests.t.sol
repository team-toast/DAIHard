pragma solidity ^0.5.6;

import "ds-test/test.sol";
import "ds-token/token.sol";

import "src/../../DAIHard.sol";

contract Hevm {
    function warp(uint) public;
}

contract DAIHardTests is DSTest {
    DSToken token;
    DAIHardFactory factory;
    Hevm hevm;

    User alice;
    User bob;
    User carl;

    uint defaultTradeAmount;
    uint defaultBeneficiaryDeposit;
    uint defaultAbortPunishment;
    uint defaultPokeReward;
    uint expectedFounderFee;
    uint defaultDevFee;
    uint defaultAutorecallInterval;
    uint defaultAutoabortInterval;
    uint defaultAutoreleaseInterval;
    address devFeeAddress;
    address founderFeeAddress;

    function setUp() public {
        defaultTradeAmount = 1000;
        defaultBeneficiaryDeposit = 100;
        defaultAbortPunishment = 18;
        defaultPokeReward = 5;
        defaultDevFee = 3;
        defaultAutorecallInterval = 20;
        defaultAutoabortInterval = 30;
        defaultAutoreleaseInterval = 40;
        devFeeAddress = address(0x1);
        founderFeeAddress = address(0x2);
        expectedFounderFee = defaultTradeAmount / 200;

        hevm = Hevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
        hevm.warp(0);

        token = new DSToken("AAA");
        token.mint(10000000000000000000000); // one milllllion DAI   http://gph.is/1OOxk4J

        factory = new DAIHardFactory(ERC20Interface(address(token)), founderFeeAddress);

        alice = new User(token, factory);
        token.push(address(alice), 1000000000000000000000);

        bob = new User(token, factory);
        token.push(address(bob), 1000000000000000000000);

        carl = new User(token, factory);
        token.push(address(carl), 1000000000000000000000);
    }

    function test_whenTradeIsCreated_assertExpectedStartingState() public {
        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Manually create trade ---- */
        DAIHardTrade t = new DAIHardTrade(ERC20Interface(address(token)), founderFeeAddress, devFeeAddress);

        /* ---- Assert correct address vars ---- */
        assertEq(address(t.daiContract()), address(token));
        assertEq(address(t.founderFeeAddress()), founderFeeAddress);
        assertEq(address(t.devFeeAddress()), devFeeAddress);
    }

    function test_whenOpenTradeCreatedByCustodian_assertExpectedInitialStateAndTokenDeposit() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));

        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Alice creates an open trade as custodian ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Assert ---- */

        // Assert all initial settings are as expected
        assertEq(trade.abortPunishment(), defaultAbortPunishment);
        assertEq(trade.pokeReward(), defaultPokeReward);
        assertEq(trade.founderFee(), expectedFounderFee);
        assertEq(trade.devFee(), defaultDevFee);
        assertEq(trade.autorecallInterval(), defaultAutorecallInterval);
        assertEq(trade.autoabortInterval(), defaultAutoabortInterval);
        assertEq(trade.autoreleaseInterval(), defaultAutoreleaseInterval);
        assertEq(trade.initiator(), address(alice));
        assertEq(trade.devFeeAddress(), devFeeAddress);
        assertTrue(trade.initiatorIsCustodian());
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Open));
        assertEq(trade.custodian(), address(alice));
        assertEq(trade.beneficiaryDeposit(), defaultBeneficiaryDeposit);

        // Assert the tradeAmount calculation resulted in the expected value
        assertEq(trade.tradeAmount(), trade.getBalance() - (expectedFounderFee + defaultPokeReward + defaultDevFee));

        // Calculate total deposit, and assert it is present in the trade and deducted from Alice.
        uint totalDeposit = defaultTradeAmount + defaultPokeReward + defaultDevFee + expectedFounderFee;
        assertEq(trade.getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance - totalDeposit);
    }

    function test_whenOpenTradeCreatedByBeneficiary_assertExpectedInitialStateAndTokenDeposit() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));

        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Alice creates an open trade as beneficiary ---- */

        alice.startOpenTradeAsBeneficiary([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Assert ---- */

        // Assert all initial settings are as expected
        assertEq(trade.abortPunishment(), defaultAbortPunishment);
        assertEq(trade.pokeReward(), defaultPokeReward);
        assertEq(trade.founderFee(), expectedFounderFee);
        assertEq(trade.devFee(), defaultDevFee);
        assertEq(trade.autorecallInterval(), defaultAutorecallInterval);
        assertEq(trade.autoabortInterval(), defaultAutoabortInterval);
        assertEq(trade.autoreleaseInterval(), defaultAutoreleaseInterval);
        assertEq(trade.initiator(), address(alice));
        assertEq(trade.devFeeAddress(), devFeeAddress);
        assertTrue(! trade.initiatorIsCustodian());
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Open));
        assertEq(trade.beneficiary(), address(alice));
        assertEq(trade.tradeAmount(), defaultTradeAmount);

        // Assert the beneficiaryDeposit calculation resulted in the expected value
        assertEq(trade.beneficiaryDeposit(), trade.getBalance() - (expectedFounderFee + defaultPokeReward + defaultDevFee));

        // Calculate total deposit, and assert it is present in the trade and deducted from Alice.
        uint totalDeposit = defaultBeneficiaryDeposit + defaultPokeReward + defaultDevFee + expectedFounderFee;
        assertEq(trade.getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance - totalDeposit);
    }

    function test_whenCommittedTradeCreatedByCustodian_assertExpectedInitialStateAndTokenDeposit() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));

        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Alice creates a committed trade, with her as custodian and bob as beneficiary ---- */

        alice.startCommittedTradeAsCustodian(address(bob), [defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Assert ---- */

        // Assert all initial settings are as expected
        assertEq(trade.beneficiaryDeposit(), defaultBeneficiaryDeposit);
        assertEq(trade.abortPunishment(), defaultAbortPunishment);
        assertEq(trade.pokeReward(), defaultPokeReward);
        assertEq(trade.autoabortInterval(), defaultAutoabortInterval);
        assertEq(trade.autoreleaseInterval(), defaultAutoreleaseInterval);
        assertEq(trade.founderFee(), expectedFounderFee);
        assertEq(trade.devFee(), defaultDevFee);
        assertTrue(trade.initiatorIsCustodian());
        assertEq(trade.custodian(), address(alice));
        assertEq(trade.beneficiary(), address(bob));
        assertEq(trade.devFeeAddress(), devFeeAddress);
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Committed));
        assertEq(trade.initiator(), address(alice));
        assertEq(trade.responder(), address(bob));

        // Assert the tradeAmount calculation resulted in the expected value
        assertEq(trade.tradeAmount(), trade.getBalance() - (expectedFounderFee + defaultDevFee + defaultPokeReward + defaultBeneficiaryDeposit));

        // Calculate total deposit, and assert it is present in the trade and deducted from Alice.
        uint totalDeposit = defaultTradeAmount + defaultBeneficiaryDeposit + defaultPokeReward + defaultDevFee + expectedFounderFee;
        assertEq(trade.getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance - totalDeposit);
    }

    function test_whenCommittedTradeCreatedByBeneficiary_assertExpectedInitialStateAndTokenDeposit() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));

        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Alice creates a committed trade, with her as beneficiary and bob as custodian ---- */

        alice.startCommittedTradeAsBeneficiary(address(bob), [defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Assert ---- */

        // Assert all initial settings are as expected
        assertEq(trade.beneficiaryDeposit(), defaultBeneficiaryDeposit);
        assertEq(trade.abortPunishment(), defaultAbortPunishment);
        assertEq(trade.pokeReward(), defaultPokeReward);
        assertEq(trade.autoabortInterval(), defaultAutoabortInterval);
        assertEq(trade.autoreleaseInterval(), defaultAutoreleaseInterval);
        assertEq(trade.founderFee(), expectedFounderFee);
        assertEq(trade.devFee(), defaultDevFee);
        assertTrue(!trade.initiatorIsCustodian());
        assertEq(trade.custodian(), address(bob));
        assertEq(trade.beneficiary(), address(alice));
        assertEq(trade.devFeeAddress(), devFeeAddress);
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Committed));
        assertEq(trade.initiator(), address(alice));
        assertEq(trade.responder(), address(bob));

        // Assert the tradeAmount calculation resulted in the expected value
        assertEq(trade.tradeAmount(), trade.getBalance() - (expectedFounderFee + defaultDevFee + defaultPokeReward + defaultBeneficiaryDeposit));

        // Calculate total deposit, and assert it is present in the trade and deducted from Alice.
        uint totalDeposit = defaultTradeAmount + defaultBeneficiaryDeposit + defaultPokeReward + defaultDevFee + expectedFounderFee;
        assertEq(trade.getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance - totalDeposit);
    }

    function test_givenOpenTrade_whenInitiatorCallsRecall_AssertTradeClosesAndRefundsAsExpected() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));

        /* ---- Set up: Alice is the Initiator of an Open trade ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Action: Alice recalls ---- */

        alice.do_recall();

        /* ---- Assert ---- */

        // Assert trade state: closed (recalled)
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Recalled));

        // Assert trade has no remaining balance, and Alice has been fully refunded
        assertEq(trade.getBalance(), uint(0));
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance);
    }

    function test_givenOpenTradeNeedingCustodian_whenBobCallsCommit_assertExpectedTokenTransferAndCorrectlyCommittedTrade() public {
        /* ---- Set up: Alice opens a trade as Beneficiary ---- */

        alice.startOpenTradeAsBeneficiary([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        uint bobStartingBalance = token.balanceOf(address(bob));
        uint tradePreCommitBalance = trade.getBalance();

        /* ---- Action: Bob commits ---- */

        bob.do_commit(trade);

        /* ---- Assert ---- */

        // Calculate expected deposit; assert this amount is deducted from Bob and added to the trade
        uint totalDeposit = defaultTradeAmount;
        assertEq(token.balanceOf(address(bob)), bobStartingBalance - totalDeposit);
        assertEq(trade.getBalance(), tradePreCommitBalance + totalDeposit);

        // Assert bob is the responder and custodian
        assertEq(trade.responder(), address(bob));
        assertEq(trade.custodian(), address(bob));

        // Assert Committed phase
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Committed));
    }

    function test_givenOpenTradeNeedingBeneficiary_whenBobCommitsBeforeAutorecall_assertExpectedTokenTransferAndCorrectlyCommittedTrade() public {
        /* ---- Set up: Alice opens a trade as Custodian ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        uint bobStartingBalance = token.balanceOf(address(bob));
        uint tradePreCommitBalance = trade.getBalance();

        /* ---- Action: Bob commits ---- */

        bob.do_commit(trade);

        /* ---- Assert ---- */

        // Calculate expected deposit; assert this amount is deducted from Bob and added to the trade
        uint totalDeposit = defaultBeneficiaryDeposit;
        assertEq(token.balanceOf(address(bob)), bobStartingBalance - totalDeposit);
        assertEq(trade.getBalance(), tradePreCommitBalance + totalDeposit);

        // Assert bob is the responder and beneficiary
        assertEq(trade.responder(), address(bob));
        assertEq(trade.beneficiary(), address(bob));

        // Assert Committed phase
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Committed));
    }

    function test_givenCommittedTrade_whenBeneficiaryCallsAbort_assertExpectedBurnsAndReturnsOfTokensAndProperlyClosedTrade() public {
        uint aliceStartingBalance = token.balanceOf(address(alice));
        uint bobStartingBalance = token.balanceOf(address(bob));
        uint burnAddressStartingBalance = token.balanceOf(address(0x0));

        /* ---- Set up: Alice opens a trade as custodian and Bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action: Bob aborts ---- */

        bob.do_abort();

        /* ---- Assert ---- */

        // Assert Alice and Bob have each been refunded all but the abortPunishment
        assertEq(token.balanceOf(address(alice)), aliceStartingBalance - defaultAbortPunishment);
        assertEq(token.balanceOf(address(bob)), bobStartingBalance - defaultAbortPunishment);

        // Assert the expected amount has been burned, and trade has no balance
        assertEq(token.balanceOf(address(0x0)), burnAddressStartingBalance + (defaultAbortPunishment * 2));
        assertEq(trade.getBalance(), 0);

        // Assert trade is properly closed
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Aborted));
    }

    function test_givenCommittedTrade_whenBeneficiaryCallsClaimBeforeAutoabort_assertClaimedPhase() public {
        /* ---- Set up: Alice opens trade as custodian, then bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action: bob calls claim ---- */
        bob.do_claim();

        /* ---- Assert Claimed phase ---- */
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Claimed));
    }

    function test_givenClaimedTrade_whenCustodianCallsRelease_assertExpectedTokenTransfersAndProperlyClosedState() public {
        uint bobStartingBalance = token.balanceOf(address(bob));

        /* ---- Set up: Alice opens trade as custodian, then bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        uint aliceBalanceAfterOpen = token.balanceOf(address(alice));

        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action: alice calls release ---- */

        alice.do_release();

        /* Assert */

        // Assert Bob ends up with tradeAmount more than he began with,
        // and Alice gets her pokeReward back (as she is the Initiator)
        assertEq(token.balanceOf(address(bob)), bobStartingBalance + defaultTradeAmount);
        assertEq(token.balanceOf(address(alice)), aliceBalanceAfterOpen + defaultPokeReward);

        // Assert founderFeeAddress and devFeeAddress get the fees
        assertEq(token.balanceOf(founderFeeAddress), expectedFounderFee);
        assertEq(token.balanceOf(devFeeAddress), defaultDevFee);

        // Assert trade state is properly closed
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Released));
    }

    function test_givenClaimedTrade_whenCustodianCallsBurn_assertAllTokensBurnedAndTradeProperlyClosed() public {
        /* ---- Set up: Alice opens trade, then bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        uint aliceBalanceAfterOpen = token.balanceOf(address(alice));
        bob.do_commit(trade);
        uint bobBalanceAfterCommit = token.balanceOf(address(bob));
        uint tradeBalanceAfterCommit = trade.getBalance();
        bob.do_claim();

        /* ---- Action: Alice calls burn ---- */

        alice.do_burn();

        /* ---- Assert ---- */

        // Assert the entire balance has been burned, and that the trade's balance is 0.
        assertEq(token.balanceOf(address(0x0)), tradeBalanceAfterCommit);
        assertEq(trade.getBalance(), 0);

        // Assert bob and alice both have no more than they did before the burn.
        assertEq(token.balanceOf(address(alice)), aliceBalanceAfterOpen);
        assertEq(token.balanceOf(address(bob)), bobBalanceAfterCommit);

        // Assert the trade state is properly closed
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Burned));
    }

    function test_givenOpenTrade_assert() public {
        /* ---- Set up:  */
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        assertTrue(!trade.autorecallAvailable());
        hevm.warp(defaultAutorecallInterval - 1);
        assertTrue(!trade.autorecallAvailable());
        hevm.warp(defaultAutorecallInterval);
        assertTrue(trade.autorecallAvailable());
    }

    function test_autoabortAvailable() public {
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        assertTrue(!trade.autoabortAvailable());
        hevm.warp(defaultAutoabortInterval - 1);
        assertTrue(!trade.autoabortAvailable());
        hevm.warp(defaultAutoabortInterval);
        assertTrue(trade.autoabortAvailable());
    }

    function test_autoreleaseAvailable() public {
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        assertTrue(!trade.autoreleaseAvailable());
        hevm.warp(defaultAutoreleaseInterval - 1);
        assertTrue(!trade.autoreleaseAvailable());
        hevm.warp(defaultAutoreleaseInterval);
        assertTrue(trade.autoreleaseAvailable());
    }

    function test_poke_autorecall() public {
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        carl.setTrade(trade);
        assertTrue(!carl.do_poke());
        hevm.warp(defaultAutorecallInterval);
        assertTrue(carl.do_poke());
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));


    }
    function test_poke_autoabort() public {
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        carl.setTrade(trade);
        assertTrue(!bob.do_poke());
        hevm.warp(defaultAutoabortInterval);
        assertTrue(bob.do_poke());
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));


    }
    function test_poke_autorelease() public {
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        carl.setTrade(trade);
        assertTrue(!bob.do_poke());
        hevm.warp(defaultAutoreleaseInterval);
        assertTrue(bob.do_poke());
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
    }
}

contract User {
    DSToken public token;
    DAIHardFactory public factory;
    DAIHardTrade public trade;

    constructor(DSToken t, DAIHardFactory f)
    public {
        token = t;
        factory = f;
        token.approve(address(factory), 1000000);
    }

    modifier checkPhaseDoesntRegress() {
        DAIHardTrade.Phase p1 = trade.phase();
        _;
        DAIHardTrade.Phase p2 = trade.phase();
        assert(uint(p2) >= uint(p1));
    }

    modifier checkPokeRewardGrantedIsFalse() {
        assert(!trade.pokeRewardGranted());
        _;
    }

    function setTrade(DAIHardTrade t)
    public {
        trade = t;
    }

    function startOpenTradeAsCustodian(uint[8] memory uintArgs, address devFeeAddress)
    public {
        token.approve(address(factory), 1000000);
        address[2] memory addressArgs = [address(this), devFeeAddress];
        trade = factory.createOpenTrade(addressArgs, true, uintArgs, "", "");
    }

    function startOpenTradeAsBeneficiary(uint[8] memory uintArgs, address devFeeAddress)
    public {
        token.approve(address(factory), 1000000);
        address[2] memory addressArgs = [address(this), devFeeAddress];
        trade = factory.createOpenTrade(addressArgs, false, uintArgs, "", "");
    }

    function startCommittedTradeAsCustodian(address beneficiary, uint[7] memory uintArgs, address devFeeAddress)
    public {
        token.approve(address(factory), 1000000);
        address[3] memory addressArgs = [address(this), beneficiary, devFeeAddress];
        trade = factory.createCommittedTrade(addressArgs, true, uintArgs, "", "", "");
    }

    function startCommittedTradeAsBeneficiary(address custodian, uint[7] memory uintArgs, address devFeeAddress)
    public {
        token.approve(address(factory), 1000000);
        address[3] memory addressArgs = [custodian, address(this), devFeeAddress];
        trade = factory.createCommittedTrade(addressArgs, false, uintArgs, "", "", "");
    }

    function do_recall()
    checkPhaseDoesntRegress()
    public {
        trade.recall();
    }
    function do_autorecallAvailable()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns (bool) {
        return trade.autorecallAvailable();
    }
    function do_commit(DAIHardTrade t)
    public {
        trade = t;
        token.approve(address(trade), 1000000);
        trade.commit(address(this), "");
    }
    function do_abort()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public {
        trade.abort();
    }
    function do_autoabortAvailable()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns (bool) {
        return trade.autoabortAvailable();
    }
    function do_claim()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public {
        trade.claim();
    }
    function do_autoreleaseAvailable()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns (bool) {
        return trade.autoreleaseAvailable();
    }
    function do_release()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public {
        trade.release();
    }
    function do_burn()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public {
        trade.burn();
    }
    function do_getState()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns(uint balance, DAIHardTrade.Phase phase, uint phaseStartTimestamp, address responder, DAIHardTrade.ClosedReason closedReason) {
        return trade.getState();
    }
    function do_getBalance()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns(uint) {
        return trade.getBalance();
    }
    function do_getParameters()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns (address initiator,
             bool initiatorIsCustodian,
             uint tradeAmount,
             uint beneficiaryDeposit,
             uint abortPunishment,
             uint autorecallInterval,
             uint autoabortInterval,
             uint autoreleaseInterval,
             uint pokeReward
             ) {
        return trade.getParameters();
    }
    function do_pokeNeeded()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns (bool) {
        return trade.pokeNeeded();
    }
    function do_poke()
    checkPhaseDoesntRegress()
    checkPokeRewardGrantedIsFalse()
    public
    returns(bool) {
        DAIHardTrade.Phase p1 = trade.phase();
        bool pokeNeeded = trade.poke();
        DAIHardTrade.Phase p2 = trade.phase();

        assert(  (pokeNeeded && p1 != DAIHardTrade.Phase.Closed && p2 == DAIHardTrade.Phase.Closed)
              || ((!pokeNeeded) && p1 == p2)
              );
        return pokeNeeded;
    }
}