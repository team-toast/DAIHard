pragma solidity 0.5.6;

import "ds-test/test.sol";
import "ds-token/token.sol";

import "src/../../DAIHard.sol";

contract DAIHardTradeWithCheats is DAIHardTrade {
    constructor(ERC20Interface _daiContract, address _founderFeeAddress, address _devFeeAddress)
    DAIHardTrade(_daiContract, _founderFeeAddress, _devFeeAddress)
    public {}

    function setPokeRewardGranted(bool f)
    public {
        pokeRewardGranted = f;
    }
}

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

    function test_01010_whenTradeIsCreated_assertExpectedStartingState() public {
        /* ---- Set up: [no scenario to set up] ---- */

        /* ---- Action: Manually create trade ---- */
        DAIHardTrade t = new DAIHardTrade(ERC20Interface(address(token)), founderFeeAddress, devFeeAddress);

        /* ---- Assert correct address vars ---- */
        assertEq(address(t.daiContract()), address(token));
        assertEq(address(t.founderFeeAddress()), founderFeeAddress);
        assertEq(address(t.devFeeAddress()), devFeeAddress);
    }

    function test_02010_givenTradeInPostCreatingPhase_whenCallingBeginInOpenPhase_assertFail() public {
        bytes memory beginInOpenPhaseCallSig = abi.encodeWithSignature("beginInOpenPhase(address,bool,uint256[8],string,string)", address(alice), true, [defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, expectedFounderFee, defaultDevFee], "", "");

        /* ---- Set up: Manually create trade and move to Open phase ---- */

        DAIHardTrade trade = new DAIHardTrade(ERC20Interface(address(token)), founderFeeAddress, devFeeAddress);
        token.push(address(trade), 2000);
        (bool success, ) = address(trade).call(beginInOpenPhaseCallSig);
        assertTrue(success);

        /* ---- Action/Assert: call beginInOpenPhase; check for failure ---- */

        (success, ) = address(trade).call(beginInOpenPhaseCallSig);
        assertTrue(! success);
    }
    
    function test_03010_givenTradeInPostCreatingPhase_whenCallingBeginInCommittedPhase_assertFail() public {
        bytes memory beginInCommittedPhaseCallSig = abi.encodeWithSignature("beginInCommittedPhase(address,address,bool,uint256[7],string,string,string)", address(alice), address(bob), true, [defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutoabortInterval, defaultAutoreleaseInterval, expectedFounderFee, defaultDevFee], "", "", "");

        /* ---- Set up: Manually create trade and move to Open phase ---- */

        DAIHardTrade trade = new DAIHardTrade(ERC20Interface(address(token)), founderFeeAddress, devFeeAddress);
        token.push(address(trade), 2000);
        (bool success, ) = address(trade).call(beginInCommittedPhaseCallSig);
        assertTrue(success);

        /* ---- Action/Assert: call beginInCommittedPhase; check for failure ---- */

        (success, ) = address(trade).call(beginInCommittedPhaseCallSig);
        assertTrue(! success);
    }

    function test_02050_whenOpenTradeCreatedByCustodian_assertExpectedInitialStateAndTokenDeposit() public {
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
        assertTrue(trade.initiator() == trade.custodian());
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

    function test_02060_whenOpenTradeCreatedByBeneficiary_assertExpectedInitialStateAndTokenDeposit() public {
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
        assertTrue(trade.initiator() != trade.custodian());
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

    function test_03050_whenCommittedTradeCreatedByCustodian_assertExpectedInitialStateAndTokenDeposit() public {
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
        assertTrue(trade.initiator() == trade.custodian());
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

    function test_03060_whenCommittedTradeCreatedByBeneficiary_assertExpectedInitialStateAndTokenDeposit() public {
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
        assertTrue(trade.initiator() != trade.custodian());
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

    function test_04010_givenTradeWithPhaseAfterOpen_whenInitiatorCallsRecall_assertFail() public {
        bytes memory doRecallCallSig = abi.encodeWithSignature("do_recall()");

        /* ---- Set up: Alice opens two trades as custodian and Bob commits to one ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade_willRemainOpen = alice.trade();
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade_willBeCommitted = alice.trade();
        bob.do_commit(trade_willBeCommitted);

        /* ---- Action: Alice calls recall on both ---- */

        alice.setTrade(trade_willRemainOpen);
        (bool recallSuccessOnOpenTrade, ) = address(alice).call(doRecallCallSig);
        alice.setTrade(trade_willBeCommitted);
        (bool recallSuccessOnCommittedTrade, ) = address(alice).call(doRecallCallSig);

        /* ---- Assert ---- */
        assertTrue(recallSuccessOnOpenTrade);
        assertTrue(! recallSuccessOnCommittedTrade);
    }

    function test_04020_givenOpenTrade_whenNonInitiatorCallsRecall_assertFail() public {
        bytes memory doRecallCallSig = abi.encodeWithSignature("do_recall()");

        /* ---- Set up: Alice is the Initiator of an Open trade ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Action/assert: bob's recall fails, but Alice's succeeds ---- */

        (bool success, ) = address(bob).call(doRecallCallSig);
        assertTrue(! success);
        (success, ) = address(alice).call(doRecallCallSig);
        assertTrue(success);
    }

    function test_04040_givenOpenTrade_whenInitiatorCallsRecall_assertTradeClosesAndRefundsAsExpected() public {
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

    function test_05030_05040_givenOpenTrade_assertAutorecallAvailableReturnsExpectedValuesAccordingToTimeCalled() public {
        /* ---- Set up: Alice opens trade ---- */
        
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Action: [no action to perform] ---- */

        /* ---- Assert ---- */

        // Assert return false immediately and just before autorecallInterval is up
        assertTrue(!trade.autorecallAvailable());
        hevm.warp(defaultAutorecallInterval - 1);
        assertTrue(!trade.autorecallAvailable());

        // Assert return true after autorecallInterval
        hevm.warp(defaultAutorecallInterval);
        assertTrue(trade.autorecallAvailable());
    }

    function test_06010_givenTradeNotInOpenPhase_whenCallingCommit_assertFail() public {
        /* ---- Set up: Create an open trade and move it to committed phase ---- */

        alice.startOpenTradeAsBeneficiary([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bytes memory doCommitCallSig = abi.encodeWithSignature("do_commit(address)", address(trade));
        (bool success, ) = address(bob).call(doCommitCallSig);
        assertTrue(success);

        /* ---- Action/Assert: carl tries to commit now; this time it should fail */
        (success, ) = address(carl).call(doCommitCallSig);
        assertTrue(! success);
    }

    function test_06030_givenOpenTradeWithAutorecallAvailable_whenCallingCommit_assertFail() public {
        /* ---- Set up: create and open trade ---- */

        alice.startOpenTradeAsBeneficiary([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bytes memory doCommitCallSig = abi.encodeWithSignature("do_commit(address)", address(trade));

        /* ---- Action/Assert ---- */

        // After autorecallInterval, bob should be unable to commit
        hevm.warp(defaultAutorecallInterval);
        (bool success, ) = address(bob).call(doCommitCallSig);
        assertTrue(! success);

        // If we warp back in time, bob should be able to commit
        hevm.warp(0);
        (success, ) = address(bob).call(doCommitCallSig);
        assertTrue(success);
    }

    function test_06040_givenOpenTradeNeedingBeneficiary_whenBobCommitsBeforeAutorecallAvailable_assertExpectedTokenTransferAndCorrectlyCommittedTrade() public {
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

    function test_06050_givenOpenTradeNeedingCustodian_whenBobCommitsBeforeAutorecallAvailable_assertExpectedTokenTransferAndCorrectlyCommittedTrade() public {
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

    function test_07010_givenTradeNotInCommittedPhase_whenBeneficiaryCallsAbort_assertFail() public {
        bytes memory doAbortCallSig = abi.encodeWithSignature("do_abort()");

        /* Set up: Alice creates two trades, bob commits to both but only claims on one ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade_willRemainCommitted = alice.trade();
        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade_willBeClaimed = alice.trade();

        bob.do_commit(trade_willRemainCommitted);
        bob.do_commit(trade_willBeClaimed);

        bob.setTrade(trade_willBeClaimed);
        bob.do_claim();

        /* ---- Action/Assert ---- */

        // If Bob calls abort on the claimed trade, assert fail
        bob.setTrade(trade_willBeClaimed);
        (bool success, ) = address(bob).call(doAbortCallSig);
        assertTrue(! success);

        // If Bob calls abort on the committed trade, assert success
        bob.setTrade(trade_willRemainCommitted);
        (success, ) = address(bob).call(doAbortCallSig);
        assertTrue(success);
    }

    function test_07020_givenCommittedTrade_whenNonBeneficiaryCallsAbort_assertFail() public {
        bytes memory doAbortCallSig = abi.encodeWithSignature("do_abort()");

        /* Set up: Alice creates a trade, bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        bob.do_commit(trade);

        /* ---- Action/Assert ---- */

        // If Carl abort, assert fail
        carl.setTrade(trade);
        (bool success, ) = address(carl).call(doAbortCallSig);
        assertTrue(! success);

        // If Bob calls abort, assert success
        bob.setTrade(trade);
        (success, ) = address(bob).call(doAbortCallSig);
        assertTrue(success);
    }

    function test_07090_givenCommittedTrade_whenBeneficiaryCallsAbort_assertExpectedBurnsAndReturnsOfTokensAndProperlyClosedTrade() public {
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

    function test_08030_08040_givenCommittedTrade_assertAutoabortAvailableReturnsExpectedValuesAccordingToTimeCalled() public {
        /* ---- Set up: Alice opens a trade, then Bob commits */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action: [no action to perform] ---- */

        /* ---- Assert ---- */

        // Assert return false immediately and just before autoabortInterval is up
        assertTrue(!trade.autoabortAvailable());
        hevm.warp(defaultAutoabortInterval - 1);
        assertTrue(!trade.autoabortAvailable());

        // Assert return true after autoabortInterval
        hevm.warp(defaultAutoabortInterval);
        assertTrue(trade.autoabortAvailable());
    }

    function test_09010_givenTradeNotInCommittedState_whenBeneficiaryCallsClaimBeforeAutoabort_assertFail() public {
        bytes memory doClaimCallSig = abi.encodeWithSignature("do_claim()");

        /* ---- Set up: Alice opens trade as custodian, then bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        (bool success, ) = address(bob).call(doClaimCallSig);
        assertTrue(success);

        /* ---- Action/Assert: Bob can no longer call claim ---- */

        (success, ) = address(bob).call(doClaimCallSig);
        assertTrue(! success);
    }

    function test_09020_givenCommittedTrade_whenNonBenficiaryCallsClaim_assertFail() public {
        bytes memory doClaimCallSig = abi.encodeWithSignature("do_claim()");

        /* ---- Set up: Alice opens trade as custodian, then bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action/Assert ---- */

        // Carl's call to claim fails
        carl.setTrade(trade);
        (bool success, ) = address(carl).call(doClaimCallSig);
        assertTrue(! success);

        // Bob's call to claim succeeds
        (success, ) = address(bob).call(doClaimCallSig);
        assertTrue(success);
    }

    function test_09030_givenCommittedTrade_whenBenficiaryCallsClaimAfterAutoabortInterval_assertFail() public {
        bytes memory doClaimCallSig = abi.encodeWithSignature("do_claim()");

        /* ---- Set up: Alice opens trade as custodian, then bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action/Assert ---- */

        // Bob can't call claim after autoabortInterval
        hevm.warp(defaultAutoabortInterval);
        (bool success, ) = address(bob).call(doClaimCallSig);
        assertTrue(! success);

        // But if we warp back in time, he can
        hevm.warp(0);
        (success, ) = address(bob).call(doClaimCallSig);
        assertTrue(success);
    }

    function test_09040_givenCommittedTrade_whenBeneficiaryCallsClaimBeforeAutoabort_assertJudgmentPhase() public {
        /* ---- Set up: Alice opens trade as custodian, then bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action: bob calls claim ---- */
        bob.do_claim();

        /* ---- Assert Judgment phase ---- */
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Judgment));
    }

    function test_10010_givenTradeNotInJudgmentPhase_whenCustodianCallsRelease_assertFail() public {
        bytes memory doReleaseCallSig = abi.encodeWithSignature("do_release()");

        /* ---- Set up: Alice opens trade as custodian and Bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action/Assert ---- */

        // Assert Alice's call to release fails in the Committed phase
        (bool success, ) = address(alice).call(doReleaseCallSig);
        assertTrue(! success);

        // Assert Alice can release after Bob claims
        bob.do_claim();
        (success, ) = address(alice).call(doReleaseCallSig);
        assertTrue(success);
    }

    function test_10020_givenJudgmentPhaseTrade_whenNonCustodianCallsRelease_assertFail() public {
        bytes memory doReleaseCallSig = abi.encodeWithSignature("do_release()");

        /* ---- Set up: Alice opens trade as custodian, then Bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action/Assert ---- */

        // Assert bob cannot call release
        (bool success, ) = address(bob).call(doReleaseCallSig);
        assertTrue(! success);

        // Assert alice can still call release
        (success, ) = address(alice).call(doReleaseCallSig);
        assertTrue(success);
    }

    function test_10070_givenJudgmentPhaseTrade_whenCustodianCallsRelease_assertExpectedTokenTransfersAndProperlyClosedState() public {
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

    function test_11030_11040_givenJudgmentPhaseTrade_assertAutoreleaseAvailableReturnsExpectedValuesAccordingToTimeCalled() public {
        /* ---- Set up: Alice opens a trade, then Bob commits and claims */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action: [no action to perform] ---- */

        /* ---- Assert ---- */

        // Assert return false immediately and just before autoreleaseInterval is up
        assertTrue(!trade.autoreleaseAvailable());
        hevm.warp(defaultAutoreleaseInterval - 1);
        assertTrue(!trade.autoreleaseAvailable());

        // Assert return true after autoreleaseInterval
        hevm.warp(defaultAutoreleaseInterval);
        assertTrue(trade.autoreleaseAvailable());
    }

    function test_12010_givenTradeNotInJudgmentPhase_whenCustodianCallsBurn_assertFail() public {
        bytes memory doBurnCallSig = abi.encodeWithSignature("do_burn()");

        /* ---- Set up: Alice opens trade as custodian and Bob commits ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action/Assert ---- */

        // Assert Alice's call to burn fails in the Committed phase
        (bool success, ) = address(alice).call(doBurnCallSig);
        assertTrue(! success);

        // Assert Alice can burn after Bob claims
        bob.do_claim();
        (success, ) = address(alice).call(doBurnCallSig);
        assertTrue(success);
    }

    function test_12020_givenJudgmentPhaseTrade_whenNonCustodianCallsReleaseBurnFail() public {
        bytes memory doBurnCallSig = abi.encodeWithSignature("do_burn()");

        /* ---- Set up: Alice opens trade as custodian, then Bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action/Assert ---- */

        // Assert bob cannot call release
        (bool success, ) = address(bob).call(doBurnCallSig);
        assertTrue(! success);

        // Assert alice can still call release
        (success, ) = address(alice).call(doBurnCallSig);
        assertTrue(success);
    }

    function test_12030_givenJudgmentPhaseTrade_whenCustodianCallsBurnAfterAutoreleaseInterval_assertFail() public {
        bytes memory doBurnCallSig = abi.encodeWithSignature("do_burn()");

        /* ---- Set up: Alice opens trade as custodian, then bob commits and claims ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action/Assert ---- */

        // Alice can't call burn after autoreleaseInterval
        hevm.warp(defaultAutoreleaseInterval);
        (bool success, ) = address(alice).call(doBurnCallSig);
        assertTrue(! success);

        // But if we warp back in time, she can
        hevm.warp(0);
        (success, ) = address(alice).call(doBurnCallSig);
        assertTrue(success);
    }

    function test_12050_givenJudgmentPhaseTrade_whenCustodianCallsBurn_assertAllTokensBurnedAndTradeProperlyClosed() public {
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

    function test_13010_givenTradeWithPokeNeededButPokeRewardSentIsTrue_whenPokeCalled_assertFail() public {
        bytes memory pokeCallSig = abi.encodeWithSignature("poke()");

        /* ---- Set up: Open a trade, warp ahead so autorecallAvailable() is true, and cheat to set pokeRewardGranted to true ---- */

        DAIHardTradeWithCheats cTrade = new DAIHardTradeWithCheats(ERC20Interface(address(token)), founderFeeAddress, devFeeAddress);
        token.push(address(cTrade), 2000);
        cTrade.beginInOpenPhase(address(alice), true, [defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, expectedFounderFee, defaultDevFee], "", "");
        hevm.warp(defaultAutorecallInterval);
        cTrade.setPokeRewardGranted(true);

        /* ---- Action/Assert ---- */

        // Calling poke now fails
        (bool success, ) = address(cTrade).call(pokeCallSig);
        assertTrue(! success);

        // Set pokeRewardGranted to false, and the call should work
        cTrade.setPokeRewardGranted(false);
        (success, ) = address(cTrade).call(pokeCallSig);
        assertTrue(success);
    }

    function test_13020_givenTradeNotNeedingPoke_whenPokeCalled_assertReturnFalse() public {
        /* ---- Set up: Open a trade ---- */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Action/Assert: poke returns false ---- */

        assertTrue(! trade.poke());
    }

    function test_13040_givenOpenAutorecallableTrade_whenCarlCallsPoke_assertCarlReceivesPokeRewardAndTradeClosesProperly_iffAutorecallAvailable() public {
        uint carlStartingBalance = token.balanceOf(address(carl));

        /* ---- Set up: Alice opens a trade */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();

        /* ---- Action and Assert: Carl (a third party) calls poke, first immediately and then after the Autorecall is available */

        carl.setTrade(trade);

        // Assert poke() returns false when called too soon
        assertTrue(!carl.do_poke());

        // Assert poke() returns true when called at the appropriate time
        hevm.warp(defaultAutorecallInterval);
        assertTrue(carl.do_poke());

        // Assert Carl has received the pokeReward
        assertEq(token.balanceOf(address(carl)), carlStartingBalance + defaultPokeReward);

        // Assert the trade is properly closed and has a balance of 0
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Recalled));
        assertEq(trade.getBalance(), 0);

    }
    function test_13050_givenOpenAutoabortableTrade_whenCarlCallsPoke_assertCarlReceivesPokeRewardAndTradeClosesProperly_iffAutoabortAvailable() public {
        uint carlStartingBalance = token.balanceOf(address(carl));

        /* ---- Set up: Alice opens a trade, then Bob commits */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);

        /* ---- Action and Assert: Carl (a third party) calls poke, first immediately and then after the Autoabort is available */

        carl.setTrade(trade);

        // Assert poke() returns false when called too soon
        assertTrue(!carl.do_poke());

        // Assert poke() returns true when called at the appropriate time
        hevm.warp(defaultAutoabortInterval);
        assertTrue(carl.do_poke());

        // Assert Carl has received the pokeReward
        assertEq(token.balanceOf(address(carl)), carlStartingBalance + defaultPokeReward);

        // Assert the trade is properly closed and has a balance of 0
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Aborted));
        assertEq(trade.getBalance(), 0);

    }
    function test_13060_givenOpenAutoreleasableTrade_whenCarlCallsPoke_assertCarlReceivesPokeRewardAndTradeClosesProperly_iffAutoreleaseAvailable() public {
        uint carlStartingBalance = token.balanceOf(address(carl));

        /* ---- Set up: Alice opens a trade, then Bob commits and claims */

        alice.startOpenTradeAsCustodian([defaultTradeAmount, defaultBeneficiaryDeposit, defaultAbortPunishment, defaultPokeReward, defaultAutorecallInterval, defaultAutoabortInterval, defaultAutoreleaseInterval, defaultDevFee], devFeeAddress);
        DAIHardTrade trade = alice.trade();
        bob.do_commit(trade);
        bob.do_claim();

        /* ---- Action and Assert: Carl (a third party) calls poke, first immediately and then after the Autorelease is available */

        carl.setTrade(trade);

        // Assert poke() returns false when called too soon
        assertTrue(!carl.do_poke());

        // Assert poke() returns true when called at the appropriate time
        hevm.warp(defaultAutoreleaseInterval);
        assertTrue(carl.do_poke());

        // Assert Carl has received the pokeReward
        assertEq(token.balanceOf(address(carl)), carlStartingBalance + defaultPokeReward);

        // Assert the trade is properly closed and has a balance of 0
        assertEq(uint(trade.phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(trade.closedReason()), uint(DAIHardTrade.ClosedReason.Released));
        assertEq(trade.getBalance(), 0);
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