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

    function setUp() public {
        hevm = Hevm(0x7109709ECfa91a80626fF3989D68f67F5b1DD12D);
        hevm.warp(0);
        
        token = new DSToken("AAA");
        token.mint(10000000000000000000000); // one milllllion DAI   http://gph.is/1OOxk4J

        factory = new DAIHardFactory(ERC20Interface(address(token)), address(0x1));

        alice = new User(token, factory);
        token.push(address(alice), 1000000000000000000000);

        bob = new User(token, factory);
        token.push(address(bob), 1000000000000000000000);

        carl = new User(token, factory);
        token.push(address(carl), 1000000000000000000000);
    }

    function test_constructor() public {
        DAIHardTrade t = new DAIHardTrade(ERC20Interface(address(token)), address(0x1), address(0x2));
        assertEq(address(t.daiContract()), address(token));
        assertEq(address(t.founderFeeAddress()), address(0x1));
        assertEq(address(t.devFeeAddress()), address(0x2));
    }

    function test_beginInOpenAsCustodian() public {
        uint preBalance = token.balanceOf(address(alice));

        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));

        assertEq(alice.trade().abortPunishment(), 15);
        assertEq(alice.trade().pokeReward(), 1);
        assertEq(alice.trade().founderFee(), 5);
        assertEq(alice.trade().devFee(), 2);
        assertEq(alice.trade().autorecallInterval(), 11);
        assertEq(alice.trade().autoabortInterval(), 12);
        assertEq(alice.trade().autoreleaseInterval(), 13);
        assertEq(alice.trade().initiator(), address(alice));
        assertEq(alice.trade().devFeeAddress(), address(0x2));
        assertTrue(alice.trade().initiatorIsCustodian());
        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Open));

        assertEq(alice.trade().custodian(), address(alice));
        assertEq(alice.trade().tradeAmount(), alice.trade().getBalance() - (5 + 1 + 2));
        assertEq(alice.trade().beneficiaryDeposit(), 100);

        uint totalDeposit = 1000 + 1 + 2 + 5;
        assertEq(alice.trade().getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), preBalance - totalDeposit);
    }

    function test_beginInOpenAsBeneficiary() public {
        uint preBalance = token.balanceOf(address(alice));

        alice.startOpenTrade(false, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));

        assertEq(alice.trade().abortPunishment(), 15);
        assertEq(alice.trade().pokeReward(), 1);
        assertEq(alice.trade().founderFee(), 5);
        assertEq(alice.trade().devFee(), 2);
        assertEq(alice.trade().autorecallInterval(), 11);
        assertEq(alice.trade().autoabortInterval(), 12);
        assertEq(alice.trade().autoreleaseInterval(), 13);
        assertEq(alice.trade().initiator(), address(alice));
        assertEq(alice.trade().devFeeAddress(), address(0x2));
        assertTrue(! alice.trade().initiatorIsCustodian());
        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Open));

        assertEq(alice.trade().beneficiary(), address(alice));
        assertEq(alice.trade().tradeAmount(), 1000);
        assertEq(alice.trade().beneficiaryDeposit(), alice.trade().getBalance() - (5 + 1 + 2));

        uint totalDeposit = 100 + 1 + 2 + 5;
        assertEq(alice.trade().getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), preBalance - totalDeposit);
    }

    function test_beginInCommittedAsCustodian() public {
        uint preBalance = token.balanceOf(address(alice));

        alice.startCommittedTradeAsCustodian(address(bob), [uint(1000), uint(100), uint(15), uint(1), uint(12), uint(13), uint(2)], address(0x2));

        assertEq(alice.trade().beneficiaryDeposit(), 100);
        assertEq(alice.trade().abortPunishment(), 15);
        assertEq(alice.trade().pokeReward(), 1);
        assertEq(alice.trade().autoabortInterval(), 12);
        assertEq(alice.trade().autoreleaseInterval(), 13);
        assertEq(alice.trade().founderFee(), 5);
        assertEq(alice.trade().devFee(), 2);
        assertTrue(alice.trade().initiatorIsCustodian());
        assertEq(alice.trade().custodian(), address(alice));
        assertEq(alice.trade().beneficiary(), address(bob));
        assertEq(alice.trade().devFeeAddress(), address(0x2));
        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Committed));
        assertEq(alice.trade().tradeAmount(), alice.trade().getBalance() - (5 + 2 + 1 + 100));

        assertEq(alice.trade().initiator(), address(alice));
        assertEq(alice.trade().responder(), address(bob));

        uint totalDeposit = 1000 + 100 + 1 + 2 + 5;
        assertEq(alice.trade().getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), preBalance - totalDeposit);
    }

    function test_beginInCommittedAsBeneficiary() public {
        uint preBalance = token.balanceOf(address(alice));

        alice.startCommittedTradeAsBeneficiary(address(bob), [uint(1000), uint(100), uint(15), uint(1), uint(12), uint(13), uint(2)], address(0x2));

        assertEq(alice.trade().beneficiaryDeposit(), 100);
        assertEq(alice.trade().abortPunishment(), 15);
        assertEq(alice.trade().pokeReward(), 1);
        assertEq(alice.trade().autoabortInterval(), 12);
        assertEq(alice.trade().autoreleaseInterval(), 13);
        assertEq(alice.trade().founderFee(), 5);
        assertEq(alice.trade().devFee(), 2);
        assertTrue(!alice.trade().initiatorIsCustodian());
        assertEq(alice.trade().custodian(), address(bob));
        assertEq(alice.trade().beneficiary(), address(alice));
        assertEq(alice.trade().devFeeAddress(), address(0x2));
        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Committed));
        assertEq(alice.trade().tradeAmount(), alice.trade().getBalance() - (5 + 2 + 1 + 100));

        assertEq(alice.trade().initiator(), address(alice));
        assertEq(alice.trade().responder(), address(bob));

        uint totalDeposit = 1000 + 100 + 1 + 2 + 5;
        assertEq(alice.trade().getBalance(), totalDeposit);
        assertEq(token.balanceOf(address(alice)), preBalance - totalDeposit);
    }

    function test_recall() public {
        uint preBalance = token.balanceOf(address(alice));

        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        alice.do_recall();

        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(alice.trade().closedReason()), uint(DAIHardTrade.ClosedReason.Recalled));
        assertEq(alice.trade().getBalance(), uint(0));
        assertEq(token.balanceOf(address(alice)), preBalance);
    }

    function test_commitAsCustodian() public {
        alice.startOpenTrade(false, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));

        uint preBalance = token.balanceOf(address(bob));
        bob.do_commit(alice.trade());
        uint totalDeposit = 1000;
        assertEq(token.balanceOf(address(bob)), preBalance - totalDeposit);
        assertEq(bob.trade().responder(), address(bob));
        assertEq(uint(bob.trade().phase()), uint(DAIHardTrade.Phase.Committed));
    }

    function test_commitAsBeneficiary() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));

        uint preBalance = token.balanceOf(address(bob));
        bob.do_commit(alice.trade());
        uint totalDeposit = 100;
        assertEq(token.balanceOf(address(bob)), preBalance - totalDeposit);
        assertEq(bob.trade().responder(), address(bob));
        assertEq(uint(bob.trade().phase()), uint(DAIHardTrade.Phase.Committed));
    }

    function test_abort() public {
        uint alicePreBalance = token.balanceOf(address(alice));
        uint bobPreBalance = token.balanceOf(address(bob));

        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        bob.do_commit(alice.trade());
        bob.do_abort();

        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(alice.trade().closedReason()), uint(DAIHardTrade.ClosedReason.Aborted));
        assertEq(token.balanceOf(address(alice)), alicePreBalance - 15);
        assertEq(token.balanceOf(address(bob)), bobPreBalance - 15);
        assertEq(alice.trade().getBalance(), 0);
    }

    function test_claim() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        bob.do_commit(alice.trade());
        bob.do_claim();

        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Claimed));
    }

    function test_release() public {
        uint bobPreBalance = token.balanceOf(address(bob));

        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        uint aliceBalanceAfterOpen = token.balanceOf(address(alice));
        bob.do_commit(alice.trade());
        bob.do_claim();
        alice.do_release();

        assertEq(uint(alice.trade().phase()), uint(DAIHardTrade.Phase.Closed));
        assertEq(uint(alice.trade().closedReason()), uint(DAIHardTrade.ClosedReason.Released));
        assertEq(token.balanceOf(address(alice)), aliceBalanceAfterOpen + 1);
        assertEq(token.balanceOf(address(0x1)), 5);
        assertEq(token.balanceOf(address(0x2)), 2);
        assertEq(token.balanceOf(address(bob)), bobPreBalance + 1000);
    }

    function test_burn() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        uint aliceBalanceAfterOpen = token.balanceOf(address(alice));
        bob.do_commit(alice.trade());
        uint bobBalanceAfterCommit = token.balanceOf(address(bob));
        bob.do_claim();
        alice.do_burn();

        assertEq(token.balanceOf(address(0x0)), 1000 + 100 + 1 + 2 + 5);
        assertEq(alice.trade().getBalance(), 0);
        assertEq(token.balanceOf(address(alice)), aliceBalanceAfterOpen);
        assertEq(token.balanceOf(address(bob)), bobBalanceAfterCommit);
    }

    function test_autorecallAvailable() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));

        assertTrue(!alice.trade().autorecallAvailable());
        hevm.warp(10);
        assertTrue(!alice.trade().autorecallAvailable());
        hevm.warp(11);
        assertTrue(alice.trade().autorecallAvailable());
    }

    function test_autoabortAvailable() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        bob.do_commit(alice.trade());

        assertTrue(!alice.trade().autoabortAvailable());
        hevm.warp(11);
        assertTrue(!alice.trade().autoabortAvailable());
        hevm.warp(12);
        assertTrue(alice.trade().autoabortAvailable());
    }

    function test_autoreleaseAvailable() public {
        alice.startOpenTrade(true, [uint(1000), uint(100), uint(15), uint(1), uint(11), uint(12), uint(13), uint(2)], address(0x2));
        bob.do_commit(alice.trade());
        bob.do_claim();

        assertTrue(!alice.trade().autoreleaseAvailable());
        hevm.warp(12);
        assertTrue(!alice.trade().autoreleaseAvailable());
        hevm.warp(13);
        assertTrue(alice.trade().autoreleaseAvailable());
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

    modifier checkPokeRewardSentIsFalse() {
        assert(!trade.pokeRewardSent());
        _;
    }

    function setTrade(DAIHardTrade t)
    public {
        trade = t;
    }

    function startOpenTrade(bool initiatorIsCustodian, uint[8] memory uintArgs, address devFeeAddress)
    public {
        token.approve(address(factory), 1000000);
        address[2] memory addressArgs = [address(this), devFeeAddress];
        trade = factory.createOpenTrade(addressArgs, initiatorIsCustodian, uintArgs, "", "");
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
    checkPokeRewardSentIsFalse()
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
    checkPokeRewardSentIsFalse()
    public {
        trade.abort();
    }
    function do_autoabortAvailable()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public
    returns (bool) {
        return trade.autoabortAvailable();
    }
    function do_claim()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public {
        trade.claim();
    }
    function do_autoreleaseAvailable()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public
    returns (bool) {
        return trade.autoreleaseAvailable();
    }
    function do_release()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public {
        trade.release();
    }
    function do_burn()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public {
        trade.burn();
    }
    function do_getState()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public
    returns(uint balance, DAIHardTrade.Phase phase, uint phaseStartTimestamp, address responder, DAIHardTrade.ClosedReason closedReason) {
        return trade.getState();
    }
    function do_getBalance()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public
    returns(uint) {
        return trade.getBalance();
    }
    function do_getParameters()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
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
    checkPokeRewardSentIsFalse()
    public
    returns (bool) {
        return trade.pokeNeeded();
    }
    function do_poke()
    checkPhaseDoesntRegress()
    checkPokeRewardSentIsFalse()
    public {
        trade.poke();
    }
}