# Contract Audit: DAIHard

## Table of Contents

{{TOC}}

## Preamble

This audit report was undertaken by @adamdossa for the purpose of providing feedback to DAIHard. It has been written without any express or implied warranty.

The initial audit was as of as of commit:  
https://github.com/burnable-tech/DAIHard/tree/d283df5f00635d758e014a1747f497d671f6e7c1  
which was then followed by a review as of commit:
https://github.com/burnable-tech/DAIHard/blob/acd79c4f9b2db1dfa7b0a4966ebf144619f00300

## Classification

* **Comment** - A note that highlights certain decisions taken and their impact on the contract.
* **Minor** - A defect that does not have a material impact on the contract execution and is likely to be subjective.
* **Moderate** - A defect that could impact the desired outcome of the contract execution in a specific scenario.
* **Major** - A defect that impacts the desired outcome of the contract execution or introduces a weakness that may be exploited.
* **Critical** - A defect that presents a significant security vulnerability or failure of the contract across a range of scenarios.

## Code Summary

Overall the code was well-commented and alongside the explanatory documents at:  
https://github.com/burnable-tech/DAIHard/tree/plus-native-version/audit-info  
was reasonably clear in its intent.

The code is structured as two flattened Solidity files:  
DAIHard.sol - implementation of DAIHard with DAI as the escrowed currency  
DAIHardNative.sol - implementation of DAIHard with ETH as the escrowed currency

~There are no unit test case scripts or deployment scripts included in the repository.~
The unit tests in solidity/dapphub-tests/ require dapphub, and are not included in this audit.

## Approach

Each of DAIHard.sol and DAIHardNative.sol were audited.

Individual issues are listed below after which I've put some more general comments and summary.

The audit was performed by carefully reviewing the contracts as well as using automated tools such as `slither`.

The raw output of these tools is included in the Appendix. These tools often give false-positives, and any issues reported by them but not included in the issue list can be considered to be not valid.

## DAIHard.sol

DAIHard.sol consists of two main contracts, DAIHardFactory and DAIHardTrade.

### Issues

**Comment** - Emit event from `changePhase` in `DAIHardTrade`

You may wish to consider having an event:  
`event ChangePhase(address indexed caller, Phase indexed phase, CloseReason indexed closeReason)`  
and then call this from the `changePhase` function, passing 0 as the `CloseReason` where it's not needed.

e.g.:  
```
function changePhase(Phase p, CloseReason r)
internal {
    phase = p;
    closedReason = r;
    phaseStartTimestamps[uint(p)] = block.timestamp;
    phaseStartBlocknums[uint(p)] = block.number;
    emit ChangePhase(msg.sender, p, r);
}
```

You could then remove:
```
event Recalled();
event Claimed();
event Aborted();
event Released();
event Burned();
```

This would allow to watch the contracts for relevant state changes by subscribing to a single event which carries the material event metadata.

_Response_: **Left unchanged.**

**Minor** - Consider using a more recent version of solc

Solidity is very active in pushing out new versions. Make sure to include the latest stable version that is compatible with your code. The codebase is currently using solidity v0.5.6.  
`pragma solidity 0.5.6;`

This version has known bugs which have been fixed in later versions.

The latest stable release is:  
https://github.com/ethereum/solidity/releases/tag/v0.5.9

_Response_: **Left unchanged. I like to stick to one version of solc throughout a project, and as I said I'm having issues changing dapphub test's version of solc.**

**Minor** - Changing `founderFeeAddress` in `DAIHardFactory`

`founderFeeAddress` is set in the constructor of `DAIHardFactory` and can't thereafter be modified.

I would consider adding the ability for the founders address to modify the `founderFeeAddress`.

This adds additional security as you can change the address if the previous address is compromised, at no additional cost (since only the founders fee address can modify it).

_Response_: **Left unchanged. One of the essential featurs of the Factories is that we have no control over them, making founderFeeAddress unchangeable in DH is part of that. While power over the founderFeeAddress is a minor thing, it changes the contracts from 100% unowned to *almost* 100% unowned.**

**Comment** - `initiatorIsCustodian` is redundant in DAIHardTrade

This value is stored in the state of DAIHardTrade:  
`bool public initiatorIsCustodian;`

This value will always be:  
`custodian == initiator`  
at any point where it is referenced.

You could avoid storing it in the contract state and save gas.

_Response_: **Left unchanged. The interface depends on this variable when calling getParameters, and this way we can guarantee that the value doesn't change in the edge case of a user committing to his own payment.**

_Status_: **Closed**

**Minor** - missing `require` on DAI transfer in DAIHardTrade

In line 686:  
```
daiContract.transfer(msg.sender, pokeReward);
```
the transfer should be wrapped in a require statement for consistency.

_Response_: **Changed as suggested.**

_Status_: **Closed**

**Comment** - use of Proxy deployment pattern to save gas

It would be relatively straightforward to modify these contracts to use the proxy deployment pattern to only deploy a proxy stub instead of the full DAIHardTrade contract. You wouldn't need upgradable proxies, so this would be purely for gas saving on new DAIHardTrade contracts.

Reference:  
https://blog.zeppelinos.org/proxy-patterns/

_Response_: **Not implemented. While saving gas is always lovely, this increases the scope of development and the security profile dramatically.**

**Minor** - use external modifiers where possible

For consistency it would be best to mark the below functions as external rather than public.

DAIHard.sol:
```
DAIHardTrade.beginInOpenPhase (DAIHard.sol#353-391)
DAIHardTrade.beginInCommittedPhase (DAIHard.sol#404-447)
DAIHardTrade.pokeNeeded (DAIHard.sol#670-680)
DAIHardTrade.initiatorStatement (DAIHard.sol#726-731)
DAIHardTrade.responderStatement (DAIHard.sol#733-738)
```

DAIHardNative.sol:
```
DAIHardTrade.beginInOpenPhase (DAIHardNative.sol#332-373)
DAIHardTrade.beginInCommittedPhase (DAIHardNative.sol#386-432)
DAIHardTrade.pokeNeeded (DAIHardNative.sol#654-664)
DAIHardTrade.initiatorStatement (DAIHardNative.sol#710-715)
DAIHardTrade.responderStatement (DAIHardNative.sol#717-722)
```

_Response_: **Changed as suggested**

_Status_: **Closed**

**Minor** - perform input parameter validation where possible

In functions such as `createOpenTrade` the contract can perform some sanity checking on parameters.

e.g.
```
require(addressArgs[0] != address(0x0), "Incorrect initiator");
```

You could also force minimum values on the auto*Interval values if appropriate.

You can also sanity check addresses in `createCommittedTrade`:
```
require(addressArgs[0] != address(0x0), "Incorrect custodian");
require(addressArgs[1] != address(0x0), "Incorrect beneficiary");
```

You can also sanity check the address when someone commits to a trade in DAIHard and DAIHardNative.

e.g.
```
function commit(address _responder, string calldata commPubkey)
external
inPhase(Phase.Open)
/* any msg.sender */ {
    require(_responder != address(0x0), "Incorrect responder");
    require(!autorecallAvailable(), "autorecallInterval has passed; this offer has expired.");
```

This will avoid funds getting locked in contracts with incorrect addresses.

_Response_: **Changed as suggested**

_Status_: **Closed**

**Comment** - add parameters to NewTrade event in DAIHardFactory

If you want to easily be able to show users filtered trades where they are the initiator, you may wish to amend the NewTrade event to something like:  
```
event NewTrade(uint id, address tradeAddress, address indexed initiator);
```

_Response_: **Changed as suggested**

_Status_: **Closed**

**Comment** - simplify code in `internalAbort()`

In this function you have:  
```
// Refund to initiator should include founderFee and devFee
uint sendBackToInitiator = founderFee.add(devFee);
// If there was a pokeReward left, it should also be sent back to the initiator
if (!pokeRewardGranted) {
    sendBackToInitiator = sendBackToInitiator.add(pokeReward);
}

require(daiContract.transfer(initiator, sendBackToInitiator), "Token refund of founderFee+devFee+pokeReward to Initiator failed!");
```

I believe this could be replaced with:
```
require(daiContract.transfer(initiator, getBalance()), "Token refund of founderFee+devFee+pokeReward to Initiator failed!");
```

_Response_: **Changed as suggested**

_Status_: **Closed**

**Minor** - Function / State / Events / Modifier ordering

Common practice is to order the contract with state variables, then event declarations, then functions (modifiers, constructor, functions).

This would make the code significantly more readable:  
https://solidity.readthedocs.io/en/v0.5.3/style-guide.html#order-of-layout

_Response_: **Left unchanged. I prefer to group the contract code by functionality (i.e. phase, read-only functions, etc). I think this makes it a little easier to read through an entire contract and understand its lifecycle.**

**Comment** - consistent function naming

At the moment there are functions such as `internalRecall`.

Standard practice is to use an `_` before internal functions, so this becomes:  
`function _recall`
which seems clearer.

_Response_: **Changed as suggested**

_Status_: **Closed**

**Comment** - consider adopting the NatSpec scheme for code and function comments

Generally solidity files are commented using NatSpec comments. This allows function comments to be easily parsed.

Reference:
https://solidity.readthedocs.io/en/v0.5.3/style-guide.html#natspec

_Response_: **Left unchanged**

## DAIHardNative.sol

DAIHardNative.sol consists of two main contracts, DAIHardFactory and DAIHardTrade.

Much of the code and business logic is in common with DAIHard.sol and the above issues for DAIHard.sol generally apply to DAIHardNative.sol as well.

### Additional Issues

**Critical** - possible to grief counterparty and break game theoretic assumptions

DAIHardNative has a major issue related to how ETH is transferred vs. how ERC20's are transferred.

With ETH, a call to e.g. `msg.sender.transfer(...)` can revert if `msg.sender` is not able to receive funds.

Whilst an EOA (external) account can always receive ETH, contracts may not have a payable function or have a payable function with a high gas cost. More over the payable function can be logically changed so that it is callable from a `transfer` only in certain circumstances.

This would allow either party to a DAIHardNative trade to prevent the trade from taking any action which involved transferring funds to themselves. They could also use a smart contract which only allowed funds to be received under certain circumstances that could depend on the state (e.g. Phase) of the DAIHard trade. They could then show this to the counterparty who would know that unless they take certain actions the trade would be unexecutable and all funds would remain locked on it.

_Response_: **Addressed, by replacing transfer() with call(), so that in the worst case, one party fails to receive payment but the other can still get the funds out.**

_Status_: **Closed - noting funds can be permanently locked in the contract, but only if the account due funds is a contract that is unable to receive ETH.**

## General Recommendations

### Include built unit test cases

It would add significant confidence in the correctness of the code if there were test cases that exercised all of the relevant game paths that can occur.

## Appendix

### Disclosure

The Reports are not an endorsement or indictment of any particular project or team, and the Reports do not guarantee the security of any particular project. This Report does not consider, and should not be interpreted as considering or having any bearing on, the potential economics of a token, token sale or any other product, service or other asset. Cryptographic tokens are emergent technologies and carry with them high levels of technical risk and uncertainty. No Report provides any warranty or representation to any Third-Party in any respect, including regarding the bugfree nature of code, the business model or proprietors of any such business model, and the legal compliance of any such business. No third party should rely on the Reports in any way, including for the purpose of making any decisions to buy or sell any token, product, service or other asset. Specifically, for the avoidance of doubt, this Report does not constitute investment advice, is not intended to be relied upon as investment advice, is not an endorsement of this project or team, and it is not a guarantee as to the absolute security of the project. There is no owed duty to any Third-Party by virtue of publishing these Reports.

PURPOSE OF REPORTS The Reports and the analysis described therein are created solely for Clients and published with their consent. The scope of our review is limited to a review of Solidity code and only the Solidity code we note as being within the scope of our review within this report. The Solidity language itself remains under development and is subject to unknown risks and flaws. The review does not extend to the compiler layer, or any other areas beyond Solidity that could present security risks. Cryptographic tokens are emergent technologies and carry with them high levels of technical risk and uncertainty.

### Contract Summary

+ Contract SafeMath
  - From SafeMath
    - add(uint256,uint256) (internal)
    - div(uint256,uint256) (internal)
    - mod(uint256,uint256) (internal)
    - mul(uint256,uint256) (internal)
    - sub(uint256,uint256) (internal)

+ Contract ERC20Interface
  - From ERC20Interface
    - allowance(address,address) (public)
    - approve(address,uint256) (public)
    - balanceOf(address) (public)
    - totalSupply() (public)
    - transfer(address,uint256) (public)
    - transferFrom(address,address,uint256) (public)

+ Contract DAIHardFactory
  - From DAIHardFactory
    - constructor(ERC20Interface,address) (public)
    - createCommittedTrade(address[3],bool,uint256[7],string,string,string) (external)
    - createOpenTrade(address[2],bool,uint256[8],string,string) (external)
    - getFounderFee(uint256) (public)
    - numTrades() (external)

+ Contract DAIHardTrade
  - From DAIHardTrade
    - abort() (external)
    - autoabortAvailable() (public)
    - autorecallAvailable() (public)
    - autoreleaseAvailable() (public)
    - beginInCommittedPhase(address,address,bool,uint256[7],string,string,string) (public)
    - beginInOpenPhase(address,bool,uint256[8],string,string) (public)
    - burn() (external)
    - claim() (external)
    - commit(address,string) (external)
    - constructor(ERC20Interface,address,address) (public)
    - getBalance() (public)
    - getParameters() (external)
    - getPhaseStartInfo() (external)
    - getResponderDeposit() (public)
    - getState() (external)
    - initiatorStatement(string) (public)
    - poke() (external)
    - pokeNeeded() (public)
    - recall() (external)
    - release() (external)
    - responderStatement(string) (public)
    - changePhase(DAIHardTrade.Phase) (internal)
    - grantPokeRewardToSender() (internal)
    - internalAbort() (internal)
    - internalBurn() (internal)
    - internalRecall() (internal)
    - internalRelease() (internal)

### Slither Results

#### DAIHard.sol

```
ethsec@84aa7d11fbf8:/share$ slither DAIHard.sol
Compilation warnings/errors on DAIHard.sol:
DAIHard.sol:761:27: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                          ^---------^
DAIHard.sol:238:5: The shadowed declaration is here:
    Phase public phase;
    ^----------------^
DAIHard.sol:761:66: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                                                                 ^---------------^
DAIHard.sol:260:5: The shadowed declaration is here:
    address public responder;
    ^----------------------^
DAIHard.sol:761:85: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                                                                                    ^-----------------------^
DAIHard.sol:246:5: The shadowed declaration is here:
    ClosedReason public closedReason;
    ^------------------------------^
DAIHard.sol:779:14: Warning: This declaration shadows an existing declaration.
    returns (address initiator,
             ^---------------^
DAIHard.sol:259:5: The shadowed declaration is here:
    address public initiator;
    ^----------------------^
DAIHard.sol:780:14: Warning: This declaration shadows an existing declaration.
             bool initiatorIsCustodian,
             ^-----------------------^
DAIHard.sol:266:5: The shadowed declaration is here:
    bool public initiatorIsCustodian;
    ^------------------------------^
DAIHard.sol:781:14: Warning: This declaration shadows an existing declaration.
             uint tradeAmount,
             ^--------------^
DAIHard.sol:314:5: The shadowed declaration is here:
    uint public tradeAmount;
    ^---------------------^
DAIHard.sol:782:14: Warning: This declaration shadows an existing declaration.
             uint beneficiaryDeposit,
             ^---------------------^
DAIHard.sol:315:5: The shadowed declaration is here:
    uint public beneficiaryDeposit;
    ^----------------------------^
DAIHard.sol:783:14: Warning: This declaration shadows an existing declaration.
             uint abortPunishment,
             ^------------------^
DAIHard.sol:316:5: The shadowed declaration is here:
    uint public abortPunishment;
    ^-------------------------^
DAIHard.sol:784:14: Warning: This declaration shadows an existing declaration.
             uint autorecallInterval,
             ^---------------------^
DAIHard.sol:318:5: The shadowed declaration is here:
    uint public autorecallInterval;
    ^----------------------------^
DAIHard.sol:785:14: Warning: This declaration shadows an existing declaration.
             uint autoabortInterval,
             ^--------------------^
DAIHard.sol:319:5: The shadowed declaration is here:
    uint public autoabortInterval;
    ^---------------------------^
DAIHard.sol:786:14: Warning: This declaration shadows an existing declaration.
             uint autoreleaseInterval,
             ^----------------------^
DAIHard.sol:320:5: The shadowed declaration is here:
    uint public autoreleaseInterval;
    ^-----------------------------^
DAIHard.sol:787:14: Warning: This declaration shadows an existing declaration.
             uint pokeReward
             ^-------------^
DAIHard.sol:322:5: The shadowed declaration is here:
    uint public pokeReward;
    ^--------------------^

INFO:Detectors:
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#695)
	- internalRecall() (DAIHard.sol#698)
	State variables written after the call(s):
	- phase (DAIHard.sol#698)
	- phaseStartTimestamps (DAIHard.sol#698)
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#702)
	- internalAbort() (DAIHard.sol#705)
	State variables written after the call(s):
	- phase (DAIHard.sol#705)
	- phaseStartTimestamps (DAIHard.sol#705)
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#709)
	- internalRelease() (DAIHard.sol#712)
	State variables written after the call(s):
	- phase (DAIHard.sol#712)
	- phaseStartTimestamps (DAIHard.sol#712)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#reentrancy-vulnerabilities-1
INFO:Detectors:
DAIHardTrade.grantPokeRewardToSender (DAIHard.sol#682-687) ignores return value by external calls "daiContract.transfer(msg.sender,pokeReward)" (DAIHard.sol#686)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#unused-return
INFO:Detectors:
DAIHardTrade.getState.phase (local variable @ DAIHard.sol#761) shadows:
	- DAIHardTrade.phase (state variable @ DAIHard.sol#238)
DAIHardTrade.getState.responder (local variable @ DAIHard.sol#761) shadows:
	- DAIHardTrade.responder (state variable @ DAIHard.sol#260)
DAIHardTrade.getState.closedReason (local variable @ DAIHard.sol#761) shadows:
	- DAIHardTrade.closedReason (state variable @ DAIHard.sol#246)
DAIHardTrade.getParameters.initiator (local variable @ DAIHard.sol#779) shadows:
	- DAIHardTrade.initiator (state variable @ DAIHard.sol#259)
DAIHardTrade.getParameters.initiatorIsCustodian (local variable @ DAIHard.sol#780) shadows:
	- DAIHardTrade.initiatorIsCustodian (state variable @ DAIHard.sol#266)
DAIHardTrade.getParameters.tradeAmount (local variable @ DAIHard.sol#781) shadows:
	- DAIHardTrade.tradeAmount (state variable @ DAIHard.sol#314)
DAIHardTrade.getParameters.beneficiaryDeposit (local variable @ DAIHard.sol#782) shadows:
	- DAIHardTrade.beneficiaryDeposit (state variable @ DAIHard.sol#315)
DAIHardTrade.getParameters.abortPunishment (local variable @ DAIHard.sol#783) shadows:
	- DAIHardTrade.abortPunishment (state variable @ DAIHard.sol#316)
DAIHardTrade.getParameters.autorecallInterval (local variable @ DAIHard.sol#784) shadows:
	- DAIHardTrade.autorecallInterval (state variable @ DAIHard.sol#318)
DAIHardTrade.getParameters.autoabortInterval (local variable @ DAIHard.sol#785) shadows:
	- DAIHardTrade.autoabortInterval (state variable @ DAIHard.sol#319)
DAIHardTrade.getParameters.autoreleaseInterval (local variable @ DAIHard.sol#786) shadows:
	- DAIHardTrade.autoreleaseInterval (state variable @ DAIHard.sol#320)
DAIHardTrade.getParameters.pokeReward (local variable @ DAIHard.sol#787) shadows:
	- DAIHardTrade.pokeReward (state variable @ DAIHard.sol#322)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#local-variable-shadowing
INFO:Detectors:
Reentrancy in DAIHardTrade.beginInCommittedPhase (DAIHard.sol#404-447):
	External calls:
	- tradeAmount = getBalance().sub(beneficiaryDeposit.add(pokeReward).add(founderFee).add(devFee)) (DAIHard.sol#438)
	State variables written after the call(s):
	- phase (DAIHard.sol#443)
	- phaseStartBlocknums (DAIHard.sol#443)
	- phaseStartTimestamps (DAIHard.sol#443)
Reentrancy in DAIHardTrade.beginInOpenPhase (DAIHard.sol#353-391):
	External calls:
	- tradeAmount = getBalance().sub(pokeReward.add(founderFee).add(devFee)) (DAIHard.sol#377)
	State variables written after the call(s):
	- beneficiaryDeposit (DAIHard.sol#378)
Reentrancy in DAIHardTrade.beginInOpenPhase (DAIHard.sol#353-391):
	External calls:
	- tradeAmount = getBalance().sub(pokeReward.add(founderFee).add(devFee)) (DAIHard.sol#377)
	- beneficiaryDeposit = getBalance().sub(pokeReward.add(founderFee).add(devFee)) (DAIHard.sol#383)
	State variables written after the call(s):
	- phase (DAIHard.sol#389)
	- phaseStartBlocknums (DAIHard.sol#389)
	- phaseStartTimestamps (DAIHard.sol#389)
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#695)
	- internalRecall() (DAIHard.sol#698)
	State variables written after the call(s):
	- closedReason (DAIHard.sol#698)
	- phaseStartBlocknums (DAIHard.sol#698)
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#702)
	- internalAbort() (DAIHard.sol#705)
	State variables written after the call(s):
	- closedReason (DAIHard.sol#705)
	- phaseStartBlocknums (DAIHard.sol#705)
Reentrancy in DAIHardTrade.poke (DAIHard.sol#689-716):
	External calls:
	- grantPokeRewardToSender() (DAIHard.sol#709)
	- internalRelease() (DAIHard.sol#712)
	State variables written after the call(s):
	- closedReason (DAIHard.sol#712)
	- phaseStartBlocknums (DAIHard.sol#712)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#reentrancy-vulnerabilities-2
INFO:Detectors:
DAIHardTrade.autorecallAvailable (DAIHard.sol#487-493) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Open)].add(autorecallInterval)) (DAIHard.sol#492)
DAIHardTrade.autoabortAvailable (DAIHard.sol#570-576) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Committed)].add(autoabortInterval)) (DAIHard.sol#575)
DAIHardTrade.autoreleaseAvailable (DAIHard.sol#632-638) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Judgment)].add(autoreleaseInterval)) (DAIHard.sol#637)
DAIHardTrade.pokeNeeded (DAIHard.sol#670-680) uses timestamp for comparisons
	Dangerous comparisons:
	- ((phase == Phase.Open && autorecallAvailable()) || (phase == Phase.Committed && autoabortAvailable()) || (phase == Phase.Judgment && autoreleaseAvailable())) (DAIHard.sol#676-679)
DAIHardTrade.poke (DAIHard.sol#689-716) uses timestamp for comparisons
	Dangerous comparisons:
	- phase == Phase.Open && autorecallAvailable() (DAIHard.sol#694-715)
	- phase == Phase.Judgment && autoreleaseAvailable() (DAIHard.sol#708-715)
	- phase == Phase.Committed && autoabortAvailable() (DAIHard.sol#701-715)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#block-timestamp
INFO:Detectors:
ERC20Interface.decimals should be constant (DAIHard.sol#76)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#state-variables-that-could-be-declared-constant
INFO:Detectors:
ERC20Interface.totalSupply (DAIHard.sol#69) should be declared external
ERC20Interface.balanceOf (DAIHard.sol#70) should be declared external
ERC20Interface.allowance (DAIHard.sol#71) should be declared external
ERC20Interface.transfer (DAIHard.sol#72) should be declared external
ERC20Interface.approve (DAIHard.sol#73) should be declared external
ERC20Interface.transferFrom (DAIHard.sol#74) should be declared external
DAIHardTrade.beginInOpenPhase (DAIHard.sol#353-391) should be declared external
DAIHardTrade.beginInCommittedPhase (DAIHard.sol#404-447) should be declared external
DAIHardTrade.pokeNeeded (DAIHard.sol#670-680) should be declared external
DAIHardTrade.initiatorStatement (DAIHard.sol#726-731) should be declared external
DAIHardTrade.responderStatement (DAIHard.sol#733-738) should be declared external
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#public-function-that-could-be-declared-as-external
INFO:Detectors:
Pragma version "0.5.6" is known to contain severe issue (https://solidity.readthedocs.io/en/v0.5.8/bugs.html) (DAIHard.sol#1)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#incorrect-versions-of-solidity
INFO:Detectors:
Parameter '_daiContract' of DAIHardFactory. (DAIHard.sol#90) is not in mixedCase
Parameter '_founderFeeAddress' of DAIHardFactory. (DAIHard.sol#90) is not in mixedCase
Parameter '_commPubkey' of DAIHardFactory.createOpenTrade (DAIHard.sol#136) is not in mixedCase
Parameter '_terms' of DAIHardFactory.createCommittedTrade (DAIHard.sol#195) is not in mixedCase
Parameter '_initiatorCommPubkey' of DAIHardFactory.createCommittedTrade (DAIHard.sol#196) is not in mixedCase
Parameter '_responderCommPubkey' of DAIHardFactory.createCommittedTrade (DAIHard.sol#197) is not in mixedCase
Parameter '_daiContract' of DAIHardTrade. (DAIHard.sol#299) is not in mixedCase
Parameter '_founderFeeAddress' of DAIHardTrade. (DAIHard.sol#299) is not in mixedCase
Parameter '_devFeeAddress' of DAIHardTrade. (DAIHard.sol#299) is not in mixedCase
Parameter '_initiator' of DAIHardTrade.beginInOpenPhase (DAIHard.sol#353) is not in mixedCase
Parameter '_initiatorIsCustodian' of DAIHardTrade.beginInOpenPhase (DAIHard.sol#354) is not in mixedCase
Parameter '_custodian' of DAIHardTrade.beginInCommittedPhase (DAIHard.sol#404) is not in mixedCase
Parameter '_beneficiary' of DAIHardTrade.beginInCommittedPhase (DAIHard.sol#405) is not in mixedCase
Parameter '_initiatorIsCustodian' of DAIHardTrade.beginInCommittedPhase (DAIHard.sol#406) is not in mixedCase
Parameter '_responder' of DAIHardTrade.commit (DAIHard.sol#495) is not in mixedCase
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#conformance-to-solidity-naming-conventions
INFO:Slither:DAIHard.sol analyzed (4 contracts), 55 result(s) found
```

#### DAIHardNative.sol

```
ethsec@84aa7d11fbf8:/share$ slither DAIHardNative.sol
Compilation warnings/errors on DAIHardNative.sol:
DAIHardNative.sol:745:27: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                          ^---------^
DAIHardNative.sol:219:5: The shadowed declaration is here:
    Phase public phase;
    ^----------------^
DAIHardNative.sol:745:66: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                                                                 ^---------------^
DAIHardNative.sol:241:5: The shadowed declaration is here:
    address payable public responder;
    ^------------------------------^
DAIHardNative.sol:745:85: Warning: This declaration shadows an existing declaration.
    returns(uint balance, Phase phase, uint phaseStartTimestamp, address responder, ClosedReason closedReason) {
                                                                                    ^-----------------------^
DAIHardNative.sol:227:5: The shadowed declaration is here:
    ClosedReason public closedReason;
    ^------------------------------^
DAIHardNative.sol:763:14: Warning: This declaration shadows an existing declaration.
    returns (address initiator,
             ^---------------^
DAIHardNative.sol:240:5: The shadowed declaration is here:
    address payable public initiator;
    ^------------------------------^
DAIHardNative.sol:764:14: Warning: This declaration shadows an existing declaration.
             bool initiatorIsCustodian,
             ^-----------------------^
DAIHardNative.sol:247:5: The shadowed declaration is here:
    bool public initiatorIsCustodian;
    ^------------------------------^
DAIHardNative.sol:765:14: Warning: This declaration shadows an existing declaration.
             uint tradeAmount,
             ^--------------^
DAIHardNative.sol:293:5: The shadowed declaration is here:
    uint public tradeAmount;
    ^---------------------^
DAIHardNative.sol:766:14: Warning: This declaration shadows an existing declaration.
             uint beneficiaryDeposit,
             ^---------------------^
DAIHardNative.sol:294:5: The shadowed declaration is here:
    uint public beneficiaryDeposit;
    ^----------------------------^
DAIHardNative.sol:767:14: Warning: This declaration shadows an existing declaration.
             uint abortPunishment,
             ^------------------^
DAIHardNative.sol:295:5: The shadowed declaration is here:
    uint public abortPunishment;
    ^-------------------------^
DAIHardNative.sol:768:14: Warning: This declaration shadows an existing declaration.
             uint autorecallInterval,
             ^---------------------^
DAIHardNative.sol:297:5: The shadowed declaration is here:
    uint public autorecallInterval;
    ^----------------------------^
DAIHardNative.sol:769:14: Warning: This declaration shadows an existing declaration.
             uint autoabortInterval,
             ^--------------------^
DAIHardNative.sol:298:5: The shadowed declaration is here:
    uint public autoabortInterval;
    ^---------------------------^
DAIHardNative.sol:770:14: Warning: This declaration shadows an existing declaration.
             uint autoreleaseInterval,
             ^----------------------^
DAIHardNative.sol:299:5: The shadowed declaration is here:
    uint public autoreleaseInterval;
    ^-----------------------------^
DAIHardNative.sol:771:14: Warning: This declaration shadows an existing declaration.
             uint pokeReward
             ^-------------^
DAIHardNative.sol:301:5: The shadowed declaration is here:
    uint public pokeReward;
    ^--------------------^

INFO:Detectors:
DAIHardTrade.internalRecall (DAIHardNative.sol#460-470) sends eth to arbitrary user
	Dangerous calls:
	- initiator.transfer(getBalance()) (DAIHardNative.sol#467)
DAIHardTrade.internalAbort (DAIHardNative.sol#525-552) sends eth to arbitrary user
	Dangerous calls:
	- beneficiary.transfer(beneficiaryDeposit.sub(abortPunishment)) (DAIHardNative.sol#541)
	- custodian.transfer(tradeAmount.sub(abortPunishment)) (DAIHardNative.sol#542)
	- initiator.transfer(sendBackToInitiator) (DAIHardNative.sol#551)
DAIHardTrade.internalRelease (DAIHardNative.sol#595-614) sends eth to arbitrary user
	Dangerous calls:
	- initiator.transfer(pokeReward) (DAIHardNative.sol#604)
	- founderFeeAddress.transfer(founderFee) (DAIHardNative.sol#609)
	- devFeeAddress.transfer(devFee) (DAIHardNative.sol#610)
	- beneficiary.transfer(getBalance()) (DAIHardNative.sol#613)
DAIHardTrade.grantPokeRewardToSender (DAIHardNative.sol#666-671) sends eth to arbitrary user
	Dangerous calls:
	- msg.sender.transfer(pokeReward) (DAIHardNative.sol#670)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#functions-that-send-ether-to-arbitrary-destinations
INFO:Detectors:
DAIHardTrade.getState.phase (local variable @ DAIHardNative.sol#745) shadows:
	- DAIHardTrade.phase (state variable @ DAIHardNative.sol#219)
DAIHardTrade.getState.responder (local variable @ DAIHardNative.sol#745) shadows:
	- DAIHardTrade.responder (state variable @ DAIHardNative.sol#241)
DAIHardTrade.getState.closedReason (local variable @ DAIHardNative.sol#745) shadows:
	- DAIHardTrade.closedReason (state variable @ DAIHardNative.sol#227)
DAIHardTrade.getParameters.initiator (local variable @ DAIHardNative.sol#763) shadows:
	- DAIHardTrade.initiator (state variable @ DAIHardNative.sol#240)
DAIHardTrade.getParameters.initiatorIsCustodian (local variable @ DAIHardNative.sol#764) shadows:
	- DAIHardTrade.initiatorIsCustodian (state variable @ DAIHardNative.sol#247)
DAIHardTrade.getParameters.tradeAmount (local variable @ DAIHardNative.sol#765) shadows:
	- DAIHardTrade.tradeAmount (state variable @ DAIHardNative.sol#293)
DAIHardTrade.getParameters.beneficiaryDeposit (local variable @ DAIHardNative.sol#766) shadows:
	- DAIHardTrade.beneficiaryDeposit (state variable @ DAIHardNative.sol#294)
DAIHardTrade.getParameters.abortPunishment (local variable @ DAIHardNative.sol#767) shadows:
	- DAIHardTrade.abortPunishment (state variable @ DAIHardNative.sol#295)
DAIHardTrade.getParameters.autorecallInterval (local variable @ DAIHardNative.sol#768) shadows:
	- DAIHardTrade.autorecallInterval (state variable @ DAIHardNative.sol#297)
DAIHardTrade.getParameters.autoabortInterval (local variable @ DAIHardNative.sol#769) shadows:
	- DAIHardTrade.autoabortInterval (state variable @ DAIHardNative.sol#298)
DAIHardTrade.getParameters.autoreleaseInterval (local variable @ DAIHardNative.sol#770) shadows:
	- DAIHardTrade.autoreleaseInterval (state variable @ DAIHardNative.sol#299)
DAIHardTrade.getParameters.pokeReward (local variable @ DAIHardNative.sol#771) shadows:
	- DAIHardTrade.pokeReward (state variable @ DAIHardNative.sol#301)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#local-variable-shadowing
INFO:Detectors:
DAIHardTrade.autorecallAvailable (DAIHardNative.sol#472-478) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Open)].add(autorecallInterval)) (DAIHardNative.sol#477)
DAIHardTrade.autoabortAvailable (DAIHardNative.sol#554-560) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Committed)].add(autoabortInterval)) (DAIHardNative.sol#559)
DAIHardTrade.autoreleaseAvailable (DAIHardNative.sol#616-622) uses timestamp for comparisons
	Dangerous comparisons:
	- (block.timestamp >= phaseStartTimestamps[uint256(Phase.Judgment)].add(autoreleaseInterval)) (DAIHardNative.sol#621)
DAIHardTrade.pokeNeeded (DAIHardNative.sol#654-664) uses timestamp for comparisons
	Dangerous comparisons:
	- ((phase == Phase.Open && autorecallAvailable()) || (phase == Phase.Committed && autoabortAvailable()) || (phase == Phase.Judgment && autoreleaseAvailable())) (DAIHardNative.sol#660-663)
DAIHardTrade.poke (DAIHardNative.sol#673-700) uses timestamp for comparisons
	Dangerous comparisons:
	- phase == Phase.Committed && autoabortAvailable() (DAIHardNative.sol#685-699)
	- phase == Phase.Judgment && autoreleaseAvailable() (DAIHardNative.sol#692-699)
	- phase == Phase.Open && autorecallAvailable() (DAIHardNative.sol#678-699)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#block-timestamp
INFO:Detectors:
DAIHardTrade.beginInOpenPhase (DAIHardNative.sol#332-373) should be declared external
DAIHardTrade.beginInCommittedPhase (DAIHardNative.sol#386-432) should be declared external
DAIHardTrade.pokeNeeded (DAIHardNative.sol#654-664) should be declared external
DAIHardTrade.initiatorStatement (DAIHardNative.sol#710-715) should be declared external
DAIHardTrade.responderStatement (DAIHardNative.sol#717-722) should be declared external
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#public-function-that-could-be-declared-as-external
INFO:Detectors:
Pragma version "0.5.6" is known to contain severe issue (https://solidity.readthedocs.io/en/v0.5.8/bugs.html) (DAIHardNative.sol#1)
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#incorrect-versions-of-solidity
INFO:Detectors:
Parameter '_founderFeeAddress' of DAIHardFactory. (DAIHardNative.sol#75) is not in mixedCase
Parameter '_commPubkey' of DAIHardFactory.createOpenTrade (DAIHardNative.sol#120) is not in mixedCase
Parameter '_terms' of DAIHardFactory.createCommittedTrade (DAIHardNative.sol#176) is not in mixedCase
Parameter '_initiatorCommPubkey' of DAIHardFactory.createCommittedTrade (DAIHardNative.sol#177) is not in mixedCase
Parameter '_responderCommPubkey' of DAIHardFactory.createCommittedTrade (DAIHardNative.sol#178) is not in mixedCase
Parameter '_founderFeeAddress' of DAIHardTrade. (DAIHardNative.sol#279) is not in mixedCase
Parameter '_devFeeAddress' of DAIHardTrade. (DAIHardNative.sol#279) is not in mixedCase
Parameter '_initiator' of DAIHardTrade.beginInOpenPhase (DAIHardNative.sol#332) is not in mixedCase
Parameter '_initiatorIsCustodian' of DAIHardTrade.beginInOpenPhase (DAIHardNative.sol#333) is not in mixedCase
Parameter '_custodian' of DAIHardTrade.beginInCommittedPhase (DAIHardNative.sol#386) is not in mixedCase
Parameter '_beneficiary' of DAIHardTrade.beginInCommittedPhase (DAIHardNative.sol#387) is not in mixedCase
Parameter '_initiatorIsCustodian' of DAIHardTrade.beginInCommittedPhase (DAIHardNative.sol#388) is not in mixedCase
Parameter '_responder' of DAIHardTrade.commit (DAIHardNative.sol#480) is not in mixedCase
Reference: https://github.com/crytic/slither/wiki/Detector-Documentation#conformance-to-solidity-naming-conventions
INFO:Slither:DAIHardNative.sol analyzed (3 contracts), 40 result(s) found
```
