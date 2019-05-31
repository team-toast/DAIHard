High-level spec for DAIHardFactory and DAIHardTrade
===

# DAIHardFactory

The Factory's primary purpose is best explained from the point of view of an interface. An interface uses the Factory contract to 1) create/begin new trades, either in the Open or Committed phase, and get the newly created trade's ID, and 2) get the CreationInfo (which contains the address and block creation number) of any trade, given the trade's ID.

The mechanics of creating a trade is discussed below in **Creating Phase**.

The only mutable state variable the Factory has is the array of CreationInfo structs. This array is only ever pushed to, and never otherwise modified. The ID of a Trade is simply its position in this array.

The Factory has two other state variables, daiContract and founderFeeAddress, but these are never changed during the Factory's lifetime.

The Factory cannot pause or suicide, and does not have an owner.

A final note: The Factory passes the founderFeeAddress to newly created trades, ensuring that if the trade resolves in a release, the founderFeeAddress will receive 0.05% of the trade value.

# DAIHardTrade

(We are only concerned with Factory-created DAIHardTrades. Manually-created trades are outside the scope of our analysis; interfaces should only interact with Trades that come from the Factory.)

The Trade's main purpose is to facilitate a trade (of DAI for some other asset or service) between two agents who don't know each other. The Trade provides rules such that cooperation is overwhelmingly likely even though there is no third-party arbiter.

The Trade's utility includes not just the Trade logic given two parties, but also a sort of game-theoretical advertisement of an offer from one party to any other party (see **Open Phase** below).

## Roles

There are four role *variables*, but only ever a maximum of two parties in the trade. Depending on how the trade is opened, the Initiator is either the Custodian or the Beneficiary; and the Responder is whatever role the Initiator left open.

Each role variable never changes, once it is assigned to some agent address.

As mentioned in the security document, each function has either an `only[role]()` modifier or an explicit "any msg.sender" comment.

### Initiator and Responder

If the trade is begun in an open mode, it has an **Initiator** but not yet a **Responder**. The Initiator has chosen all Trade settings (including the `terms`). He can also choose to recall, as described below in **Open Phase.**

When another user commits (more on this below in **Open Phase**), he becomes the **Responder**. Alternatively, the Trade may be started in a Committed phase right away, in which case both an Initiator and Responder are set from the beginning.

After the Open Phase, the Initiator and Responder roles aren't very relevant, except that certain un-spent fees will return to the Initiator if the trade ends in an abort or recall, since they were first funded by the Initiator.

### Custodian and Beneficiary

(Note that PR for DAIHard generally refers to the Beneficiary as the *buyer* and the Custodian as the *seller*. This is because our pilot interface has the Custodian "selling" DAI and the Beneficiary "buying" it.)

The **Custodian** has deposited `tradeAmount` DAI into the Trade, and has the final burn/release decision in the Judgment phase (more on this below, under **Judgment Phase**). The Custodian can be thought of as a mixture of a *customer* and *judge*, and it is in the Custodian's interest to burn the DAI if the Beneficiary does not deliver whatever `terms` were set forth upon Trade creation. Otherwise, he can manually release or let the auto-release occur after `autoreleaseInterval` (more on this under **Phase Intervals, Auto\* Behavior, and Poke()**).

The **Beneficiary** has deposited a smaller `beneficiaryDeposit` into the Trade, and has the claim/abort decision in the Committed phase (more on this below, under **Committed Phase**). Crucially, the Beneficiary is the only possible recipient of the Trade's balance once the trade moves to the Judgment phase. The Beneficiary can be thought of as a *worker*, *courier* or *entrepreneur*, and it's in the Beneficiary's interest to satisfy the Custodian regarding whatever `terms` were set forth upon Trade creation.

## The `Initiated` Event and the `terms` Argument

When the Trade is initiated, a string argument `terms` is included and emitted with the `Initiated` event, but not stored in state (to reduce gas costs). The Initiator is expected to include in natural language the terms by which the Custodian shall judge whether to burn or release. (In the pilot use case of fiat/DAI exchange, the terms could be something like "the beneficiary must meet the custodian in in London and give him 50 EUR".)

This is merely a communication tool, so that by the time the Trade is in the Committed phase, both parties are in agreement on what the Beneficiary must do, and by what criteria the Custodian will decide to burn or release in the final analysis (more on this in **Judgment Phase**).

## Phases

The bulk of the Trade's functions are contained in specific **phases**. As mentioned in the security notes document, each function has either an `inPhase` modifier or an explicit "any phase" comment.

The contract code is arranged according to each Phase, so we recommend following along in the Solidity while reading this section.

### *Creating* Phase

This phase is never seen by the user or interface; the Factory always moves the Trade past this phase in a single transaction. It's a technical necessity that isn't very interesting!

When the Factory begins a new Trade, it first instantiates the Trade, which begins in the **Creating** phase. At this point, the trade has very few variables set (none that are relevant to end-user intentions, such as trade amount or interval lengths).

Then the Factory calculates and sends the appropriate amount of DAI from msg.sender to the newly created trade.

Finally, the Factory calls either beginInOpenPhase or beginInCommittedPhase. This moves the Trade into the appropriate phase, and sets the rest of the state variables.

This 3-step process allows the Trade to take into account the amount of DAI it had been funded with, when setting its initial state.

### *Open* Phase

In the Open phase, the Initiator waits for a Responder to commit.

If the Initiator is the Custodian, he has already deposited `tradeAmount` of DAI into the Trade, and he is waiting for someone to `commit()` to the Beneficiary role, which requires a deposit of `beneficiaryDeposit` DAI into the Trade.

Alternatively, if the Initiator is the Beneficiary, he has already deposited `beneficiaryDeposit` into the Trade, and waits for a someone to `commit()` to the Custodian role by depositing the full `tradeAmount`.

Until and unless someone commits to the missing role, the Initiator can call `recall()`. This closes the trade and refunds the Trade's balance to the Initiator.

### *Committed* Phase

In the Committed Phase, there is both an Beneficiary and Custodian (as well as, less crucially, an Initiator and Responder). Again, these roles will never change hereafter.

This phase indicates that both parties have *committed* to meeting the `terms`. Either:
- the Trade was initiated by the Beneficiary, who described in `terms` what he would be willing to do in exchange for `tradeAmount`, and the Custodian committed and deposited `tradeAmount` because he'd like to see that action done in exchange for `tradeAmount` DAI
- the Trade was initiated by the Custodian, who described in `terms` what any committed Beneficiary must do in exchange for `tradeAmount`, and the Beneficiary committed and deposited `beneficiaryDeposit` because he'd like to earn `tradeAmount` DAI in exchange for meeting the `terms`.

In this phase, the Beneficiary is expected to meet the `terms` first, and then call `claim()`, which moves the contract to the Judgment Phase. By doing this, he indicate he is ready for the Custodian to make the final burn/release call. 

Alternatively, the Beneficiary can call `abort()` if he can't meet the `terms`. This burns `abortPunishment` (set upon Trade initiation) from each party and returns the remainder to each, and closes the contract.

### *Judgment* Phase

In this phase, the Beneficiary has indicated he has met the `terms`, and awaits the judgement of the Custodian.

The Custodian has two options: burn or release. Burning simply destroys the balance of the Trade by sending all the DAI to the 0x0 address. Releasing sends the balance of DAI to the Beneficiary. Either option moves the Trade to the Closed phase.

It is expected that the Custodian will burn if the Beneficiary has not met the terms, and release otherwise.

### *Closed* Phase

The Trade will never leave this state, and cannot undergo any state changes. However, it can still be used as a communication portal between the two parties, as the statement functionality is still allowed (see below, **Statements**).

In the Closed phase, there is also a `ClosedReason` variable, that describes how it was closed: via recall, abort, burn, or release.

## Phase Intervals, Auto* Behavior, and Poke()

Each functional, user-facing phase (Open, Committed, and Judgment) has a timeout feature, where a default action is "triggered" after some interval. The length of these intervals are set by the Initiator when the trade is created.

- The Open phase defaults in a *recall* after `autorecallInterval` seconds..
- The Committed phase defaults in an *abort* after `autoabortInterval` seconds.
- The Judgment phase defaults in a *release* after `autoreleaseInterval` seconds.

Of course, an Ethereum smart contract can't actually trigger itself after some delay, so we have to work around this limitation.

First, after an auto\*Interval has passed, all functions other than the default action are prevented from being called. This at least makes the contract game theoretically similar to if the default action was actually triggered. For example, after autorecallInterval in the Open phase, even though the Trade has not been technically recalled yet, the offer has functionally expired; no one could then commit and become the responder.

Second, we expose a `poke()` function to any msg.sender. This function checks if any default behavior is due, and if so, calls the relevant internal function. Additionally, if calling `poke()` did result in a state change, the caller is granted `pokeReward` DAI. `pokeReward` is a custom-set fee from the Initiator, designed to incentivize such poking behavior, thus simulating the contract automatically triggering itself.

An Initiator who doesn't care about "automatically" triggering the default action can opt to set `pokeReward` to 0. Also note that a simple server could send low-gas `poke` commands to all trades needing a poke, as an added service.

## Statements

Each party can make a statement by calling `initiatorStatement` or `responderStatement`, respectively. Like `terms`, these are just logged in an event and not stored in state, and are for communication purposes.

Used in conjunction with the `initiatorCommPubkey` and `responderCommPubkey` emitted in earlier events, these statements can support public-key encryption.

Because each statement costs gas to execute, it is expected that users of a long-term trade will use this as a jumping-off point to move to another means of communication.