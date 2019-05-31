This is a short document intended for potential auditors, that collects all of our security analysis and documentation, and describes specifically what we are looking for in an audit.

To get acquainted with the contract and its intended behavior, see these two documents:
- [Medium post on the game theory](https://medium.com/@coinop.logan/daihard-game-theory-21a456ef224e)
- [High-level, thorough description of all intended functionality in the contracts](high_level_feature_spec.md)

We are specifically looking for an audit of the code and internal logic of the DAIHardFactory and the DAIHardTrade seen [here](https://github.com/burnable-tech/DAIHard/blob/33b5e15163001f0a5b75244308c915d5d5c7eaf4/solidity/DAIHard.sol).

We **do not include** the game theory in the scope of this audit. While we welcome criticism and questions on this front, we would rather keep the scope of the paid audit limited to where it is most sorely needed: overview of the code and internal logic.

We've generated a low-level spec sheet and automated tests for crucial functions. See [this](spec_sheet_and_tests.md) for more on this.

### Assumptions

- autorecallInterval, autoabortInterval, and autoreleaseInterval will each never be set to less than 5 minutes.
- We ignore any Trade that has not been created by the Factory.