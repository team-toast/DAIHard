This is a short document intended for potential auditors, that collects all of our security analysis and documentation, and describes specifically what we are looking for in an audit.

To get acquainted with the contract and its intended behavior, take a look at these links:
- ANNOUNCE POST NEEDED
- [High-level description of all intended functionality in the contract](high-level-feature-spec.md)

The version of the contract to audit can be seen here(LINK NEEDED).

We've generated a low-level spec sheet and automated tests for crucial functions. LINKS NEEDED

### Assumptions

- The Sale is started with all the tokens on sale it will need to disburse throughout the sale - specifically, it will be given a balance of (bucketCount * bucketSupply) upon instantiation.