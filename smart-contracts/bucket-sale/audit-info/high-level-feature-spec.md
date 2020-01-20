High Level Description of Bucket.sol
===

### Buckets

The purpose of this smart contract is to sell `tokenOnSale` (typically a newly minted token) for some other `tokenSoldFor` (typically an established token, such as Dai or ETH) in a series of "buckets". Team Toast will be using this mechanism to sell FRY for Dai, but we have written the contract such that it should work with any pair of ERC20 tokens, and we request the scope of the audit include this assumption.

Each bucket lasts `bucketPeriod` seconds, set in the constructor and never changed thereafter. For any bucket that is active or in the future, any number of agents can **enter** the bucket by depositing any amount of `tokenSoldFor`. This can be done multiple times to add to the total number of entered tokens on any given bucket. For any bucket in the past, any agent who had entered the bucket can **exit** the bucket, which disburses some number of `tokenOnSale`. The amount of `tokenOnSale` the agent receives is proportional to how much `tokenSoldFor` in total he entered into that bucket, divided by the total number of `tokenSoldFor` was deposited by him and any other agents. For example, if Alice **enter**s a bucket with 10 `tokenSoldFor` and Bob enters with 90, and `bucketSupply` is 1000 `tokenOnSale`, then Alice can exit that bucket to claim 100 `tokenOnSale` while Bob can exit to claim 900.

The bucket sale is designed to end after `bucketCount` buckets. The owner is expected to fund the sale with enough `tokenOnSale` upon instantiation such that the contract's balance of `tokenOnSale` >= `bucketSupply * bucketCount`.

### Referrals

The **enter** method takes an optional referrer value (optional meaning that 0x0 indicates no referrer). This gives a bonus to the buyer as well as to the referrer, and the referrer's bonus scales with the total amount of `tokenSoldFor` they are responsible for referring. This is expected to be used in the context of an interface that tracks referral data and passes it along silently to the **enter** call.

This bonus takes the form of a "free" entry into the next bucket, proportional to the amount the buyer is entering into the target bucket. The public **enter** function does this by making 3 separate calls to the internal **registerEnter** call: first for the initial "normal" enter, second for the buyer's bonus (10% of the amount they are entering with), and third for the referrer's bonus (10%-100% of the amount the buyer is entering with). The referrer's bonus starts at 10% and increases 1% for every 1000 `tokenSoldFor` the referrer has referred in the past, and caps out at 100%.

Note that since `registerEnter` reverts on an invalid bucket, calling `enter` with a referrer on the last bucket valid will also revert, as it will not be able to register the bonuses.

### Owner Can "Break" The Sale

The only `onlyOwner` method is **forward**, which allows the owner to execute arbitrary transactions as the sale. This is primarily intended to be used to recover the accumulated `tokenSoldFor` by forwarding ERC20 transfer calls, but also allows the owner to "refund" any ERC20 tokens erroneously sent to the sale.

It is worth noting that the owner could use this mechanism to pull out any amount of `tokenOnSale`, which could cause the sale to become essentially insolvent, as there could no longer be enough `tokenOnSale` to cover all buckets. In this case, due to the require on line WHAT LINE, the sale would immediately stop accepting any **enter** call, but would continue to accept **exit** calls. This may lead to a situation where the last users to try to exit cannot exit, as the sale has run out of `tokenOnSale` while there are still un-processed exits.