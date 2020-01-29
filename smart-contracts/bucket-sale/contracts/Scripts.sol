//
// Attention Auditor:
// We consider this contract outside of the audit scope, given that it
// interacts with the BucketSale contract as an external user and is
// fundamentally limited in the same ways.
//

pragma solidity ^0.5.11;

import "./Bucket.sol";

contract Scripts {
    using SafeMath for uint;

    function getExitInfo(BucketSale _bucketSale, address _buyer)
        public
        view
        returns (uint[1253] memory) // Max 7-hour buckets in 1 year = 1252. Add 1 for the first element, which will be the total exitable tokens.
    {
        // goal:
        // 1. return the total FRY the buyer can extract
        // 2. return the bucketIds of each bucket they can extract from

        // logic:
        // *loop over all concluded buckets
        //   *check the .buys for this _buyer
        //   *if there is a buy
        //      *add to the first array element
        //      *add the bucketId to the array

        
        uint[1253] memory results;
        uint pointer = 0;
        for (uint bucketId = 0; bucketId < _bucketSale.currentBucket(); bucketId = bucketId.add(1))
        {
            (uint valueEntered, uint buyerTokensExited) = _bucketSale.buys(bucketId, _buyer);

            if (valueEntered > 0 && buyerTokensExited == 0) {
                // update the running total for this buyer
                results[0] = results[0]
                    .add(_bucketSale.calculateExitableTokens(bucketId, _buyer));

                // append the bucketId to the results array
                pointer = pointer.add(1);
                results[pointer] = bucketId;
            }
        }

        return results;
    }

    function exitMany(
            BucketSale _bucketSale,
            address _buyer,
            uint[] memory bucketIds)
        public
    {
        for (uint bucketIdIter = 0; bucketIdIter < bucketIds.length; bucketIdIter = bucketIdIter.add(1))
        {
            _bucketSale.exit(bucketIds[bucketIdIter], _buyer);
        }
    }
}
