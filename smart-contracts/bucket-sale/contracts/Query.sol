//
// Attention Auditor:
// This contract does not affect state and is outside of the audit scope.
//

pragma solidity ^0.5.11;

import "./Bucket.sol";

contract Query {
    using SafeMath for uint256;

    function getExitInfo(BucketSale _bucketSale, address _buyer)
        public
        view
        returns (uint256[1252] memory)
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

        uint256[1400] memory results;
        uint256 pointer = 0;
        for (
            uint256 bucketId = 0;
            bucketId < _bucketSale.currentBucket();
            bucketId = bucketId.add(1)
        ) {
            (uint256 valueEntered, uint256 buyerTokensExited) = _bucketSale
                .buys(bucketId, _buyer);
            if (valueEntered > 0 && buyerTokensExited != 0) {
                // update the running total for this buyer
                results[0] = results[0].add(
                    _bucketSale.calculateExitableTokens(bucketId, _buyer)
                );

                // append the bucketId to the results array
                pointer = pointer.add(1);
                results[pointer] = bucketId;
            }
        }

        return results;
    }
}
