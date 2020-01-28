pragma solidity ^0.5.11;

import "../../common/SafeMath.sol";
import "../../common/ERC20Interface.sol";

contract BucketSale
{
    using SafeMath for uint256;

    // When passing around bonuses, we use 3 decimals of precision.
    uint constant HUNDRED_PERC = 100000;
    uint constant ONE_PERC = 1000;

    /*
    Every pair of (uint bucketId, address buyer) identifies exactly one 'buy'.
    This buy tracks the total value entered by the user (of the tokens the sale accepts),
    and the total value exited (of the tokens being sold).
    */

    struct Buy
    {
        uint valueEntered;
        uint buyerTokensExited;
    }

    /*
    Each Bucket tracks the total value entered (of the tokens the sale accepts);
    this is used to determine what proportion of the tokens on sale the user can later exit with.
    */

    struct Bucket
    {
        uint totalValueEntered;
    }

    mapping (uint => Bucket) public buckets;
    mapping (uint => mapping (address => Buy)) public buys;

    // For each address, this tallies how much value (of the tokens the sale accepts) the user has referred.
    mapping (address => uint) public referredTotal;

    address public owner;
    uint public startOfSale;
    uint public bucketPeriod;
    uint public bucketSupply;
    uint public bucketCount;
    uint public totalExitedTokens;
    ERC20Interface public tokenOnSale;
    ERC20Interface public tokenSoldFor;

    constructor (
            address _owner,
            uint _startOfSale,
            uint _bucketPeriod,
            uint _bucketSupply,
            uint _bucketCount,
            ERC20Interface _tokenOnSale,      // FRY in our case
            ERC20Interface _tokenSoldFor)     // typically DAI
        public
    {
        owner = _owner;
        startOfSale = _startOfSale;
        bucketPeriod = _bucketPeriod;
        bucketSupply = _bucketSupply;
        bucketCount = _bucketCount;
        tokenOnSale = _tokenOnSale;
        tokenSoldFor = _tokenSoldFor;
    }

    modifier onlyOwner()
    {
        require(msg.sender == owner, "only owner");
        _;
    }

    function timestamp() public view returns (uint256 _now) { return block.timestamp; }

    /*
    Allows the owner to execute any transaction in the sale's name. This is the only 'onlyOwner' method.
    This will be used to send away the tokens accumulated by the sale, but can be used for general transactions as well.
    */
    event Forwarded(
        address _to,
        bytes _data,
        uint _wei,
        bool _success,
        bytes _resultData);
    function forward(address _to, bytes memory _data, uint _wei)
        public
        onlyOwner
        returns (bool, bytes memory)
    {
        (bool success, bytes memory resultData) = _to.call.value(_wei)(_data);
        emit Forwarded(_to, _data, _wei, success, resultData);
        return (success, resultData);
    }

    function currentBucket()
        public
        view
        returns (uint)
    {
        return timestamp().sub(startOfSale).div(bucketPeriod);
    }

    event Entered(
        address _sender,
        uint256 _bucketId,
        address indexed _buyer,
        uint _valueEntered,
        uint _buyerReferralReward,
        address indexed _referrer,
        uint _referrerReferralReward);
    function enter(
            address _buyer,
            uint _bucketId,
            uint _amount,
            address _referrer)
        public
    {
        registerEnter(_bucketId, _buyer, _amount);
        referredTotal[_referrer] = referredTotal[_referrer].add(_amount);
        bool transferSuccess = tokenSoldFor.transferFrom(msg.sender, address(this), _amount);
        require(transferSuccess, "enter transfer failed");

        if (_referrer != address(0)) // If there is a referrer
        {
            uint buyerReferralReward = _amount.mul(buyerReferralRewardPerc()).div(HUNDRED_PERC);
            uint referrerReferralReward = _amount.mul(referrerReferralRewardPerc(_referrer)).div(HUNDRED_PERC);

            // Both rewards are registered as buys in the next bucket
            registerEnter(_bucketId.add(1), _buyer, buyerReferralReward);
            registerEnter(_bucketId.add(1), _referrer, referrerReferralReward);

            emit Entered(
                msg.sender,
                _bucketId,
                _buyer,
                _amount,
                buyerReferralReward,
                _referrer,
                referrerReferralReward);
        }
        else
        {
            emit Entered(
                msg.sender,
                _bucketId,
                _buyer,
                _amount,
                0,
                address(0),
                0);
        }
    }

    function registerEnter(uint _bucketId, address _buyer, uint _amount)
        internal
    {
        require(_bucketId >= currentBucket(), "cannot enter past buckets");
        require(_bucketId < bucketCount, "invalid bucket id--past end of sale");
        require(_amount > 0, "can't buy nothing");

        // If at any point the sale cannot support all planned buckets, prevent all entry for any bucket.
        require(
            tokenOnSale.balanceOf(address(this)).add(totalExitedTokens) >= _bucketId.add(1).mul(bucketSupply),
            "insufficient tokens to sell");

        Buy storage buy = buys[_bucketId][_buyer];
        buy.valueEntered = buy.valueEntered.add(_amount);

        Bucket storage bucket = buckets[_bucketId];
        bucket.totalValueEntered = bucket.totalValueEntered.add(_amount);
    }

    event Exited(
        uint256 _bucketId,
        address indexed _buyer,
        uint _tokensExited);
    function exit(uint _bucketId, address _buyer)
        public
    {
        require(
            _bucketId < currentBucket(),
            "can only exit from concluded buckets");

        Buy storage buyToWithdraw = buys[_bucketId][_buyer];
        require(buyToWithdraw.valueEntered > 0, "can't take out if you didn't put in");
        require(buyToWithdraw.buyerTokensExited == 0, "already withdrawn");

        /*
        Note that buyToWithdraw.buyerTokensExited serves a dual purpose:
        First, it is always set to a non-zero value when a buy has been exited from,
        and checked in the line above to guard against repeated exits.
        Second, it's used as simple record-keeping for future analysis; hence the use of uint
        rather than something like bool buyerTokensHaveExited.
        */

        buyToWithdraw.buyerTokensExited = calculateExitableTokens(_bucketId, _buyer);
        totalExitedTokens = totalExitedTokens.add(buyToWithdraw.buyerTokensExited);

        bool transferSuccess = tokenOnSale.transfer(_buyer, buyToWithdraw.buyerTokensExited);
        require(transferSuccess, "exit transfer failed");

        emit Exited(
            _bucketId,
            _buyer,
            buyToWithdraw.buyerTokensExited);
    }

    function buyerReferralRewardPerc()
        public
        pure
        returns(uint)
    {
        return ONE_PERC.mul(10);
    }

    function referrerReferralRewardPerc(address _referrerAddress)
        public
        view
        returns(uint)
    {
        if (_referrerAddress == address(0))
        {
            return 0;
        }
        else
        {
            // integer number of dai contributed
            uint daiContributed = referredTotal[_referrerAddress].div(10 ** 18);

            /*
            A more explicit way to do the following 'uint multiplier' line would be something like:

            float bonusFromDaiContributed = daiContributed / 100000.0;
            float multiplier = bonusFromDaiContributed + 0.1;

            However, because we are already using 3 digits of precision for bonus values,
            the integer amount of Dai happens to exactly equal the bonusPercent value we want
            (i.e. 10,000 Dai == 10000 == 10*ONE_PERC)
            */
            uint multiplier = daiContributed.add(ONE_PERC.mul(10)); // this guarentees every referrer gets at least 10% of what the buyer is buying

            uint result = SafeMath.min(HUNDRED_PERC, multiplier); // Cap it at 100% bonus
            return result;
        }
    }

    function calculateExitableTokens(uint _bucketId, address _buyer)
        public
        view
        returns(uint)
    {
        Bucket storage bucket = buckets[_bucketId];
        Buy storage buyToWithdraw = buys[_bucketId][_buyer];
        return bucketSupply
            .mul(buyToWithdraw.valueEntered)
            .div(bucket.totalValueEntered);
    }
}