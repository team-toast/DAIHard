pragma solidity ^0.5.11;

import "../../common/SafeMath.sol";
import "../../common/ERC20Interface.sol";

contract BucketSale
{
    using SafeMath for uint256;

    uint constant HUNDRED_PERC = 100000;
    uint constant ONE_PERC = 1000;

    struct Buy
    {
        uint valueEntered;
        uint buyerTokensExited;
    }

    struct Bucket
    {
        uint totalValueEntered;
    }

    mapping (uint => Bucket) public buckets;
    mapping (uint => mapping (address => Buy)) public buys;
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

    // used to act as the contract and move things sent to the contract
    event Forwarded(
        address _to,
        bytes _data,
        uint _wei,
        bool _success,
        bytes _resultData);
    function forward(address _to, bytes memory _data, uint _wei)
        public
        onlyOwner
    {
        (bool success, bytes memory resultData) = _to.call.value(_wei)(_data);
        emit Forwarded(_to, _data, _wei, success, resultData);
    }

    function currentBucket()
        public
        view
        returns (uint)
    {
        return timestamp().sub(startOfSale).div(bucketPeriod);
    }

    event Entered(
        uint256 _bucketId,
        address indexed _buyer,
        uint _valueEntered,
        uint _buyerReferralReward,
        address indexed _referrer,
        uint _referrerReferralReward);
    function enter(uint _bucketId, uint _amount, address _referrer)
        public
    {
        registerEnter(_bucketId, msg.sender, _amount);
        referredTotal[_referrer] = referredTotal[_referrer].add(_amount);
        bool transferSuccess = tokenSoldFor.transferFrom(msg.sender, address(this), _amount);
        require(transferSuccess, "enter transfer failed");

        uint buyerReferralReward = _amount.mul(buyerReferralRewardPerc(_referrer)).div(HUNDRED_PERC);
        uint referrerReferralReward = _amount.mul(referrerReferralRewardPerc(_referrer)).div(HUNDRED_PERC);

        registerEnter(_bucketId.add(1), msg.sender, buyerReferralReward);
        registerEnter(_bucketId.add(1), _referrer, referrerReferralReward);

        emit Entered(
            _bucketId,
            msg.sender,
            _amount,
            buyerReferralReward,
            _referrer,
            referrerReferralReward);
    }

    function registerEnter(uint _bucketId, address _buyer, uint _amount)
        internal
    {
        require(_bucketId >= currentBucket(), "cannot enter past buckets");
        require(_bucketId < bucketCount, "invalid bucket id--past end of sale");
        require(_amount > 0, "can't buy nothing");
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
    function exit(address _buyer, uint _bucketId)
        public
    {
        require(
            _bucketId < currentBucket(),
            "can only exit from concluded buckets");

        Buy storage buyToWithdraw = buys[_bucketId][_buyer];
        require(buyToWithdraw.valueEntered > 0, "can't take out if you didn't put in");
        require(buyToWithdraw.buyerTokensExited == 0, "already withdrawn");

        Bucket storage bucket = buckets[_bucketId];
        buyToWithdraw.buyerTokensExited = bucketSupply.mul(buyToWithdraw.valueEntered).div(bucket.totalValueEntered);
        totalExitedTokens = totalExitedTokens.add(buyToWithdraw.buyerTokensExited);

        bool transferSuccess = tokenOnSale.transfer(_buyer, buyToWithdraw.buyerTokensExited);
        require(transferSuccess, "exit transfer failed");

        emit Exited(
            _bucketId,
            _buyer,
            buyToWithdraw.buyerTokensExited);
    }

    function buyerReferralRewardPerc(address _referrerAddress)
        public
        pure
        returns(uint)
    {
        return _referrerAddress == address(0) ? 0 : ONE_PERC.mul(10);
    }

    //perc is between 0 and 100k, so 3 decimal precision.
    function referrerReferralRewardPerc(address _referrerAddress)
        public
        view
        returns(uint)
    {
        uint daiContributed = referredTotal[_referrerAddress].div(10 ** 18);
        uint multiplier = daiContributed.add(ONE_PERC.mul(10)); // this guarentees every referrer gets at least 10% of what the buyer is buying
        uint result = SafeMath.min(HUNDRED_PERC, multiplier);
        return result;
    }
}