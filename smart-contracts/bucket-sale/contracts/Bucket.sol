pragma solidity ^0.5.6;

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
        uint referrerTokensExited;
        address referrerAddress;
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
    uint public totalExitedTokens;
    ERC20Interface public tokenOnSale;
    ERC20Interface public tokenSoldFor;

    constructor (
            uint _startOfSale,
            uint _bucketPeriod,
            uint _bucketSupply,
            ERC20Interface _tokenOnSale,      // SUGR in our case
            ERC20Interface _tokenSoldFor)    // typically DAI
        public
    {
        owner = msg.sender;
        startOfSale = _startOfSale;
        bucketPeriod = _bucketPeriod;
        bucketSupply = _bucketSupply;
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
    event Forwarded(address _to, bytes _data, uint _wei, bool _success, bytes _resultData);
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

    function actualAvailableSupply()
        public
        returns (uint)
    {

    }

    event Entered(
        uint256 _bucket,
        address indexed _buyer,
        address indexed _referrer,
        uint _valueEntered);
    function enter(address _buyer, uint _amount, address _referrerAddress)
        public
    {
        // todo: specify bucketId

        require(_amount > 0, "can't buy nothing");

        // todo:fix this to take real available supply into account
        require(tokenOnSale.balanceOf(address(this)) >= actualAvailableSupply(), "insufficient tokens to sell");

        Buy storage buy = buys[currentBucket()][_buyer];
        buy.valueEntered = buy.valueEntered.add(_amount);
        buy.referrerAddress = _referrerAddress;

        Bucket storage bucket = buckets[currentBucket()];
        bucket.totalValueEntered = bucket.totalValueEntered.add(_amount);
        referredTotal[_referrerAddress] = referredTotal[_referrerAddress].add(_amount);

        bool transferSuccess = tokenSoldFor.transferFrom(_buyer, address(this), _amount);
        require(transferSuccess, "transfer failed");

        emit Entered(currentBucket(), _buyer, _referrerAddress, _amount);
    }

    event Exited(
        uint256 _bucket,
        address indexed _buyer,
        uint _buyerAmount,
        address indexed _referrer,
        uint _referrerAmount);
    function exit(address _buyer, uint _bucketID)
        public
    {
        require(
            _bucketID < currentBucket(),
            "can only exit from concluded buckets");

        Buy storage buyToWithdraw = buys[_bucketID][_buyer];
        require(buyToWithdraw.valueEntered > 0, "can't take out if you didn't put in");
        require(buyToWithdraw.buyerTokensExited == 0, "already withdrawn");

        Bucket storage bucket = buckets[_bucketID];
        uint baseAmount = bucketSupply.mul(buyToWithdraw.valueEntered).div(bucket.totalValueEntered);
        uint rewardAmount = baseAmount.mul(buyerReferralRewardPerc(buyToWithdraw.referrerAddress)).div(HUNDRED_PERC);
        buyToWithdraw.buyerTokensExited = baseAmount.add(rewardAmount);

        bool transferSuccess = tokenOnSale.transfer(_buyer, buyToWithdraw.buyerTokensExited);
        require(transferSuccess, "erc20 buyer transfer failed");

        if (buyToWithdraw.referrerAddress != address(0))
        {
            buyToWithdraw.referrerTokensExited = baseAmount.mul(referrerReferralRewardPerc(buyToWithdraw.referrerAddress)).div(HUNDRED_PERC);
            bool rewardTransferSuccess = tokenOnSale.transfer(buyToWithdraw.referrerAddress, buyToWithdraw.referrerTokensExited);
            require(rewardTransferSuccess, "erc20 referrer transfer failed");
        }

        totalExitedTokens = totalExitedTokens
            .add(buyToWithdraw.buyerTokensExited)
            .add(buyToWithdraw.referrerTokensExited);

        emit Exited(
            _bucketID,
            _buyer,
            buyToWithdraw.buyerTokensExited,
            buyToWithdraw.referrerAddress,
            buyToWithdraw.referrerTokensExited);
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
        uint daiContributed = referredTotal[_referrerAddress].div(1000000000000000000);
        uint multiplier = daiContributed.add(ONE_PERC.mul(10)); // this guarentees every referrer gets at least 10% of what the buyer is buying
        uint result = SafeMath.min(HUNDRED_PERC, multiplier);
        return result;
    }
}