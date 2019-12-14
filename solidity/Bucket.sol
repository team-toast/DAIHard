pragma solidity 0.5.11;

import "./SafeMath.sol";
import "./Erc20.sol";

contract BucketSale
{
    using SafeMath for uint256;

    uint public HUNDRED_PERC = 100000;

    struct Buy
    {
        uint valueEntered;
        uint tokensExited;
        address referralAddress;
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
    uint public endOfSale;
    uint public bucketPeriod;
    uint public bucketSupply;
    Erc20 public tokenOnSale;
    Erc20 public tokenSoldFor;

    constructor (
            uint _bucketPeriod,
            uint _bucketSupply,
            Erc20 _tokeOnSale,      // SUGR in our case
            Erc20 _tokenSoldFor)    // typically DAI
        public
    {
        owner = msg.sender;
        bucketPeriod = _bucketPeriod;
        bucketSupply = _bucketSupply;
        tokenOnSale = _tokeOnSale;
        tokenSoldFor = _tokenSoldFor;
    }

    modifier onlyOwner()
    {
        require(msg.sender == owner, "only owner");
        _;
    }

    function timestamp() public view returns (uint256 _now) { return block.timestamp; }

    function startSale(uint256 _startDateTime)
        public
        onlyOwner
    {
        startOfSale = _startDateTime;
    }

    function stopSale(uint _inputEndOfSale)
        public
        onlyOwner
    {
        require(_inputEndOfSale < timestamp(), "cannot be in the past");
        require(endOfSale == 0 || endOfSale < timestamp(), "cannot extend sale after end date has passed");
        endOfSale = _inputEndOfSale;
    }

    event Drained(address indexed _target, uint _amount);
    function drain(address _target)
        public
        onlyOwner
    {
        uint balance = tokenSoldFor.balanceOf(address(this));
        bool transferSuccess = tokenSoldFor.transfer(_target, balance);
        require(transferSuccess, "transfer failed");
        emit Drained(_target, balance);
    }

    function currentBucket()
        public
        view
        returns (uint)
    {
        if (endOfSale == 0 || endOfSale > timestamp())
        {
            return timestamp().sub(startOfSale).div(bucketPeriod);
        }
        else
        {
            return endOfSale.sub(startOfSale).div(bucketPeriod);
        }
    }

    event Entered(address indexed _buyer, uint256 _bucket, uint _amount);
    function enter(address _buyer, uint _amount, address _referralAddress)
        public
    {
        require(_amount > 0, "can't buy nothing");

        Buy storage buy = buys[currentBucket()][_buyer];
        buy.valueEntered = buy.valueEntered.add(_amount);

        Bucket storage bucket = buckets[currentBucket()];
        bucket.totalValueEntered = bucket.totalValueEntered.add(_amount);
        bucket.referralAddress = _referralAddress;
        referredTotal[_referralAddress] = referredTotal[_referralAddress].add(_amount);

        bool transferSuccess = tokenSoldFor.transferFrom(_buyer, address(this), _amount);
        require(transferSuccess, "transfer failed");

        emit Entered(_buyer, currentBucket(), _amount);
    }

    event Exited(address indexed _buyer, uint256 _bucket, uint _amount);
    function exit(address _buyer, uint _bucketID)
        public
    {
        require(
            timestamp() < endOfSale || _bucketID < currentBucket(),
            "can only exit from concluded buckets");

        Buy storage buyToWithdraw = buys[_bucketID][msg.sender];
        require(buyToWithdraw.valueEntered > 0, "can't take out if you didn't put in");
        require(buyToWithdraw.tokensExited == 0, "already withdrawn");

        Bucket storage bucket = buckets[_bucketID];
        uint baseAmount = bucketSupply.mul(buyToWithdraw.valueEntered).div(bucket.totalValueEntered);
        uint rewardAmount = baseAmount.mul().div(1000000);
        uint referralAmount = baseAmount.mul(referrerReferralRewardPerc(buy.referralAddress)).div(HUNDRED_PERC);

        buyToWithdraw.tokensExited = baseAmount + referralAmount;

        bool transferSuccess = tokenOnSale.transfer(msg.sender, buyToWithdraw.tokensExited);
        require(transferSuccess, "erc20 transfer failed");

        TODO: transfer referral reward to referrer

        emit Exited(_buyer, _bucketID, baseAmount);
    }

    function buyerReferralRewardPerc(address _referralAddress)
        public
        returns(uint)
    {
        return 1100000;
    }

    //perc is between 0 and 100k, so 3 decimal precision.
    function referrerReferralRewardPerc(address _referralAddress)
        public
        returns(uint)
        //comment
    {
        uint daiContributed = referredTotal[_referralAddress].div(1000000000000000000);
        uint multiplier = daiContributed + 10000;
        uint result = min(HUNDRED_PERC, multiplier);
        return result;
}