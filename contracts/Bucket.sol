// solium-disable linebreak-style
pragma solidity 0.5.11;

import "./SafeMath.sol";
import "./ERC20Interface.sol";

contract BucketSale
{
    function min(uint a, uint b) public pure returns (uint) {
        if (a < b) return a;
        else return b;
    }

    using SafeMath for uint256;

    uint public HUNDRED_PERC = 100000;

    struct Buy
    {
        uint valueEntered;
        uint tokensDisbursed;
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
    ERC20Interface public tokenOnSale;
    ERC20Interface public tokenSoldFor;

    constructor (
            uint _bucketPeriod,
            uint _bucketSupply,
            ERC20Interface _tokeOnSale,      // SUGR in our case
            ERC20Interface _tokenSoldFor)    // typically DAI
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
        require(_startDateTime >= timestamp(), "Can't start the sale in the past");
        require(startOfSale == 0, "Sale start has already been set");
        startOfSale = _startDateTime;
    }
    
    function startSaleNow()
        public
        onlyOwner
    {
        startSale(timestamp());
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
        require(startOfSale != 0, "No start time for sale");
        require(startOfSale <= timestamp(), "sale not yet started");
        
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
        buy.referralAddress = _referralAddress;

        Bucket storage bucket = buckets[currentBucket()];
        bucket.totalValueEntered = bucket.totalValueEntered.add(_amount);
        
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
        require(buyToWithdraw.tokensDisbursed == 0, "already withdrawn");

        Bucket storage bucket = buckets[_bucketID];
        uint baseAmount = bucketSupply.mul(buyToWithdraw.valueEntered).div(bucket.totalValueEntered);

        uint msgsenderReferralBonus;
        uint referrerReward;
        if (buyToWithdraw.referralAddress == address(0x0)) {
            msgsenderReferralBonus = referrerReward = 0;
        }
        else {
            msgsenderReferralBonus = baseAmount.mul(110000).div(100000);
            referrerReward = baseAmount.mul(referrerRewardPerc(buyToWithdraw.referralAddress)).div(HUNDRED_PERC);
        }

        buyToWithdraw.tokensDisbursed = baseAmount + msgsenderReferralBonus + referrerReward;

        bool transferSuccess = tokenOnSale.transfer(msg.sender, baseAmount + msgsenderReferralBonus);
        require(transferSuccess, "erc20 transfer failed");

        if (buyToWithdraw.referralAddress != address(0x0)) {
            transferSuccess = tokenOnSale.transfer(buyToWithdraw.referralAddress, referrerReward);
            require(transferSuccess, "erc20 transfer to referrer failed");
        }

        emit Exited(_buyer, _bucketID, baseAmount);
    }

    //perc is between 0 and 100k, so 3 decimal precision.
    function referrerRewardPerc(address _referralAddress)
        public
        view
        returns(uint)
    {
        uint daiReferredTotal = referredTotal[_referralAddress].div(1000000000000000000);
        uint multiplier = daiReferredTotal + 10000;
        uint result = min(HUNDRED_PERC, multiplier);
        return result;
    }
}