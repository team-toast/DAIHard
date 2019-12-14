pragma solidity 0.5.11;

import "./SafeMath.sol";
import "./Erc20.sol";

contract BucketSale
{
    using SafeMath for uint256;

    struct Buy
    {
        uint valueEntered;
        uint tokensExited;
        bool haveWithdrawn;
    }

    struct Bucket
    {
        uint totalValueEntered;
    }

    mapping (uint256 => Bucket) public buckets;
    mapping (uint256 => mapping (address => Buy)) public buys;

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
    function enter(address _buyer, uint _amount)
        public
    {
        require(_amount > 0, "can't buy nothing");

        Buy storage buy = buys[currentBucket()][_buyer];
        buy.valueEntered = buy.valueEntered.add(_amount);

        Bucket storage bucket = buckets[currentBucket()];
        bucket.totalValueEntered = bucket.totalValueEntered.add(_amount);

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
        require(!buyToWithdraw.haveWithdrawn, "already withdrawn");

        Bucket storage bucket = buckets[_bucketID];
        uint amountToWithdraw = bucketSupply.div(bucket.totalValueEntered).mul(buyToWithdraw.valueEntered);
        buyToWithdraw.haveWithdrawn = true;

        bool transferSuccess = tokenOnSale.transfer(msg.sender, amountToWithdraw);
        require(transferSuccess, "erc20 transfer failed");

        emit Exited(_buyer, _bucketID, amountToWithdraw);
    }
}