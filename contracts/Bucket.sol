pragma solidity 0.5.11;

import "./SafeMath.sol";
import "./Erc20.sol";

contract BucketSale
{
    using SafeMath for uint256;

    uint constant HUNDRED_PERC = 100000;
    uint constant ONE_PERC = 1000;

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
    uint public bucketPeriod;
    uint public bucketSupply;
    Erc20 public tokenOnSale;
    Erc20 public tokenSoldFor;

    constructor (
            uint _startOfSale,
            uint _bucketPeriod,
            uint _bucketSupply,
            Erc20 _tokenOnSale,      // SUGR in our case
            Erc20 _tokenSoldFor)    // typically DAI
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

    event Entered(address indexed _buyer, uint256 _bucket, uint _amount);
    function enter(address _buyer, uint _amount, address _referralAddress)
        public
    {
        require(_amount > 0, "can't buy nothing");
        require(tokenOnSale.balanceOf(address(this)) >= bucketSupply.mul(21).div(10), "insufficient tokens to sell");
        //TODO: The above line is not robust; it assumes all previous buckets have already disbursed all they can.

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
            _bucketID < currentBucket(),
            "can only exit from concluded buckets");

        Buy storage buyToWithdraw = buys[_bucketID][msg.sender];
        require(buyToWithdraw.valueEntered > 0, "can't take out if you didn't put in");
        require(buyToWithdraw.tokensExited == 0, "already withdrawn");

        Bucket storage bucket = buckets[_bucketID];
        uint baseAmount = bucketSupply.mul(buyToWithdraw.valueEntered).div(bucket.totalValueEntered);
        uint rewardAmount = baseAmount.mul(buyerReferralRewardPerc()).div(HUNDRED_PERC);
        uint referralAmount = rewardAmount.mul(referrerReferralRewardPerc(buyToWithdraw.referralAddress)).div(HUNDRED_PERC);

        bool transferSuccess = tokenOnSale.transfer(msg.sender, baseAmount);
        require(transferSuccess, "erc20 base transfer failed");

        if (buyToWithdraw.referralAddress != address(0))
        {
            bool rewardTransferSuccess = tokenOnSale.transfer(buyToWithdraw.referralAddress, referralAmount);
            require(rewardTransferSuccess, "erc20 referral reward transfer failed");
        }

        emit Exited(_buyer, _bucketID, baseAmount);
    }

    function buyerReferralRewardPerc()
        public
        pure
        returns(uint)
    {
        return ONE_PERC.mul(11);
    }

    //perc is between 0 and 100k, so 3 decimal precision.
    function referrerReferralRewardPerc(address _referralAddress)
        public
        view
        returns(uint)
    {
        uint daiContributed = referredTotal[_referralAddress].div(1000000000000000000);
        uint multiplier = daiContributed.add(10 * ONE_PERC);
        uint result = SafeMath.min(HUNDRED_PERC, multiplier);
        return result;
    }
}