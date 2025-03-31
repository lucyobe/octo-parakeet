# Tokenized Data Transaction

## Overview
The **Tokenized Data Transaction** smart contract facilitates secure transactions involving tokenized data. It includes features for staking, escrow-based transactions, and reward-based incentives for stakeholders. The contract is built using Clarity and operates on a blockchain platform, ensuring transparency and immutability.

## Features
- **Fungible Token Integration:** Implements a fungible token (`data-token`) for transactions.
- **Staking Mechanism:** Users can stake tokens and receive annual rewards.
- **Escrow Transactions:** A secure escrow system ensures fair transactions between buyers and sellers.
- **Permissioned Minting:** Only the contract owner can mint new tokens.
- **Transfer Functionality:** Secure peer-to-peer transfer of tokens.

## Contract Constants
- `contract-owner`: The account that deploys the contract.
- `err-owner-only`: Error code (100) for unauthorized access.
- `err-insufficient-balance`: Error code (101) for insufficient balance.
- `err-invalid-escrow`: Error code (102) for invalid escrow transactions.
- `staking-reward-rate`: Set to 5% annual reward.

## Data Structures
### 1. Staking
A `stakes` map tracks the staker’s principal, staked amount, and the block at which staking started.
```clarity
(define-map stakes
    { staker: principal }
    { amount: uint, start-block: uint })
```
### 2. Escrow Transactions
An `escrows` map maintains information about pending and completed transactions.
```clarity
(define-map escrows
    { id: uint }
    { seller: principal,
      buyer: principal,
      amount: uint,
      data-hash: (buff 32),
      status: (string-ascii 20) })
```
- `escrow-nonce`: Tracks escrow transaction count.

## Public Functions
### 1. Token Management
- `mint(amount, recipient)`: Mints `amount` tokens to `recipient` (only owner).
- `transfer(amount, sender, recipient)`: Transfers `amount` from `sender` to `recipient`.

### 2. Staking Functions
- `stake(amount)`: Locks `amount` of tokens in staking.
- `unstake()`: Unstakes tokens and earns rewards based on staking duration.

### 3. Escrow Transactions
- `create-escrow(amount, data-hash, seller)`: Initiates an escrow between buyer and seller.
- `complete-escrow(escrow-id)`: Finalizes an escrow transaction and releases tokens to the seller.

## Read-Only Functions
- `get-stake-info(staker)`: Returns the stake details of a `staker`.
- `get-escrow(escrow-id)`: Retrieves escrow details.
- `get-balance(account)`: Returns `data-token` balance of `account`.

## Private Functions
- `calculate-reward(amount, start-block)`: Computes staking rewards based on duration and rate.

## How It Works
1. **Token Minting:** The contract owner mints tokens to distribute.
2. **Staking:** Users stake tokens and earn rewards over time.
3. **Escrow Transactions:** Buyers deposit tokens in escrow, which are released to sellers upon completion.
4. **Unstaking:** Users can withdraw staked tokens along with accumulated rewards.

## Security Considerations
- **Access Control:** Only the contract owner can mint tokens.
- **Safe Transfers:** Token transactions are validated for sender authorization.
- **Escrow Protection:** Ensures fair transactions between parties.

## Conclusion
The **Tokenized Data Transaction** contract provides a robust framework for secure, trustless data transactions and staking incentives. It ensures fair dealing between parties while incentivizing long-term participation through staking rewards.

