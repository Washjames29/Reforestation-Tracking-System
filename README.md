# Reforestation Tracking System Smart Contract

A Clarity smart contract for tracking and rewarding community tree planting initiatives on the Stacks blockchain.

## Features

- Register tree planting projects with location and number of trees
- Verification system for planted trees
- Automated reward distribution for verified projects
- Community statistics tracking
- Total reforestation impact monitoring

## Contract Functions

### Public Functions

1. `register-project`: Register a new tree planting project
   - Parameters: location (string-ascii), trees-count (uint)
   - Returns: project-id

2. `verify-project`: Verify a registered project (owner only)
   - Parameters: project-id (uint)
   - Returns: boolean

3. `claim-reward`: Claim rewards for verified projects
   - Parameters: project-id (uint)
   - Returns: reward amount

### Read-Only Functions

1. `get-project`: Get project details
2. `get-community-stats`: Get community statistics
3. `get-total-trees`: Get total trees planted
4. `get-total-projects`: Get total registered projects

## Usage

1. Deploy the contract using Clarinet
2. Communities can register projects using `register-project`
3. Contract owner verifies legitimate projects
4. Communities claim rewards for verified projects

## Reward System

- 10 tokens per verified tree planted
- Rewards can only be claimed once per project
- Projects must be verified before claiming rewards