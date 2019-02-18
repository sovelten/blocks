# blocks

A very simple blockchain client

## Installation

```
$ stack setup
$ stack build
$ stack install
```

## Code Organization

The code is divided in three main sections

### Models

Where the main logic and data structures are.

#### Operation

The smallest building block for transactions

#### Transaction

A list of input/output operations

#### Block

A block containing a list of transactions, the predecessor hash and its own hash 

#### BlockChain

The blockchain is actually just a list of blocks ordered from initial block to last block.
This part of the code contains code to calculate the state of a blockchain.

#### Blockgraph

The blockgraph is a data structure maintaining all blocks that are added.
The data structure consists of a hash map where the key is the hash of the block and the value is a node containing the block itself and its height in the tree.

The height information is added to avoid computing it everytime it is needed. Since once a block is added it is never removed or changed, its height in the chain is always the same.

Also, a list of heads is maintained and updated accordingly when a block is added.
The heads represent endpoints of a chain.

### Commands

Contains the controllers, the input output logic of the application

### Main

The runnable application 

## Code complexity

It might be worthy to mention the expected complexity of the operations that the client supports

n : number of blocks
q : number of heads
h : height of blockchain

### Adding a block

Adding a block consists of:
- Finding the parent block - O(1) if the hash map does not have collisions
- Adding the new block to the hash map - O(1)
- Updating heads, involve finding the parent in the list of heads and replacing it or adding a new head - O(q)

### Querying heads

Since heads are precomputed each time a block is added, querying is O(1)

### Querying state

Querying state is not precomputed and is the most expensive of the operations.
It involves:

- Finding head of longest chain - O(q)
- Getting longest chain - O(h)
- Processing transactions - O(h), assuming each block has a similar amount of transactions and operations
