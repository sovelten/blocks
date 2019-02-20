# blocks

A very simple blockchain client implemented in Haskell

## Installation

The code is built using Haskell Tool Stack

```
$ stack setup
$ stack build
$ stack install
```

## Runnning

```
$ stack exec blocks-exe
```

## Runnning tests

Some unit tests are written as documentation using haskell-doctest. There is also one generative test for testing consistency of state update when adding a block.

They can be run with:

```
$ stack test
```

## Documentation

The documentation on the code can be rendered into a html document using:

```
$ stack haddock
```

## Code Organization

The code is divided in three main sections

### Models

Where the main logic and data structures are.

#### Operation

The smallest building block for transactions

#### Transaction

A list of input/output operations

Some assumptions were made on how to properly execute a transaction.

There was no mention that the code should check that the transactions in a block being added actually have inputs to consume to be considered valid, but it seemed to make sense that transaction inputs need to match some unspent output from the blockchain. If transactions are somehow invalid, the block is not added with error `{"error": "invalid transaction inputs"}`

#### Block

A block containing a list of transactions, the predecessor hash and its own hash 

#### ChainState

State is the data structure responsible for storing a chain or sub-chain state, containing height, block hash and unspent outputs.

#### Blockgraph

The blockgraph is a data structure maintaining all blocks that are added.
The data structure consists of a hash map where the key is the hash of the block and the value is a node containing the block itself and the state at this point in the chain.

The state information is added to avoid computing it everytime it is needed.
Since once a block is added it is never removed or changed, its state in the chain is always the same, so it makes sense that it should be calculated only once.

Also, a list of heads is maintained and updated accordingly when a block is added.
The heads represent endpoints of a chain.

### Commands

Contains the controllers, the input output logic of the application

### Main

The runnable application 

## Code complexity

It might be worthy to mention the expected complexity of the operations that the client supports

h : number of heads
o : number of unspent outputs
p : number of operations in a block

### Adding a block

Adding a block consists of:
- Finding the parent node - O(1) if the hash map does not have collisions
- Adding the new block to the hash map - O(1)
- Calculating state from previous state - O(o+p)
- Updating heads, involve finding the parent in the list of heads and replacing it or adding a new head - O(h)

### Querying heads

Since heads are precomputed each time a block is added, querying is O(1)

### Querying state

State is also precomputed and querying it should be as simple as getting the longest head - O(h)
