# OCaml Recursion Schemes Example: Coin Change Problem

This project demonstrates the use of recursion schemes in OCaml to solve the classic coin change problem.

## Problem Description

Given a set of coin denominations (i.e. [50; 25; 10; 5; 1] cents), find the number of distinct ways to make change for a given amount. This is a classic combinatorial problem often used to demonstrate dynamic programming and recursion techniques.

### Example

For coin denominations [1, 5, 10, 25, 50] cents:

* There are 2 ways to make change for 5 cents: [5] and [1,1,1,1,1]
* There are 4 ways to make change for 10 cents: [10], [5,5], [5,1,1,1,1,1], and [1,1,1,1,1,1,1,1,1,1]

## Building and Running

To build the library and the example, run (from the project root):

```bash

$ opam install --deps-only .
$ dune build

```

To run the example (from the current directory):

```bash

$ dune exec ./change.exe

```


To run the example (from the project root):

```bash

$ dune exec ./examples/change/change.exe

```

## Implementation Details

In this implementation, we use recursion schemes – a powerful functional programming technique that separates the recursive structure of data from the operations performed on it. Specifically, we use:

* Histomorphisms for computations that need access to previous results
* Attributes for caching intermediate results

### Core Data Structure

The algebra `go` returns a three-element tuple `(given, n, res)` containing change-making information for amount `given`:

- **`given`**: The amount to make change for (in cents)
- **`n`**: Number of ways to make change using the full coin set
- **`res`**: List of pairs `(ncoins, nways)` where each pair stores the number of ways `nways` to make change for amount `given` using exactly `ncoins` coin types

### Algorithm Flow

1. **Initialization**: Convert the target amount into a standard recursive structure for natural numbers
2. **Bottom-up traversal**: Process amounts from smallest to largest using a histomorphism (fold with cached results)
3. **Base case**: There is exactly 1 way to make change for 0 cents (using no coins)
4. **Recursive step**: At each step, cached results from previous amounts are available. Thus, we proceed from the smallest amount upwards, always knowing how to make change for all previous amounts.

### Step Processing

For each amount, we process coins from smallest to largest denomination:

- **Single coin**: Only 1 way to make change using one coin type
- **Multiple coins**: To compute ways for `n` coin types:
  - Ways using all `n` coins = Ways using `n-1` coins + Ways for amount `(given - largest_coin)` using `n` coins

This follows the recursive relation:

```text

change [10; 5; 1] 16 = change [10; 5; 1] 6 + change [5; 1] 16

```

## Implementation Notes

The algorithm differs from naive recursion by maintaining a cache of precomputed results. For detailed implementation, see the comments in `lib/change.ml`.

## Lean More

This example demonstrates recursion schemes – a powerful technique for handling recursive structures. For further exploration:

### Recommended Resources

| Resource | Description |
|----------|-------------|
| [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire](https://maartenfokkinga.github.io/utwente/mmf91m.pdf) | The seminal paper introducing recursion schemes |
| [Practical Recursion Schemes](https://blog.sumtypeofway.com) | Excellent tutorial series by Patrick Thomson |
| [recursion-schemes Haskell package](https://hackage.haskell.org/package/recursion-schemes) | The canonical implementation |
| [OCaml Recursion Schemes](https://github.com/ocamllabs/ocaml-recursion-schemes) | OCaml-specific implementations |
