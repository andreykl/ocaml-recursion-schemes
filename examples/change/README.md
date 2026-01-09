# OCaml Recursion Schemes Example: Coin Change Problem

This project demonstrates the use of recursion schemes in OCaml to solve the classic coin change problem.

## Problem Description

Given a set of coin denominations (i.e. [50; 25; 10; 5; 1] cents), find the number of distinct ways to make change for a given amount. This is a classic combinatorial problem often used to demonstrate dynamic programming and recursion techniques.

### Example

For coin denominations [1, 5, 10, 25, 50] cents:

* There are 2 ways to make change for 5 cents: [5] and [1,1,1,1,1]
* There are 4 ways to make change for 10 cents: [10], [5,5], [5,1,1,1,1,1], and [1,1,1,1,1,1,1,1,1,1]

## Implementation Details

In this implementation, we use recursion schemes – a powerful functional programming technique that separates the recursive structure of data from the operations performed on it. Specifically, we use:

* Histomorphisms for computations that need access to previous results
* Attributes for caching intermediate results

