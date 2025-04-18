Given an array of integers e, arrange the elements in it so that:

e[0] ≤ e[1] ≥ e[2] ≤ e[3] ≥ e[4] ...

The elements in e need not be unique (they may be repeated)

5 2 1 7 9 8 ->  1 ≤ 7 ≥ 5 ≤ 9 ≥ 2 ≤ 8  or  2 ≤ 5 ≥ 1 ≤ 9 ≥ 7 ≤ 8 or ...

1 2 3 4 5 6 ->  1 ≤ 3 ≥ 2 ≤ 5 ≥ 4 ≤ 6  or ...

-2 3 3 -3   ->  3 ≤ 3 ≥ -3 ≤ -2  or  -2 ≤ 3 ≥ -3 ≤ 3 or ...

# solution -1

sort : O(n logn)
swapping : O (n)

O(n log n)


---

