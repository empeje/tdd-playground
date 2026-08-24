# FareHarbor Flood Fill (DFS & BFS)

## Problem Description

Given a 2D matrix (grid) representing pixel colors, a starting coordinate `(sr, sc)`, and a new color value `new_color`, perform a flood fill operation changing the color of the starting pixel and all 4-directionally connected pixels of the same original color.

### Input Format

- `grid`: 2D list of integers representing colors.
- `sr`: Starting row index (0-indexed).
- `sc`: Starting column index (0-indexed).
- `new_color`: Target color integer to apply.

```python
grid = [
    [1, 1, 1],
    [1, 1, 0],
    [1, 0, 1]
]
sr = 1, sc = 1, new_color = 2
```

### Expected Output

Updated 2D matrix:

```python
[
    [2, 2, 2],
    [2, 2, 0],
    [2, 0, 1]
]
```

### Requirements

1. Implement both `flood_fill_dfs` (recursive depth-first search) and `flood_fill_bfs` (queue-based breadth-first search).
2. Handle early return when the initial pixel color is already equal to `new_color` (preventing infinite recursion or redundant loops).
3. Correctly respect matrix boundary bounds (`0 <= r < rows`, `0 <= c < cols`).
