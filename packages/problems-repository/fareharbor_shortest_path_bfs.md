# FareHarbor Shortest Path (BFS)

## Problem Description

Given an unweighted graph represented as an adjacency list, calculate the shortest path (minimum edge count) between a given `start` node and `end` node. If no valid path connects the two nodes, return `-1`.

### Input Format

- `graph`: Dictionary of node adjacency lists.
- `start`: Identifier of starting node.
- `end`: Identifier of target destination node.

```python
graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B', 'F'],
    'F': ['C', 'E']
}
start = 'A', end = 'F'
```

### Expected Output

Integer distance: `2` (corresponding to path `A -> C -> F`).

### Edge Cases

- `start == end`: Distance is `0`.
- Target node unreachable / disconnected component: Return `-1`.
- Graph containing cycles: Visited set ensures termination.
- Empty graph or non-existent start node: Return `-1`.
