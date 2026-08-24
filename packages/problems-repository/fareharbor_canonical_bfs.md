# FareHarbor Canonical BFS Traversal & Levels

## Problem Description

Implement standard Breadth-First Search (BFS) graph traversal and level computation algorithms for unweighted graphs represented as adjacency lists.

### Concepts

1. **Queue (FIFO)**: Explores nodes in order of discovery (level-by-level).
2. **Visited Set**: Tracks previously explored vertices to prevent infinite loops in cyclic graphs.
3. **Level Mapping**: Computes the shortest distance (in number of edges) from the source vertex to every reachable vertex.

### Input Format

- `graph`: Dictionary where keys are node identifiers and values are lists of neighbor nodes.
- `start_node`: The starting vertex identifier.

```python
example_graph = {
    'A': ['B', 'C'],
    'B': ['A', 'D', 'E'],
    'C': ['A', 'F'],
    'D': ['B'],
    'E': ['B', 'F'],
    'F': ['C', 'E']
}
```

### Expected Output

1. `bfs(graph, start_node)`: List of node identifiers in the exact order they were visited:
   `['A', 'B', 'C', 'D', 'E', 'F']`
2. `bfs_with_levels(graph, start_node)`: Dictionary mapping each visited node to its level (distance from root):
   `{'A': 0, 'B': 1, 'C': 1, 'D': 2, 'E': 2, 'F': 2}`

### Complexity

- **Time Complexity**: $O(V + E)$ where $V$ is number of vertices and $E$ is number of edges.
- **Space Complexity**: $O(V)$ to store visited set and queue.
