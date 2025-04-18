# Booking.com Name Chain Problem

## Problem Description

Given a list of names in random order, create a function that chains the names such that the last character of each name matches the first character of the next name. The solution should be unique, meaning there should be only one possible starting name.

### Example

Input:
```python
["Raymond", "Nora", "Daniel", "Louie", "Peter", "Esteban"]
```

Output:
```python
["Peter", "Raymond", "Daniel", "Louie", "Esteban", "Nora"]
```

### Problem Breakdown

1. **Chain Requirements**:
   - Last character of current name must match first character of next name
   - All names must be used exactly once
   - Solution must be unique (only one possible starting name)

2. **Key Observations**:
   - The first name in the chain must be unique in that no other name can start with its last character
   - The last name in the chain must be unique in that no other name can end with its first character
   - This forms a Hamiltonian path in a graph where:
     - Nodes are names
     - Edges connect names where last character matches first character

### Solution Approach

1. **Graph Construction**:
   - Create a graph where each name is a node
   - Add directed edges from name A to name B if last character of A matches first character of B

2. **Find Start and End Points**:
   - Count in-degree and out-degree for each name
   - Start node has out-degree = in-degree + 1
   - End node has in-degree = out-degree + 1

3. **Path Finding**:
   - Start from the identified start node
   - Use DFS to find a path that uses all nodes
   - Ensure path is unique

### Example Solution in Python

```python
from typing import List, Dict, Set

def find_name_chain(names: List[str]) -> List[str]:
    if not names:
        return []
    
    # Build graph
    graph: Dict[str, List[str]] = {name: [] for name in names}
    for name in names:
        last_char = name[-1].lower()
        for other in names:
            if other != name and other[0].lower() == last_char:
                graph[name].append(other)
    
    # Find start and end nodes
    in_degree: Dict[str, int] = {name: 0 for name in names}
    out_degree: Dict[str, int] = {name: 0 for name in names}
    
    for name in names:
        out_degree[name] = len(graph[name])
        for neighbor in graph[name]:
            in_degree[neighbor] += 1
    
    start_node = None
    for name in names:
        if out_degree[name] == in_degree[name] + 1:
            start_node = name
            break
    
    if not start_node:
        return []  # No valid chain possible
    
    # Find path using DFS
    path: List[str] = []
    visited: Set[str] = set()
    
    def dfs(node: str) -> bool:
        if len(path) == len(names):
            return True
        
        for neighbor in graph[node]:
            if neighbor not in visited:
                visited.add(neighbor)
                path.append(neighbor)
                if dfs(neighbor):
                    return True
                path.pop()
                visited.remove(neighbor)
        return False
    
    path.append(start_node)
    visited.add(start_node)
    if dfs(start_node):
        return path
    return []
```

### Time Complexity

- Graph construction: O(n²) where n is number of names
- Finding start/end nodes: O(n)
- DFS path finding: O(n!) in worst case
- Overall: O(n!)

### Space Complexity

- Graph storage: O(n²)
- DFS stack: O(n)
- Overall: O(n²)

### Interview Tips

1. **Clarifying Questions to Ask**:
   - Are names case-sensitive?
   - Can the chain be circular?
   - What if multiple valid chains exist?
   - How to handle empty input?

2. **Edge Cases to Consider**:
   - Empty input
   - Single name
   - No valid chain possible
   - Multiple possible chains
   - Names with same start/end characters

3. **Optimization Ideas**:
   - Early termination if multiple chains found
   - Precompute character matches
   - Use memoization for repeated subproblems
   - Consider using Eulerian path algorithms

## Follow-up Questions

1. How would you modify the solution if:
   - The chain can be circular?
   - Multiple valid chains are acceptable?
   - Names can be used multiple times?
   - We need to find the longest possible chain?

2. System Design Considerations:
   - How to handle very large lists of names?
   - How to make the solution distributed?
   - How to handle real-time updates to the name list?

## References

- Original source: [Booking.com Live Test](https://github.com/malvee/Booking.com/blob/master/live-test.md)
- Related problems:
  - Hamiltonian Path
  - Word Chain
  - Eulerian Path 