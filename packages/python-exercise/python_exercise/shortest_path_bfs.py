"""Shortest path in unweighted graph using Breadth-First Search (BFS)."""

from collections import deque
from typing import Any, Dict, List


def shortest_path_bfs(graph: Dict[Any, List[Any]], start: Any, end: Any) -> int:
    """Calculate the shortest distance between start and end node.

    Returns the minimum number of edges on the path from start to end.
    Returns 0 if start == end.
    Returns -1 if no path exists or start node is not in graph.
    """
    if not graph or start not in graph:
        return -1

    if start == end:
        return 0

    queue = deque([(start, 0)])
    visited = {start}

    while queue:
        node, dist = queue.popleft()

        for neighbor in graph.get(node, []):
            if neighbor == end:
                return dist + 1
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, dist + 1))

    return -1
