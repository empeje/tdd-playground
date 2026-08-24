"""Canonical Breadth-First Search (BFS) Traversal & Level Calculations."""

from collections import deque
from typing import Any, Dict, List


def bfs(graph: Dict[Any, List[Any]], start_node: Any) -> List[Any]:
    """Perform standard BFS traversal on an unweighted graph.

    Returns the list of visited nodes in discovery order.
    """
    if not graph or start_node not in graph:
        return []

    queue = deque([start_node])
    visited = {start_node}
    visit_order = []

    while queue:
        current_node = queue.popleft()
        visit_order.append(current_node)

        for neighbor in graph.get(current_node, []):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append(neighbor)

    return visit_order


def bfs_with_levels(graph: Dict[Any, List[Any]], start_node: Any) -> Dict[Any, int]:
    """Compute the shortest distance (level) from start_node to all reachable nodes."""
    if not graph or start_node not in graph:
        return {}

    queue = deque([(start_node, 0)])
    visited = {start_node}
    levels: Dict[Any, int] = {}

    while queue:
        node, level = queue.popleft()
        levels[node] = level

        for neighbor in graph.get(node, []):
            if neighbor not in visited:
                visited.add(neighbor)
                queue.append((neighbor, level + 1))

    return levels
