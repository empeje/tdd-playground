from typing import List, Dict, Set, Tuple, Optional, Mapping
from collections import defaultdict
import unicodedata


class Node:
    def __init__(self, id: int, value: str, char_to_base: Dict[str, str]):
        self.id = id
        self.value = value
        self.lowercase = value.lower()
        self.normalized_first = self.normalize_char(self.lowercase[0], char_to_base) if self.lowercase else ''
        self.normalized_last = self.normalize_char(self.lowercase[-1], char_to_base) if self.lowercase else ''
    
    @staticmethod
    def normalize_char(char: str, char_to_base: Dict[str, str]) -> str:
        """Normalize a character to its base form."""
        char = char.lower()
        # Try the mapping first
        if char in char_to_base:
            return char_to_base[char]
        # If not in mapping, try to decompose and get base character
        normalized = unicodedata.normalize('NFD', char)
        base_char = normalized[0].lower()
        return base_char
    
    def __repr__(self) -> str:
        return f"Node(id={self.id}, value='{self.value}', first='{self.normalized_first}', last='{self.normalized_last}')"


class Graph:
    def __init__(self, nodes: List[Node]):
        self.nodes = nodes
        self.adjacency_list: Dict[int, List[int]] = {node.id: [] for node in nodes}
        self.in_degree: Dict[int, int] = {node.id: 0 for node in nodes}
        self.out_degree: Dict[int, int] = {node.id: 0 for node in nodes}
        self._build_graph()
        self._calculate_degrees()
    
    def _build_graph(self) -> None:
        """Build a directed graph where edges connect names with matching characters."""
        for node in self.nodes:
            last_char = node.normalized_last
            
            for other in self.nodes:
                if node.id != other.id:
                    if other.normalized_first == last_char:
                        self.adjacency_list[node.id].append(other.id)
    
    def _calculate_degrees(self) -> None:
        """Calculate in-degree and out-degree for each node."""
        for node_id in self.adjacency_list:
            self.out_degree[node_id] = len(self.adjacency_list[node_id])
            for neighbor_id in self.adjacency_list[node_id]:
                self.in_degree[neighbor_id] += 1
    
    def find_start_node(self) -> Optional[int]:
        """Find the appropriate start node for the chain."""
        # First try to find a node with out_degree = in_degree + 1
        for node_id in self.adjacency_list:
            if self.out_degree[node_id] == self.in_degree[node_id] + 1:
                return node_id
        
        # Check for circular chain
        is_circular = True
        for node_id in self.adjacency_list:
            if self.in_degree[node_id] != self.out_degree[node_id]:
                is_circular = False
                break
        
        if is_circular and any(self.out_degree.values()):  # Make sure there are some connections
            # For circular chains, just pick the first node as the starting point
            return self.nodes[0].id
        
        # Handle the case of duplicate names with valid chains
        for node_id in self.adjacency_list:
            if self.out_degree[node_id] > 0:  # Node has outgoing edges
                return node_id
        
        return None
    
    def find_path_dfs(self, start_node_id: int, total_names: int) -> List[int]:
        """Find a path through the graph using depth-first search."""
        path: List[int] = []
        visited: Set[int] = set()
        
        def dfs(node_id: int) -> bool:
            if len(path) == total_names:
                return True
            
            for neighbor_id in self.adjacency_list[node_id]:
                if neighbor_id not in visited:
                    visited.add(neighbor_id)
                    path.append(neighbor_id)
                    if dfs(neighbor_id):
                        return True
                    path.pop()
                    visited.remove(neighbor_id)
            return False
        
        path.append(start_node_id)
        visited.add(start_node_id)
        
        if dfs(start_node_id):
            return path
        
        return []
    
    def __repr__(self) -> str:
        return f"Graph(nodes={len(self.nodes)}, edges={sum(len(edges) for edges in self.adjacency_list.values())})"


def create_unicode_mapping() -> Dict[str, str]:
    """Create mappings for unicode character normalization."""
    # Map of unicode character variations
    unicode_map = {
        'a': ['a', 'á', 'à', 'â', 'ä', 'ã', 'å', 'æ'],
        'e': ['e', 'é', 'è', 'ê', 'ë', 'ẽ'],
        'i': ['i', 'í', 'ì', 'î', 'ï', 'ĩ'],
        'o': ['o', 'ó', 'ò', 'ô', 'ö', 'õ', 'ø', 'œ'],
        'u': ['u', 'ú', 'ù', 'û', 'ü', 'ũ'],
        'c': ['c', 'ç'],
        'n': ['n', 'ñ'],
        's': ['s', 'ß'],
        'y': ['y', 'ý', 'ÿ']
    }
    
    # Create reverse mapping for quick lookups
    char_to_base = {}
    for base, variations in unicode_map.items():
        for char in variations:
            char_to_base[char] = base
            
    return char_to_base


def normalize_char(char: str, char_to_base: Dict[str, str]) -> str:
    """Normalize a character to its base form."""
    return Node.normalize_char(char, char_to_base)


def create_nodes(names: List[str]) -> Tuple[List[Node], Dict[str, List[Node]]]:
    """Create node objects for each name with normalized characters."""
    nodes = []
    name_to_nodes = defaultdict(list)
    char_to_base = create_unicode_mapping()
    
    for i, name in enumerate(names):
        node = Node(i, name, char_to_base)
        nodes.append(node)
        name_to_nodes[node.lowercase].append(node)
    
    return nodes, name_to_nodes


def find_name_chain(names: List[str]) -> List[str]:
    """
    Find a chain of names where the last character of each name matches
    the first character of the next name. The solution must be unique.
    
    Args:
        names: List of names to chain
        
    Returns:
        List of names in chain order, or empty list if no valid chain exists
    """
    if not names:
        return []
    
    if len(names) == 1:
        return names
    
    nodes, name_to_nodes = create_nodes(names)
    graph = Graph(nodes)
    
    start_node_id = graph.find_start_node()
    
    if start_node_id is None:
        return []
    
    path = graph.find_path_dfs(start_node_id, len(names))
    
    if path:
        result = [nodes[node_id].value for node_id in path]
        return result
    
    return []