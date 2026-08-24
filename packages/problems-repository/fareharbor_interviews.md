# FareHarbor Interview Questions

This document contains a collection of interview questions and practical coding challenges commonly associated with FareHarbor's technical interviews.

## Overview

FareHarbor's technical evaluation emphasizes practical software engineering, clean object-oriented and functional patterns, algorithm design (graph traversal, tree/nested data manipulation), and domain-driven design for booking/tour operations.

## Core Problem Categories

### 1. Nested Data Structure Processing
- **Aggregate Revenue from Nested Regions**: [Aggregate Revenue Problem](fareharbor_dict_processing.md)
  - Extract and aggregate product revenue across arbitrarily structured regional sales records.
- **Flatten Nested Objects**: [Flatten Object Problem](fareharbor_flatten_object.md)
  - Convert deeply nested dictionaries and arrays into flat key-value pairs using dot notation.
- **Sanitize JSON**: [Sanitize JSON Problem](fareharbor_sanitize_json.md)
  - Recursively identify and remove sensitive keys (passwords, tokens, PII) across nested JSON structures.

### 2. Graph & Matrix Algorithms
- **Canonical BFS Traversal & Levels**: [Canonical BFS Problem](fareharbor_canonical_bfs.md)
  - Breadth-First Search traversal and calculating level/distance maps for graph nodes.
- **Shortest Path in Unweighted Graph**: [Shortest Path BFS](fareharbor_shortest_path_bfs.md)
  - Finding the minimum path length between two arbitrary nodes using BFS.
- **Flood Fill (Grid Traversal)**: [Flood Fill Problem](fareharbor_flood_fill.md)
  - Updating connected components in 2D grids using DFS and BFS algorithms.

### 3. Domain Service & Business Logic
- **Daily Booking Summary**: [Daily Summary Problem](fareharbor_daily_summary.md)
  - Building domain service objects and models to aggregate booking counts and revenue for given dates.

## Engineering Method Framework

When approaching FareHarbor coding problems, follow this engineering workflow:

1. **Explore Problem**: Clarify input constraints, output types, and edge conditions.
2. **Brainstorm**: Identify possible algorithmic approaches and evaluate time/space trade-offs.
3. **Plan**: Formulate step-by-step logic and pseudo-code.
4. **Implement**: Write clean, readable, modular code adhering to idiomatic language standards.
5. **Verify**: Test against standard cases and edge cases (empty inputs, cycle graphs, deeply nested structures).
