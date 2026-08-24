# TDD Playground

A dedicated repository for practicing and mastering Test-Driven Development (TDD) skills. This playground provides a structured environment to improve your TDD workflow and understanding.

## What is TDD?

Test-Driven Development is a software development approach where you:

1. Write a failing test first (Red)
2. Write the minimum code to make the test pass (Green)
3. Refactor the code while keeping tests passing (Refactor)

This cycle is often called the "Red-Green-Refactor" cycle.

## Purpose

This repository serves as a:

- Practice ground for TDD exercises
- Place to experiment with different testing approaches
- Collection of TDD examples and patterns
- Space to improve code design through testing
- Repository of interview problems and design patterns (Booking.com, FareHarbor, Revolut, etc.)

## Available Exercise Types

This playground supports TDD exercises in multiple languages:

- JavaScript/TypeScript
- Ruby
- Python
- Java

Each language has its own setup and test configuration.

## Prerequisites

- Node.js (v20 or later)
- pnpm (v10.8.1 or later)
- Ruby (v3.2 or later) - for Ruby exercises
- Python (v3.10 or later) - for Python exercises
- Java (v21 or later) & Maven (v3.8 or later) - for Java exercises

## Getting Started

1. Clone this repository
```bash
git clone https://github.com/yourusername/tdd-playground.git
cd tdd-playground
```

2. Install pnpm dependencies
```bash
pnpm install
```

3. Running tests based on exercise type:

For JavaScript exercises:
```bash
pnpm dlx nx test javascript-exercise
```

For Ruby exercises:
```bash
# Install Ruby dependencies
pnpm dlx nx bundle ruby-exercise
# Run tests
pnpm dlx nx test ruby-exercise
```

For Python exercises:
```bash
# Install Python dependencies
pip install uv
pnpm dlx nx install python-exercise
# Run tests
pnpm dlx nx test python-exercise
```

For Java exercises:
```bash
# Run tests
pnpm dlx nx test java-exercise
```

## Project Structure

- `/packages/problems-repository` - Markdown problem statements and interview guides (Booking.com, FareHarbor, Revolut, Cracking the Coding Interview)
- `/packages/javascript-exercise` - JavaScript/TypeScript TDD exercises
- `/packages/ruby-exercise` - Ruby TDD exercises
- `/packages/python-exercise` - Python TDD exercises (Booking.com & FareHarbor problem suites)
- `/packages/java-exercise` - Java TDD exercises (Revolut Load Balancer & URL Shortener suites)
- Each exercise directory contains its own tests and implementation files

## Problem Collections

- **Booking.com**: Closest Hotels, Hotel List, Name Chain, Payment Logs
- **FareHarbor**: Daily Summary Service, Dict Processing (Aggregate Revenue), Canonical BFS, Flood Fill, Shortest Path BFS, Sanitize JSON, Flatten Object
- **Revolut**: In-Memory Load Balancer, URL Shortener & Base62, HTTP Reverse Proxy, In-Memory CRUD API

## Best Practices

When practicing TDD in this playground:

1. Always write the test first
2. Keep tests and production code simple
3. Make small, incremental changes
4. Maintain a clean and readable test suite
5. Practice regular refactoring
6. Commit after each successful test-code cycle

## Contributing

Feel free to:

- Add new TDD exercises
- Improve existing examples
- Share different testing approaches
- Suggest improvements

## Continuous Integration

This repository includes GitHub Actions workflows that automatically run tests for all exercise types on push and pull requests to the master branch.

## License

MIT License - Feel free to use this playground for your own learning and practice.
