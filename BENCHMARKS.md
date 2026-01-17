# Benchmarks Guide

This document provides detailed information about the benchmarks available in the Van Emde Boas Tree project.

## Overview

The project includes comprehensive benchmarks using the [ScalaMeter](https://scalameter.github.io/) framework to measure:
- **Performance**: Time complexity of operations (insert, successor, predecessor)
- **Memory**: Space complexity and memory footprint
- **Comparison**: Mutable vs Immutable implementations

## Quick Start

### Using the Helper Scripts

The easiest way to run benchmarks is using the provided shell scripts:

```bash
# List all available benchmarks
./list-benchmarks.sh

# Run all benchmarks (may take a while)
./run-benchmarks.sh

# Run specific benchmark categories
./run-benchmarks.sh performance
./run-benchmarks.sh memory
./run-benchmarks.sh comparison
```

### Using SBT

For more control, you can use sbt commands directly:

```bash
# Run all benchmarks
sbt "bench:test"

# Run specific benchmark suites by pattern
sbt "bench:testOnly *Performance*"
sbt "bench:testOnly *Memory*"
sbt "bench:testOnly *Comparision*"

# Run a specific benchmark class
sbt "bench:testOnly com.nachinius.vanEmdeBoas.vanEmdeBoasAPerformance"
sbt "bench:testOnly com.nachinius.vanEmdeBoas.vanEmdeBoasAMemory"
```

## Available Benchmarks

### 1. Performance Benchmarks (`vanEmdeBoasAPerformance`)

Located in: `src/bench/scala/com/nachinius/vanEmdeBoas/vanEmdeBoasAPerformance.scala`

**Benchmarks included:**

#### Insert vs Data Size
- **What it measures**: Time to insert elements as data size increases
- **Parameters**: Data sizes from 8 to 100,000 (exponential)
- **Purpose**: Verify O(lg w) time complexity for insert operation

#### Successor vs Data Size
- **What it measures**: Time to find successor as data size increases
- **Parameters**: Data sizes from 8 to 100,000 (exponential)
- **Purpose**: Verify O(lg w) time complexity for successor operation

#### Successor vs Bit Size
- **What it measures**: Time to find successor as bit size (w) increases
- **Parameters**: Bit sizes from 4 to 30, with 50,000 elements
- **Purpose**: Verify O(lg w) time complexity dependency on word size

**How to run:**
```bash
./run-benchmarks.sh performance
# or
sbt "bench:testOnly *vanEmdeBoasAPerformance*"
```

### 2. Memory Benchmarks (`vanEmdeBoasAMemory`)

Located in: `src/bench/scala/com/nachinius/vanEmdeBoas/vanEmdeBoasAPerformance.scala`

**Benchmarks included:**

#### Memory Footprint vs Data Size and Bit Size
- **What it measures**: Memory consumption as data size (n) and bit size (w) increase
- **Parameters**: 
  - Data sizes: 10 to 100,000 (exponential, factor of 2)
  - Bit sizes: 10 to 30 (step of 2)
- **Purpose**: Verify O(n) space complexity

**How to run:**
```bash
./run-benchmarks.sh memory
# or
sbt "bench:testOnly *vanEmdeBoasAMemory*"
```

### 3. Comparison Benchmarks (Mutable vs Immutable)

#### Performance Comparison

Located in: `src/bench/scala/com/nachinius/vanEmdeBoas/ComparisionPerformanceVanEmdeBoasMutableAndImmutableTestPerformance.scala`

**Benchmarks included:**
- Successor operation performance for immutable implementation
- Successor operation performance for mutable implementation
- **Parameters**: Bit sizes from 16 to 30, data sizes from 1 to 10,000

#### Memory Comparison

Located in: `src/bench/scala/com/nachinius/vanEmdeBoas/ComparisionMemoryVanEmdeBoasMutableAndImmutableTestPerformance.scala`

**Benchmarks included:**
- Memory footprint comparison between mutable and immutable implementations

**How to run:**
```bash
./run-benchmarks.sh comparison
# or
sbt "bench:testOnly *Comparision*"
```

## Understanding the Results

### Result Location
Benchmark results are stored in the `tmp` directory in the project root.

### Result Format
ScalaMeter generates HTML reports with:
- Charts showing performance/memory trends
- Statistical analysis (mean, median, standard deviation)
- Confidence intervals
- Regression analysis

### Interpreting Performance Results

For **time-based benchmarks**:
- Lower values are better
- Look for the trend as parameters increase
- Expected: O(lg w) complexity means roughly linear growth with bit size

For **memory benchmarks**:
- Lower values are better
- Expected: O(n) complexity means linear growth with data size

## Configuration

The benchmark configuration is defined in `build.sbt`:
- Benchmark configuration extends Test configuration
- ScalaMeter version: 0.8.2
- Parallel execution is disabled for benchmarks
- Log buffering is disabled for real-time output

## Tips for Running Benchmarks

1. **Close other applications**: For accurate results, minimize other running applications
2. **Be patient**: Comprehensive benchmarks can take several minutes to complete
3. **Start small**: Test with a single benchmark type first before running all
4. **Check results**: Review the generated reports in the `tmp` directory
5. **Multiple runs**: For publication-quality results, run benchmarks multiple times

## Troubleshooting

### sbt not found
Install sbt following the instructions at https://www.scala-sbt.org/download.html

### Benchmarks taking too long
Try running specific benchmark suites instead of all benchmarks:
```bash
./run-benchmarks.sh performance  # Usually faster than memory benchmarks
```

### Out of memory errors
Increase JVM heap size:
```bash
export SBT_OPTS="-Xmx4G -Xms1G"
./run-benchmarks.sh
```

## Contributing

When adding new benchmarks:
1. Place them in `src/bench/scala/com/nachinius/vanEmdeBoas/`
2. Extend `Bench.OfflineReport` for offline HTML reports
3. Use ScalaMeter's `Gen` for test data generation
4. Update this document with the new benchmark description
5. Update `list-benchmarks.sh` if adding a new category

## References

- [ScalaMeter Documentation](https://scalameter.github.io/)
- [Van Emde Boas Tree on Wikipedia](https://en.wikipedia.org/wiki/Van_Emde_Boas_tree)
- [MIT 6.851 Advanced Data Structures - Lecture 11](https://courses.csail.mit.edu/6.851/fall17/lectures/L11.html)
