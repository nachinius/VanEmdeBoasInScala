#!/bin/bash
# Script to list available benchmarks in the Van Emde Boas Tree project

set -e

# Colors for output
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${GREEN}Available Benchmarks in Van Emde Boas Tree Project${NC}"
echo "===================================================="
echo ""

BENCH_DIR="src/bench/scala/com/nachinius/vanEmdeBoas"

if [ ! -d "$BENCH_DIR" ]; then
    echo "Error: Benchmark directory not found at $BENCH_DIR"
    exit 1
fi

echo -e "${BLUE}Performance Benchmarks:${NC}"
echo "  - vanEmdeBoasAPerformance"
echo "    * insert vs n - Benchmark insert operation against data size"
echo "    * successor vs n - Benchmark successor operation against data size"
echo "    * successor vs bits - Benchmark successor operation against bit size"
echo ""

echo -e "${BLUE}Memory Benchmarks:${NC}"
echo "  - vanEmdeBoasAMemory"
echo "    * memory footprint vs n - Measure memory consumption"
echo ""

echo -e "${BLUE}Comparison Benchmarks:${NC}"
echo "  - ComparisionPerformanceVanEmdeBoasMutableAndImmutableTestPerformance"
echo "    * successor vs bits (immutable) - Benchmark immutable version"
echo "    * successor vs bits (mutable) - Benchmark mutable version"
echo ""
echo "  - ComparisionMemoryVanEmdeBoasMutableAndImmutableTestPerformance"
echo "    * memory consumption comparison between mutable and immutable"
echo ""

echo "To run benchmarks, use:"
echo "  ./run-benchmarks.sh [all|performance|memory|comparison]"
echo ""
echo "Or use sbt directly:"
echo "  sbt 'bench:test'                    # Run all benchmarks"
echo "  sbt 'bench:testOnly *Performance*'  # Run performance benchmarks only"
echo "  sbt 'bench:testOnly *Memory*'       # Run memory benchmarks only"
