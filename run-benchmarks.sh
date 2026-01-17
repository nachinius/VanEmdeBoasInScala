#!/bin/bash
# Script to run benchmarks for Van Emde Boas Tree implementation
# Usage: ./run-benchmarks.sh [all|performance|memory|comparison]

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored messages
print_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check if sbt is installed
if ! command -v sbt &> /dev/null; then
    print_error "sbt is not installed. Please install sbt first."
    exit 1
fi

# Parse command line arguments
BENCHMARK_TYPE=${1:-all}

print_info "Van Emde Boas Benchmarks Runner"
print_info "================================"
echo ""

case $BENCHMARK_TYPE in
    all)
        print_info "Running ALL benchmarks..."
        print_warning "This may take a while..."
        sbt "bench:test"
        ;;
    performance)
        print_info "Running performance benchmarks only..."
        sbt "bench:testOnly *Performance*"
        ;;
    memory)
        print_info "Running memory benchmarks only..."
        sbt "bench:testOnly *Memory*"
        ;;
    comparison)
        print_info "Running comparison benchmarks (mutable vs immutable)..."
        sbt "bench:testOnly *Comparision*"
        ;;
    *)
        print_error "Unknown benchmark type: $BENCHMARK_TYPE"
        echo ""
        echo "Usage: $0 [all|performance|memory|comparison]"
        echo ""
        echo "Options:"
        echo "  all         - Run all benchmarks (default)"
        echo "  performance - Run performance benchmarks only"
        echo "  memory      - Run memory benchmarks only"
        echo "  comparison  - Run comparison benchmarks (mutable vs immutable)"
        echo ""
        echo "Examples:"
        echo "  $0"
        echo "  $0 all"
        echo "  $0 performance"
        exit 1
        ;;
esac

print_info "Benchmarks completed!"
print_info "Results are saved in the 'tmp' directory"
