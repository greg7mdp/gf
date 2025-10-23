#!/bin/bash
set -euo pipefail

# Build reproducible gf binary using Docker
# Usage: ./.github/tools/build-reproducible.sh [output-dir]

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
OUTPUT_DIR="${1:-$REPO_ROOT/build-reproducible}"

echo "Building reproducible gf binary..."
echo "Repository root: $REPO_ROOT"
echo "Output directory: $OUTPUT_DIR"

mkdir -p "$OUTPUT_DIR"

# Build using Docker
docker buildx build \
    --file "$REPO_ROOT/.github/tools/reproducible.Dockerfile" \
    --target exporter \
    --output "type=local,dest=$OUTPUT_DIR" \
    --build-arg "GF_BUILD_JOBS=$(nproc)" \
    "$REPO_ROOT"

echo ""
echo "Build complete!"
echo "Binary location: $OUTPUT_DIR/gf"
echo ""
echo "Binary info:"
file "$OUTPUT_DIR/gf"
echo ""
echo "Size: $(du -h "$OUTPUT_DIR/gf" | cut -f1)"
echo ""
echo "Linked libraries:"
ldd "$OUTPUT_DIR/gf" || true
