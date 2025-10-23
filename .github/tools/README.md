# gf Build Tools

This directory contains tools for building reproducible gf binaries.

## Reproducible Builds

The reproducible build system uses Docker to create portable Linux binaries that will run on most modern Linux distributions (glibc 2.27+).

### Overview

- **Base Image**: Debian Bullseye (oldoldstable) for maximum compatibility
- **Compiler**: Clang 21.1.4 with libc++ (statically linked)
- **CMake**: 3.27.6
- **Build Features**:
  - Statically linked libc++ for portability
  - Security hardening flags enabled
  - Reproducible builds with pinned timestamps
  - Minimal dependencies (only X11 and FreeType required at runtime)

### Building Locally

To build a reproducible binary locally:

```bash
./.github/tools/build-reproducible.sh [output-dir]
```

The binary will be created in the output directory (default: `build-reproducible/gf`).

### Requirements

- Docker with BuildKit support
- Docker Buildx (included in recent Docker versions)

### GitHub Actions Workflow

The reproducible build runs automatically when a new release is published on GitHub. The workflow:

1. Builds the binary using the reproducible Dockerfile
2. Creates a tarball with version name (e.g., `gf-v1.0.0-linux-x86_64.tar.gz`)
3. Generates SHA256 checksum
4. Uploads the binary and checksum to the GitHub release

You can also trigger a manual build via the GitHub Actions UI.

### Files

- `reproducible.Dockerfile` - Multi-stage Docker build for reproducible binaries
- `build-reproducible.sh` - Local build script for testing
- `README.md` - This file

### Testing the Binary

After building, verify the binary works:

```bash
# Check binary information
file build-reproducible/gf
ldd build-reproducible/gf

# Run the binary
./build-reproducible/gf --help
```

### Portability

The resulting binary should run on:
- Ubuntu 18.04+ (Bionic and newer)
- Debian 10+ (Buster and newer)
- RHEL/CentOS 8+
- Fedora 28+
- Most modern Linux distributions

Runtime requirements:
- X11 libraries (libX11)
- FreeType library (libfreetype)
- Linux kernel 3.2+
- glibc 2.27+

### Troubleshooting

**Docker build fails:**
- Ensure you have enough disk space (build requires ~5GB)
- Make sure Docker BuildKit is enabled: `export DOCKER_BUILDKIT=1`

**Binary won't run on target system:**
- Check glibc version: `ldd --version`
- Ensure X11 and FreeType are installed: `apt install libx11-6 libfreetype6`

**GPG key verification fails:**
- The build verifies signatures for CMake and LLVM downloads
- If keys expire, update the key IDs in the Dockerfile

### Security

The reproducible build includes security hardening:
- Stack protection (`-fstack-protector-strong`)
- Position-independent executable (PIE)
- FORTIFY_SOURCE=2
- RELRO and immediate binding for shared libraries
- Static linking of C++ standard library to avoid ABI issues

### Reproducibility

To verify reproducibility, build twice and compare:

```bash
./.github/tools/build-reproducible.sh output1
./.github/tools/build-reproducible.sh output2
sha256sum output1/gf output2/gf
```

The hashes should be identical.
