FROM swipl:9.3.33

ARG DEBIAN_FRONTEND=noninteractive
ARG RUST_TOOLCHAIN=nightly-2025-08-01
ARG UV_VERSION=0.8.22

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
        binutils \
        build-essential \
        ca-certificates \
        cmake \
        curl \
        git \
        pkg-config \
        python3 \
        python3-dev \
        python3-pip \
        python3-venv \
    && rm -rf /var/lib/apt/lists/*

ENV CARGO_HOME=/opt/myclaw/cargo \
    RUSTUP_HOME=/opt/myclaw/rustup \
    UV_CACHE_DIR=/opt/myclaw/uv-cache \
    CARGO_TARGET_DIR=/opt/myclaw/mork-ffi-target \
    HOME=/tmp/myclaw-home \
    PATH=/opt/myclaw/cargo/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin

RUN mkdir -p \
        "$CARGO_HOME" \
        "$RUSTUP_HOME" \
        "$UV_CACHE_DIR" \
        "$CARGO_TARGET_DIR" \
        "$HOME" \
    && chmod 1777 \
        "$CARGO_HOME" \
        "$RUSTUP_HOME" \
        "$UV_CACHE_DIR" \
        "$CARGO_TARGET_DIR" \
        "$HOME" \
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        | sh -s -- -y --profile minimal --default-toolchain "$RUST_TOOLCHAIN" \
    && python3 -m pip install --break-system-packages --no-cache-dir "uv==$UV_VERSION" \
    && rustc --version \
    && cargo --version \
    && swipl --version \
    && uv --version

ENTRYPOINT []
CMD ["bash"]
