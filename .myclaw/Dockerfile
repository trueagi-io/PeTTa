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

ENV CARGO_HOME=/opt/myclaw/cargo-home \
    RUSTUP_HOME=/opt/myclaw/toolchain/rustup \
    UV_CACHE_DIR=/opt/myclaw/uv-cache \
    CARGO_TARGET_DIR=/opt/myclaw/mork-ffi-target \
    HOME=/tmp/myclaw-home \
    PATH=/opt/myclaw/toolchain/cargo/bin:/usr/local/bin:/usr/local/sbin:/usr/sbin:/usr/bin:/sbin:/bin

RUN mkdir -p \
        /opt/myclaw/toolchain/cargo \
        "$RUSTUP_HOME" \
        "$CARGO_HOME/registry" \
        "$CARGO_HOME/git" \
        "$UV_CACHE_DIR" \
        "$CARGO_TARGET_DIR" \
        "$HOME" \
    && chmod 1777 \
        "$CARGO_HOME" \
        "$CARGO_HOME/registry" \
        "$CARGO_HOME/git" \
        "$UV_CACHE_DIR" \
        "$CARGO_TARGET_DIR" \
        "$HOME" \
    && curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs \
        -o /tmp/rustup-init.sh \
    && CARGO_HOME=/opt/myclaw/toolchain/cargo \
        RUSTUP_HOME="$RUSTUP_HOME" \
        HOME=/root \
        sh /tmp/rustup-init.sh -y --profile minimal --default-toolchain "$RUST_TOOLCHAIN" \
    && rm /tmp/rustup-init.sh \
    && ln -s /opt/myclaw/toolchain/cargo/bin/cargo /usr/local/bin/cargo \
    && ln -s /opt/myclaw/toolchain/cargo/bin/rustc /usr/local/bin/rustc \
    && ln -s /opt/myclaw/toolchain/cargo/bin/rustup /usr/local/bin/rustup \
    && python3 -m pip install --break-system-packages --no-cache-dir "uv==$UV_VERSION" \
    && rustc --version \
    && cargo --version \
    && swipl --version \
    && uv --version

ENTRYPOINT []
CMD ["bash"]
