FROM swipl:9.3.35

# Install system build tools for your Python/native deps
RUN apt-get update \
 && apt-get install -y --no-install-recommends build-essential python3-dev git \
 && rm -rf /var/lib/apt/lists/*

