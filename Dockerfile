# Multi-language FF3 Format Preserving Encryption development environment
# Supports: Ada, C++, .NET, Erlang, Go, Haskell, Java, JavaScript, Python, Rust, Swift

FROM ubuntu:24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG USERNAME=dev
ARG USER_UID=1000
ARG USER_GID=1000
ARG GO_VERSION=1.22.5
ARG DOTNET_CHANNEL=8.0
ARG NVM_VERSION=v0.39.7

# Base system packages and build dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    bash-completion \
    build-essential \
    ca-certificates \
    cmake \
    curl \
    erlang \
    g++ \
    gcc \
    gdb \
    git \
    gnat \
    gnupg \
    gprbuild \
    less \
    libcurl4-openssl-dev \
    libedit-dev \
    libgmp-dev \
    libsqlite3-0 \
    libssl-dev \
    libxml2-dev \
    locales \
    make \
    nano \
    ninja-build \
    openjdk-21-jdk \
    pkg-config \
    python3 \
    python3-pip \
    python3-venv \
    rsync \
    sudo \
    tar \
    unzip \
    vim \
    wget \
    xz-utils \
    zip && \
    rm -rf /var/lib/apt/lists/* \
 && erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().' -noshell && echo "✓ Erlang 25+ installed successfully"

# Configure UTF-8 locale
RUN sed -i 's/^# *en_US.UTF-8/en_US.UTF-8/' /etc/locale.gen && locale-gen
ENV LANG=en_US.UTF-8
ENV LC_ALL=en_US.UTF-8

# Create development user (handle existing UID/GID gracefully)
RUN (groupadd --gid ${USER_GID} ${USERNAME} 2>/dev/null || groupmod -n ${USERNAME} $(getent group ${USER_GID} | cut -d: -f1)) \
 && (useradd --uid ${USER_UID} --gid ${USER_GID} -m ${USERNAME} -s /bin/bash 2>/dev/null || usermod -l ${USERNAME} -d /home/${USERNAME} -m $(getent passwd ${USER_UID} | cut -d: -f1)) \
 && echo "${USERNAME} ALL=(ALL) NOPASSWD:ALL" >/etc/sudoers.d/90-${USERNAME}

USER ${USERNAME}
WORKDIR /home/${USERNAME}

# Erlang build tool (rebar3) - Install from apt repository
USER root
RUN apt-get update \
 && apt-get install -y --no-install-recommends rebar3 \
 && rm -rf /var/lib/apt/lists/* \
 && rebar3 version && echo "✓ rebar3 installed successfully"
USER ${USERNAME}

# Ada package manager (Alire)
RUN mkdir -p $HOME/.alire/bin \
 && curl -fsSL -o /tmp/alr.zip \
      https://github.com/alire-project/alire/releases/download/v2.1.0/alr-2.1.0-bin-x86_64-linux.zip \
 && cd /tmp && unzip -q alr.zip && install -m 0755 /tmp/bin/alr $HOME/.alire/bin/alr \
 && sudo install -m 0755 $HOME/.alire/bin/alr /usr/local/bin/alr \
 && rm -rf /tmp/alr.zip /tmp/bin \
 && alr version && echo "✓ Alire installed successfully"

# Rust toolchain
RUN curl -fsSL https://sh.rustup.rs | sh -s -- -y \
 && echo 'source $HOME/.cargo/env' >> $HOME/.bashrc \
 && bash -lc 'source $HOME/.cargo/env && rustc --version && cargo --version' \
 && echo "✓ Rust installed successfully"

# Node.js via nvm
ENV NVM_DIR=/home/${USERNAME}/.nvm
RUN mkdir -p ${NVM_DIR} \
 && curl -fsSL https://raw.githubusercontent.com/nvm-sh/nvm/${NVM_VERSION}/install.sh | bash \
 && bash -lc 'source $NVM_DIR/nvm.sh && nvm install --lts && nvm alias default lts/*' \
 && { echo 'export NVM_DIR="$HOME/.nvm"'; \
      echo '[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"'; } >> $HOME/.bashrc \
 && bash -lc 'source $NVM_DIR/nvm.sh && node --version && npm --version' \
 && echo "✓ Node.js installed successfully"

# Go programming language
RUN curl -fsSL https://go.dev/dl/go${GO_VERSION}.linux-amd64.tar.gz -o /tmp/go.tgz \
 && sudo tar -C /usr/local -xzf /tmp/go.tgz && rm /tmp/go.tgz \
 && { echo 'export GOROOT=/usr/local/go'; echo 'export GOPATH=$HOME/go'; \
      echo 'export PATH=$GOROOT/bin:$GOPATH/bin:$PATH'; } >> $HOME/.bashrc \
 && /usr/local/go/bin/go version && echo "✓ Go installed successfully"

# Haskell compiler and package manager
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1 BOOTSTRAP_HASKELL_MINIMAL=1
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh \
 && echo '[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env"' >> $HOME/.bashrc
RUN bash -lc '. ~/.ghcup/env && ghcup install ghc recommended && ghcup install cabal recommended && ghcup set ghc recommended' \
 && bash -c '. ~/.ghcup/env && ghc --version && cabal --version' \
 && echo "✓ Haskell installed successfully"

# Java and Gradle via SDKMAN
ENV SDKMAN_DIR=/home/${USERNAME}/.sdkman
RUN curl -fsSL https://get.sdkman.io | bash \
 && bash -lc 'source "$HOME/.sdkman/bin/sdkman-init.sh" && sdk install java 25-open && sdk install gradle 9.1.0' \
 && { echo 'export SDKMAN_DIR="$HOME/.sdkman"'; \
      echo '[[ -s "$HOME/.sdkman/bin/sdkman-init.sh" ]] && source "$HOME/.sdkman/bin/sdkman-init.sh"'; } >> $HOME/.bashrc \
 && bash -lc 'source "$HOME/.sdkman/bin/sdkman-init.sh" && java -version && gradle --version' \
 && echo "✓ Java/Gradle installed successfully"

# .NET SDK
USER root
RUN wget -q https://packages.microsoft.com/config/ubuntu/24.04/packages-microsoft-prod.deb \
 && dpkg -i packages-microsoft-prod.deb && rm -f packages-microsoft-prod.deb \
 && apt-get update && apt-get install -y --no-install-recommends dotnet-sdk-${DOTNET_CHANNEL} \
 && rm -rf /var/lib/apt/lists/* \
 && dotnet --version && echo "✓ .NET SDK installed successfully"
USER ${USERNAME}

# Swift programming language
USER ${USERNAME}
RUN curl -O https://download.swift.org/swiftly/linux/swiftly-$(uname -m).tar.gz \
 && tar zxf swiftly-$(uname -m).tar.gz \
 && ./swiftly init --quiet-shell-followup \
 && rm swiftly-* \
 && echo '. "${SWIFTLY_HOME_DIR:-$HOME/.local/share/swiftly}/env.sh"' >> $HOME/.bashrc \
 && bash -lc '. ~/.bashrc && swiftly install latest' \
 && bash -lc '. ~/.bashrc && swift --version' \
 && echo "✓ Swift installed successfully"

USER ${USERNAME}
WORKDIR /home/${USERNAME}
RUN echo '[[ ":$PATH:" != *":$HOME/.local/bin:"* ]] && PATH="$HOME/.local/bin:$PATH"' >> $HOME/.bashrc \
 && echo '[[ ":$PATH:" != *":$HOME/.alire/bin:"* ]] && PATH="$HOME/.alire/bin:$PATH"' >> $HOME/.bashrc \
 && echo 'export PS1="(devbox) \\u@\\h:\\w$ "' >> $HOME/.bashrc

# Build and test all FF3 implementations
# Copy dependency files first for better Docker layer caching
COPY --chown=${USERNAME}:${USERNAME} implementations/python/setup.py implementations/python/requirements.txt* /home/${USERNAME}/workspace/implementations/python/
COPY --chown=${USERNAME}:${USERNAME} implementations/ada/alire.toml /home/${USERNAME}/workspace/implementations/ada/
COPY --chown=${USERNAME}:${USERNAME} implementations/cpp/CMakeLists.txt /home/${USERNAME}/workspace/implementations/cpp/
COPY --chown=${USERNAME}:${USERNAME} implementations/go/go.mod implementations/go/go.sum /home/${USERNAME}/workspace/implementations/go/
COPY --chown=${USERNAME}:${USERNAME} implementations/java/build.gradle implementations/java/gradlew* /home/${USERNAME}/workspace/implementations/java/
COPY --chown=${USERNAME}:${USERNAME} implementations/javascript/package*.json /home/${USERNAME}/workspace/implementations/javascript/
COPY --chown=${USERNAME}:${USERNAME} implementations/rust/Cargo.toml implementations/rust/Cargo.lock /home/${USERNAME}/workspace/implementations/rust/
COPY --chown=${USERNAME}:${USERNAME} implementations/swift/Package.swift /home/${USERNAME}/workspace/implementations/swift/
COPY --chown=${USERNAME}:${USERNAME} implementations/haskell/stack.yaml* implementations/haskell/*.cabal /home/${USERNAME}/workspace/implementations/haskell/
COPY --chown=${USERNAME}:${USERNAME} implementations/dotnet/*.csproj /home/${USERNAME}/workspace/implementations/dotnet/

WORKDIR /home/${USERNAME}/workspace

# Install language-specific dependencies (--break-system-packages needed for Ubuntu 24.04 PEP 668)
RUN cd implementations/python && pip3 install --break-system-packages cryptography pytest
RUN cd implementations/javascript && bash -lc 'export NVM_DIR=/home/dev/.nvm && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" && npm install'

# Copy complete source code
COPY --chown=${USERNAME}:${USERNAME} . /home/${USERNAME}/workspace

# Set up NIST test vectors in standard location for all implementations
RUN sudo mkdir -p /tmp/ff3-test-data && \
    sudo cp shared/test-vectors/nist_ff3_official_vectors.json /tmp/ff3-test-data/ && \
    sudo chown -R ${USERNAME}:${USERNAME} /tmp/ff3-test-data/

# Set environment variable for test vector path
ENV FF3_TEST_VECTORS_PATH=/tmp/ff3-test-data/nist_ff3_official_vectors.json

# Make benchmark runner scripts executable
RUN chmod +x scripts/run-all-benchmarks.sh scripts/run-quick-benchmarks.sh

# Add scripts directory to PATH for easy access
RUN echo 'export PATH="$HOME/workspace/scripts:$PATH"' >> $HOME/.bashrc

# Build and test all implementations
# Erlang
RUN cd implementations/erlang && make test

# Ada
RUN cd implementations/ada && alr -n build

# C++
RUN cd implementations/cpp && mkdir -p build && cd build \
    && cmake .. && cmake --build . && ctest --verbose

# Go
RUN cd implementations/go && bash -lc 'export GOROOT=/usr/local/go && export GOPATH=$HOME/go && export PATH=$GOROOT/bin:$GOPATH/bin:$PATH && go test -race -cover ./...'

# Java
RUN cd implementations/java && bash -lc 'source "$HOME/.sdkman/bin/sdkman-init.sh" && ./gradlew clean build'

# .NET
RUN cd implementations/dotnet && dotnet build && dotnet test

# JavaScript/Node.js
RUN cd implementations/javascript && bash -lc 'export NVM_DIR=/home/dev/.nvm && [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh" && npm install && npm test'

# Python
RUN cd implementations/python && pip3 install --break-system-packages . && python3 tests/simple_test.py

# Rust
RUN cd implementations/rust && bash -lc '. ~/.bashrc && cargo test'

# Haskell
RUN cd implementations/haskell && bash -c '. ~/.ghcup/env && cabal update && cabal build && cabal test'

# Swift
RUN cd implementations/swift && bash -lc '. ~/.bashrc && swift build && swift test'

# Verification message
RUN echo "All FF3 implementations built and tested successfully."

# Usage notes (displayed when container starts)
RUN echo '' >> $HOME/.bashrc && \
    echo 'echo "=========================================="' >> $HOME/.bashrc && \
    echo 'echo "FF3 Multi-Language Benchmark Environment"' >> $HOME/.bashrc && \
    echo 'echo "=========================================="' >> $HOME/.bashrc && \
    echo 'echo ""' >> $HOME/.bashrc && \
    echo 'echo "Quick benchmark: run-quick-benchmarks.sh"' >> $HOME/.bashrc && \
    echo 'echo "All benchmarks:  run-all-benchmarks.sh"' >> $HOME/.bashrc && \
    echo 'echo "Help:            run-all-benchmarks.sh --help"' >> $HOME/.bashrc && \
    echo 'echo ""' >> $HOME/.bashrc && \
    echo 'echo "Results saved to: ~/workspace/artifacts/"' >> $HOME/.bashrc && \
    echo 'echo "=========================================="' >> $HOME/.bashrc && \
    echo 'echo ""' >> $HOME/.bashrc

CMD ["/bin/bash", "-l"]
