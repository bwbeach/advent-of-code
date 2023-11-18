# Use the official Ubuntu LTS base image
FROM gitpod/workspace-base

# Install dependencies
# RUN sudo apt-get update

# Running ghcup install by hand with workspace base said this:
# Please ensure the following distro packages are installed before continuing (you can exit ghcup and return at any time): build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN sudo apt-get -y install \
    build-essential \
    curl \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5

# Install ghcup
RUN curl https://get-ghcup.haskell.org -sSf | sh

# Switch to the version of GHC that I'm using
RUN ghcup install ghc 9.2.8 && ghcup set ghc 9.2.8

# Add ghcup to the PATH
ENV PATH="$HOME/.ghcup/bin:${PATH}"

# Clean up
# RUN apt-get clean && \
#    rm -rf /var/lib/apt/lists/*
