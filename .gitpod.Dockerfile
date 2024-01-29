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

# Install b2 cli
RUN python3 -m pip install b2

# Install ghcup
RUN curl https://get-ghcup.haskell.org -sSf | sh

# Add ghcup to the PATH
ENV PATH="$HOME/.ghcup/bin:$HOME/.cabal/bin:${PATH}"

# Switch to the version of GHC that I'm using
RUN ghcup install ghc 9.2.8 && ghcup set ghc 9.2.8

# VSCode wants this version of hls when it starts up
RUN ghcup install hls 2.6.0.0

# Install some packages
RUN cabal install linear lens doctest

# Clean up
# RUN apt-get clean && \
#    rm -rf /var/lib/apt/lists/*
