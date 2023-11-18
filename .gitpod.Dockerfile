# Use the official Ubuntu LTS base image
FROM gitpod/workspace-base

# Set environment variables
# ENV DEBIAN_FRONTEND noninteractive

# Install dependencies
# RUN sudo apt update

# Running ghcup install by hand with workspace base said this:
# Please ensure the following distro packages are installed before continuing (you can exit ghcup and return at any time): build-essential curl libffi-dev libffi8ubuntu1 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5

RUN sudo apt install \
    build-essential \
    curl \
    libffi-dev \
    libffi8ubuntu1 \
    libgmp-dev \
    libgmp10 \
    libncurses-dev \
    libncurses5 \
    libtinfo5

#RUN apt-get update && \
#    apt-get install -y \
#    curl \
#    libgmp-dev \
#    libtinfo-dev \
#    libsqlite3-dev \
#    zlib1g-dev \
#    git

# Install ghcup
RUN curl https://get-ghcup.haskell.org -sSf | sh

# Add ghcup to the PATH
ENV PATH="/$HOME/.ghcup/bin:${PATH}"

# Clean up
# RUN apt-get clean && \
#    rm -rf /var/lib/apt/lists/*

# Set the working directory
# WORKDIR /app

# Set the default command
# CMD ["bash"]
