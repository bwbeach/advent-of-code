# Use the official Ubuntu LTS base image
FROM ubuntu:latest

# Set environment variables
ENV DEBIAN_FRONTEND noninteractive

# Install dependencies
RUN apt-get update && \
    apt-get install -y \
    curl \
    libgmp-dev \
    libtinfo-dev \
    libsqlite3-dev \
    zlib1g-dev

# Install ghcup
RUN curl https://get-ghcup.haskell.org -sSf | sh

# Add ghcup to the PATH
ENV PATH="/root/.ghcup/bin:${PATH}"

# Clean up
RUN apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set the working directory
WORKDIR /app

# Set the default command
CMD ["bash"]
