FROM ubuntu:latest

# Basic system libraries
#RUN    apk update \
#    && apk upgrade --available \
#    && apk add \
#        build-base make cmake gcc gmp curl xz perl cpio coreutils \
#        binutils-gold tar gzip unzip\
#        libc-dev musl-dev ncurses-dev gmp-dev zlib-dev expat-dev libffi-dev \
#        gd-dev postgresql-dev linux-headers git

# Bootstrap Cabal and GHC
# RUN curl https://gitlab.haskell.org/haskell/ghcup/raw/master/bootstrap-haskell -sSf | sh && /root/.ghcup/bin/ghcup set
# ENV PATH "$PATH:/root/.cabal/bin:/root/.ghcup/bin"
