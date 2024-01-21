#! /bin/bash

# Fail if any command fails
set -e 

# Expect one arg that is the name of the directory/project to create.
if [[ -z "$1" ]]; then
    echo "Usage: $0 <dirToCreate>"
    exit 1
fi

# Create the directory
if [[ -e "$1" ]]; then
    echo "$1 already exists"
    exit 1
fi
mkdir $1

# Get the root of the git project
gitroot=$(git rev-parse --show-toplevel)
if [[ ! -e $gitroot/templates/cabal.project ]]; then
    echo "could not find templates"
    exit 1
fi

# Initialize the Haskell project with cabal
cd $1
cabal init

# Copy in templates
cp -v $gitroot/templates/cabal.project .
cp -v $gitroot/templates/Main.hs app
cp -v $gitroot/templates/answers.txt .

# Add dependency on advent library
echo "Adding dependency on advent"
mv $1.cabal $1.tmp
cat $1.tmp | \
    sed -e '/build-depends/s/$/, advent, split, containers/' | \
    sed -e '/default-language/a\
    ghc-options:      -Wall' > $1.cabal
rm $1.tmp
