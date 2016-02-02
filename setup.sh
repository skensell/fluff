#!/bin/bash
#
# Run this script to setup the development environment.
#

set -e

GHC_VERSION=7.8.4
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

die(){ echo "ERROR: $1" >&2; exit 1; }

doNormalSetup(){
    echo
    echo "Using cabal at $(which cabal)"
    echo "which has version: $(cabal --version)"
    echo
    echo "Removing existing sandbox..."
    cabal sandbox delete
    echo 
    echo "Creating sandbox..."
    cabal sandbox init
    echo
    echo "Updating cabal..."
    cabal update
    echo
    echo "Installing packages..."
    cabal install --only-dependencies
    echo
    echo "Configuring..."
    cabal configure
    echo
    echo "Building fluff..."
    cabal build

    echo
    echo "==NOTES=="
    echo
    echo "A working version of fluff is at ${THIS_DIR}/bin/fluff"
    echo "Add ${THIS_DIR}/bin to your PATH for conveniently calling fluff from anywhere."
    echo
    echo "Success! Your dev environment is setup and fluff is built."
}

makeContainerForGHC(){
    mkdir -p ghc-distributions

    if [ ! -d ghc-distributions/ghc-${GHC_VERSION}.app/Contents/bin ]; then
        if [ ! -f ghc-distributions/ghc-${GHC_VERSION}-r1.zip ]; then
            echo
            echo 'Downloading ghc-dot-app...'
            curl -L -o ghc-distributions/ghc-${GHC_VERSION}-r1.zip https://github.com/ghcformacosx/ghc-dot-app/releases/download/v${GHC_VERSION}-r1/ghc-${GHC_VERSION}-r1.zip
        fi
        echo
        echo 'Unzipping ghc-dot-app...'
        cd ghc-distributions
        unzip ghc-${GHC_VERSION}-r1.zip
        cd ..
    fi

    export PATH="${THIS_DIR}/ghc-distributions/ghc-${GHC_VERSION}.app/Contents/bin":$PATH
    echo "Container created for ghc at ghc-distributions/ghc-${GHC_VERSION}.app/Contents/bin"
}

if [[ $(ghc --version | awk '{ print $8; }') == 7.8.* ]]; then
    echo 'Thank god. You have ghc 7.8.*. No hacks required.'
    doNormalSetup
    exit 0
fi

echo 
echo 'Your version of ghc is not 7.8.*. We will download ghc 7.8 and create a container for it.'
makeContainerForGHC
doNormalSetup

echo 
echo "If you want to build fluff again from source, you must make sure that you are using ghc-${GHC_VERSION}."
echo "An easy way to do this is to add ghc-distributions/ghc-${GHC_VERSION}.app/Contents/bin to your PATH."
echo "Another way is to try running this script again (which will reinstall all packages too)."

