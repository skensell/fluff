#!/bin/bash
#
# Run this script to setup the development environment.
#

set -e

GHC_VERSION=7.8.4
THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

export PATH=$PATH:"${THIS_DIR}/ghc-distributions/ghc-${GHC_VERSION}.app/Contents/bin"

