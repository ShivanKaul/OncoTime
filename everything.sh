#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color


chmod +x build.sh
chmod +x tests.sh
./build.sh && ./tests.sh
