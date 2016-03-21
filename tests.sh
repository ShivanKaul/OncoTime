#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color

chmod +x oncotime
printf "\n${RED}Running tests...${NC}\n"
if [ $# -eq 0 ]; then
    python regression.py
elif [ $# -eq 1 ]; then
    python regression.py $1
else
    python regression.py $1 $2
fi

