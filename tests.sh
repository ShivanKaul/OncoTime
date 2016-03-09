#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color

chmod +x oncotime
printf "\n${RED}Running tests...${NC}\n"
python regression.py
