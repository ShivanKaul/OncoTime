#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color

chmod +x copy.sh

printf "\n${RED}Building...${NC}\n"
cabal build
build_status=$?
if [[ build_status == 0 ]] 
then
printf "\n${RED}Copying...${NC}\n"
source copy.sh

fi
