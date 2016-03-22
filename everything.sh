#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color


chmod +x build.sh
chmod +x tests.sh
. build.sh

if [[ $build_status == 0 ]]
then
	. tests.sh $2
else
	printf "\n${RED}Error while building. Not running tests${NC}\n"
fi
