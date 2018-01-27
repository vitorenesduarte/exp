#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

sudo swapoff -a
echo -e "swapoff -a ${GREEN}successfull${NC}" &&
sudo groupadd docker
echo -e "groupadd docker ${GREEN}successfull${NC}" &&
sudo systemctl restart docker.service
echo -e "systemctl start docker.service ${GREEN}successfull${NC}" &&
CMD=$(cat ~/kubeadmjoin) &&
echo "${CMD}" &&
sudo $CMD &&
echo -e "KUBEADMINITCLI ${GREEN}successfull${NC}" &&
echo -e "${GREEN}init-node.sh DONE${NC}"
