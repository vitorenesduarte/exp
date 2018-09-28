#!/usr/bin/env bash

NODES_FILE=nodes-table

USER=$(grep user emulab.config  | cut -d= -f2)
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

Main=$(cat ${NODES_FILE} | grep main | awk '{print $4}') &&
echo -e "Main done ${GREEN}successfull${NC}" &&

Args=$(tail -n +2 ${NODES_FILE} | awk '{print $4}' | tr '\n' ' ') &&
echo -e "Args done ${GREEN}successfull${NC}" &&

scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ./init-node.sh ${USER}@"$Main":~/init-node.sh &&
echo -e "scp init-node.sh done ${GREEN}successfull${NC}" &&

ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ${USER}@"$Main" 'bash -s'< ./init-master.sh "~/init-node.sh" "${USER}" "${Args[@]}" &&

scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ${USER}@"$Main":~/.kube/config ~/.kube/config

echo -e "ssh init-master.sh done ${GREEN}successfull${NC}"
