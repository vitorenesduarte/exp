#!/usr/bin/env bash

NODES_FILE=nodes-table

USER=$(grep user emulab.config  | cut -d= -f2)
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

Main=$(cat ${NODES_FILE} | grep main | awk '{print $4}')
Args=$(tail -n +2 ${NODES_FILE} | awk '{print $4}' | tr '\n' ' ')

echo -e "Copying init-node.sh to ${USER}@${Main}"
scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ./init-node.sh ${USER}@"$Main":~/init-node.sh
echo -e "scp init-node.sh done ${GREEN}successfull${NC}"

echo -e "Executing init-master.sh in ${USER}@${Main}"
ssh -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ${USER}@"$Main" 'bash -s'< ./init-master.sh "~/init-node.sh" "${USER}" "${Args[@]}"
echo -e "ssh init-master.sh done ${GREEN}successfull${NC}"

echo -e "Copying k8s config from ${USER}@${Main}"
scp -o "StrictHostKeyChecking no" -o "UserKnownHostsFile /dev/null" ${USER}@"$Main":~/.kube/config ~/.kube/config
