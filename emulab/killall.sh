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

ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" 'sudo kubectl delete deployment --all' &&

echo -e "delete deployment ${GREEN}successfull${NC}" &&

Nodes=$(kubectl get nodes | tail -n +3 | awk '{print $1}') &&
echo -e "get each node name ${GREEN}successfull${NC}" &&

MainNode=$(kubectl get nodes | head -n +2 | tail -n +1 | awk '{print $1}') &&
echo -e "get main name ${GREEN}successfull${NC}" &&

for Node in ${Nodes[@]}
do
ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" Env_Node=$Node 'bash -s' << 'ENDSSH'
sudo kubectl drain $Env_Node --delete-local-data --force --ignore-daemonsets &&
sudo kubectl delete node $Env_Node
ENDSSH
echo -e "drain and delete each ${GREEN}successfull${NC}"
done &&

for NODE in ${Args[@]}
do
ssh -o "StrictHostKeyChecking no" ${USER}@"$NODE" 'sudo kubeadm reset' &&
echo -e "reset each node kubeadm ${GREEN}successfull${NC}"
done &&

ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" Env_MainNode=$MainNode 'bash -s' << 'ENDSSH'
sudo kubectl drain $Env_MainNode --delete-local-data --force --ignore-daemonsets &&
sudo kubectl delete node $Env_MainNode
ENDSSH

echo -e "drain and delete node master ${GREEN}successfull${NC}" &&

ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" 'sudo kubeadm reset' &&
echo -e "killall.sh ${GREEN}successfull${NC}"
