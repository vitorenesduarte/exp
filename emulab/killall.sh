#!/usr/bin/env bash

NODES_FILE=nodes-table

USER=$(grep user emulab.config  | cut -d= -f2)
RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

Main=$(cat ${NODES_FILE} | grep main | awk '{print $4}')
echo -e "Main done ${GREEN}successfull${NC}"

Args=$(tail -n +2 ${NODES_FILE} | awk '{print $4}' | tr '\n' ' ')
echo -e "Args done ${GREEN}successfull${NC}"

Nodes=$(kubectl get nodes | tail -n +3 | awk '{print $1}')
echo -e "get each node name ${GREEN}successfull${NC}"

MainNode=$(kubectl get nodes | head -n +2 | tail -n +1 | awk '{print $1}')
echo -e "get main name ${GREEN}successfull${NC}"

for NODE in ${Nodes[@]}; do
    cmd="sudo kubectl drain ${NODE} --delete-local-data --force --ignore-daemonsets && sudo kubectl delete node ${NODE}"
    ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" ${cmd} &
done
wait
echo -e "drain and delete each ${GREEN}successfull${NC}"

for NODE in ${Args[@]}; do
    cmd="sudo kubeadm reset"
    ssh -o "StrictHostKeyChecking no" ${USER}@"$NODE" ${cmd} &
done
wait
echo -e "reset each node kubeadm ${GREEN}successfull${NC}"

cmd="sudo kubectl drain ${MainNode} --delete-local-data --force --ignore-daemonsets && sudo kubectl delete node ${MainNode}"

ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" ${cmd}
echo -e "drain and delete node master ${GREEN}successfull${NC}"

ssh -o "StrictHostKeyChecking no" ${USER}@"$Main" 'sudo kubeadm reset'
echo -e "killall.sh ${GREEN}successfull${NC}"
