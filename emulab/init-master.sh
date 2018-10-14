#!/usr/bin/env bash

RED='\033[0;31m'
NC='\033[0m' # No Color
GREEN='\033[0;32m'

INIT_NODE=$1; shift
USER=$1; shift
NODES=( "$@" )

sudo swapoff -a
echo -e "sudo swapoff -a ${GREEN}successfull${NC}"

sudo groupadd docker
echo -e "sudo groupadd docker ${GREEN}successfull${NC}"

sudo systemctl start docker.service
echo -e "sudo systemctl start docker.service ${GREEN}successfull${NC}"

APISERV=$(ifconfig | grep -Eb1 "enp6s7" | grep -Eo "inet addr:[0-9\.]+" | awk -F : '{print $2}') &&
echo -e "APISERVER ADVERTISE ADDRESS is ${APISERV}" && 

# sudo kubeadm init --pod-network-cidr=192.168.0.0/16 --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&
sudo kubeadm init --pod-network-cidr=10.244.0.0/16 --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&

#sudo kubeadm init --apiserver-advertise-address=${APISERV} --kubernetes-version stable-1.8 2>&1 | tee tmp &&

KUBEADMINIT=$(tail -2 tmp | grep join) &&
echo "${KUBEADMINIT}" > ~/kubeadmjoin &&
echo -e "sudo kubeadm init ${GREEN}successfull${NC}" &&

mkdir -p $HOME/.kube && 
echo -e "mkdir -p $HOME/.kube ${GREEN}successfull${NC}" &&

yes | sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config && 
echo -e "sudo cp -i /etc/kubernetes/admin.conf $HOME/.kube/config  ${GREEN}successfull${NC}" &&

sudo chown `id -u`:`id -g` $HOME/.kube/config &&
echo -e "sudo chown `id -u`:`id -g` $HOME/.kube/config ${GREEN}successfull${NC}" &&

sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/v0.9.1/Documentation/kube-flannel.yml &&
# sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml &&
echo -e "sudo kubectl apply -f https://raw.githubusercontent.com/coreos/flannel/master/Documentation/kube-flannel.yml ${GREEN}successfull${NC}" &&

sudo kubectl create clusterrolebinding add-on-cluster-admin --clusterrole=cluster-admin --serviceaccount=default:default &&
echo -e "sudo kubectl create clusterrolebinding add-on-cluster-admin --clusterrole=cluster-admin --serviceaccount=default:default ${GREEN}successfull${NC}" &&

for NODE in ${NODES[@]}; do
  echo "$arg1 $NODE $USER"
  ssh -o "StrictHostKeyChecking no" ${USER}@"$NODE" 'bash -s' < $INIT_NODE &
done 
wait
echo -e "${GREEN}init-master.sh DONE${NC}"
