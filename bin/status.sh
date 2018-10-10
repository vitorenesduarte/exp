#!/usr/bin/env bash


show_pod() {
    pod=$1
    status=$(kubectl logs ${pod} | grep -Eo "Event [0-9]+ \| Observed [0-9]+" | tail -n 1 | awk '{ print $2" of " $5 }')
    echo "${pod}: ${status}"
}

pods=$(kubectl get pods --no-headers | grep exp- | awk '{ print $1 }')

for pod in ${pods[@]}; do
    show_pod ${pod} &
done
wait
