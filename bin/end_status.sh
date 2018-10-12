#!/usr/bin/env bash

show_pod() {
    pod=$1
    status=$(kubectl logs ${pod} |
        grep -Eo "Events observed [0-9]+" |
        tail -n 1 |
        awk '{ print $3 }')
    echo "${pod}: ${status}"
}

pods=$(kubectl get pods --no-headers |
    grep Running |
    grep exp- |
    awk '{ print $1 }')

for pod in ${pods[@]}; do
    show_pod ${pod} &
done
wait
