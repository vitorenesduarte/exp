#!/usr/bin/env bash

MAX_LINES=50

show_pod() {
    pod=$1
    status=$(kubectl logs ${pod} --since=10s |
        grep -Eo "Events observed [0-9]+" |
        tail -n 1 |
        awk '{ print $3 }')
    echo "${pod}: ${status}"
}

pods=$(kubectl get pods --no-headers |
    grep Running |
    grep exp- |
    sort -R |
    head -n ${MAX_LINES} |
    awk '{ print $1 }')

for pod in ${pods[@]}; do
    show_pod ${pod} &
done
wait
