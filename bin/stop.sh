#!/bin/bash

delete_all() {
    kubectl delete deployment --all
    kubectl delete pods --all
}

while [ "${empty}" != "0" ]; do
    empty=$(kubectl get pods 2>/dev/null |
        grep -E "(Running|Terminating)" |
        wc -l |
        xargs echo
    )
    delete_all
    sleep 1
done
