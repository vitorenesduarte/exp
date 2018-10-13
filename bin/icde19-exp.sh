#!/usr/bin/env bash

DIR=$(dirname "$0")

# ${DIR}/bench_retwis.sh
${DIR}/bench_metadata.sh
${DIR}/bench_micro.sh
