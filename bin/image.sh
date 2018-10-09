#!/usr/bin/env bash

if [ -z "$1" ]; then
  TAG=latest
else
  TAG="$1"
fi

DIR=$(dirname "$0")
IMAGE=vitorenesduarte/exp-copy:${TAG}
DOCKERFILE=${DIR}/../Dockerfiles/exp-copy

# release vcd
cd "${DIR}"/.. && make rel && cd -

# build image
docker build \
  --no-cache \
  -t "${IMAGE}" \
  -f "${DOCKERFILE}" .

# push image
docker push "${IMAGE}"
