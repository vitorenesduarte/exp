#!/usr/bin/env bash

ENV_VARS=(
  DIR
  IMAGE
  DOCKERFILE
)

for ENV_VAR in "${ENV_VARS[@]}"
do
  if [ -z "${!ENV_VAR}" ]; then
    echo ">>> ${ENV_VAR} is not configured; please export it."
    exit 1
  fi
done

# release vcd
# shellcheck disable=SC2164
cd "${DIR}"/.. && make rel && cd -

# build image
docker build \
  --no-cache \
  -t "${IMAGE}" -f "${DOCKERFILE}" .

# push image
docker push "${IMAGE}"

