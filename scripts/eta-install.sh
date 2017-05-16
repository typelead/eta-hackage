#!/usr/bin/env bash
if [ ! -d "${HOME}/eta" ]; then
    echo "Cloning eta"
    cd ${HOME}
    git clone --recursive https://github.com/typelead/eta
fi

cd ${HOME}/eta
git fetch --all --tags -p
git pull --all

if [ -n "${ETA_BRANCH}" ]; then
    echo "Building eta branch ${ETA_BRANCH}"
    git checkout "${ETA_BRANCH}"

elif [ -n "${ETA_TAG}" ]; then
    echo "Building eta tag ${ETA_TAG}"
    git checkout "${ETA_TAG}"

elif [ -n "${ETA_PR}" ]; then
    echo "Building eta pull request ${ETA_PR}"
    ETA_PR_NO=$(echo "${ETA_PR}" | sed -e 's/.*\///')
    ETA_BRANCH="pr-${ETA_PR_NO}"
    git fetch origin "pull/${ETA_PR_NO}/head:${ETA_BRANCH}"
    git checkout "${ETA_BRANCH}"
else
    echo "Building without trigger"
    git checkout master
fi

git submodule sync
git submodule update --init --recursive
./install.sh
