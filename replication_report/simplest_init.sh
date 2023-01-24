#!/bin/sh

# Create variables
WORK_DIR=/home/onyxia/work/PA_matching
REPO_URL=https://${GIT_PERSONAL_ACCESS_TOKEN}@github.com/fBedecarrats/PA_matching.git # As initial
# curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

# Git
git clone $REPO_URL $WORK_DIR
chown -R onyxia:users $WORK_DIR
