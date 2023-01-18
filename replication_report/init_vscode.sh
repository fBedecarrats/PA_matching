#!/bin/sh

# Install Google CLI
# sudo apt-get update
# sudo apt-get install apt-transport-https ca-certificates gnupg
# echo "deb [signed-by=/usr/share/keyrings/cloud.google.gpg] https://packages.cloud.google.com/apt cloud-sdk main" | sudo tee -a /etc/apt/sources.list.d/google-cloud-sdk.list
# sudo apt-get update && sudo apt-get install google-cloud-cli

sudo apt update
sudo apt remove -y tzdata # possiblement ce qui gèle l'installation sinon
# Install R (from https://rtask.thinkr.fr/installation-of-r-4-2-on-ubuntu-22-04-lts-and-tips-for-spatial-packages/)
sudo apt install -y --no-install-recommends software-properties-common dirmngr
# Add the keys
wget -qO- https://cloud.r-project.org/bin/linux/ubuntu/marutter_pubkey.asc | sudo tee -a /etc/apt/trusted.gpg.d/cran_ubuntu_key.asc
# add the R 4.0 repo from CRAN -- adjust 'focal' to 'groovy' or 'bionic' as needed
sudo add-apt-repository "deb https://cloud.r-project.org/bin/linux/ubuntu $(lsb_release -cs)-cran40/"
sudo apt update
# Install R
sudo apt install -y r-base r-base-core r-recommended r-base-dev
# Install spatial dependencies
sudo apt install -y libgdal-dev libproj-dev libgeos-dev libudunits2-dev libv8-dev libnode-dev libcairo2-dev libnetcdf-dev

# Install R packages
R -e "install.packages('languageserver')"

# Install R extension
code-server --install-extension reditorsupport.r

# Create variables
WORK_DIR=/home/onyxia/work/PA_matching
REPO_URL=https://${GIT_PERSONAL_ACCESS_TOKEN}@github.com/fBedecarrats/PA_matching.git # As initial
# curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | sudo apt-key --keyring /usr/share/keyrings/cloud.google.gpg add -

# Git
git clone $REPO_URL $WORK_DIR
chown -R onyxia:users $WORK_DIR

cd 