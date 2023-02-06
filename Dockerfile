FROM inseefrlab/onyxia-vscode-r-python-julia:r4.2.1

USER root

RUN /rocker_scripts/install_geospatial.sh # && \
    # Install additional R packages
   # install2.r -e -s your_list_of_r_packages && \
    # Install additional Python packages
   #  pip install your_list_python_packages

USER 1000
