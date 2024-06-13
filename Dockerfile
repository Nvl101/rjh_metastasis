# CUTOVER https://stackoverflow.com/questions/55123637
# copy this folder to container
FROM continuumio/miniconda3
WORKDIR /app
COPY . .
RUN echo $(ls .)
# import conda, python and R environment from configuration
RUN echo 'creating conda environment'
RUN conda env create -f conda_env.yml
# Activate the environment, and make sure it's activated:
# RUN conda init bash
# RUN echo "conda activate drug_recommendation" > ~/.bashrc
# RUN conda activate drug_recommendation
# RUN R -e "renv::restore()"
# save this image, but don't run shinyapp
# TODO: re-run this script to build docker container