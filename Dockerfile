# Base Dockerfile for Rshiny application hosted out of GCS bucket
#
# Adjust version of container to match desired R version
# Usage: 
#  Tune this Dockerfile to install the appropriate version of R and necessary prequisites
#  Tune the entrypoint.sh script to set up any runtime prerequisites as necessary. By default, mount gcs and start shiny-server.
#  Create a service account for the application to run as in IAM
#  Create a bucket for the application to run out of
#  Add Storage Legacy Bucket+Object Reader permission for application service account on bucket
#  Upload the R source code and any other app files to the bucket
#  Build container and push the image to the project's container or artifact registry
#  Create as a Cloud Run application, hardware generation 2 (critical!). Set port the following environment vars:
#    BUCKET  - the name of the bucket that contains the app
#    MNT_DIR - where to mount the bucket in the container. Defaults to /srv/shiny-server. Creates symlink from $MNT_DIR to /srv/shiny-server/$MNT_DIR,
#              (This is if it needs to be accessable at https://url/somepath. Set mountdir to /opt/somepath to make this happen) 

FROM rocker/shiny:4.2


# Install any prerequisites with package manager. This will depend on R packags required.

RUN apt-get update && apt-get install -y curl libssl-dev libcurl4-gnutls-dev libxml2-dev lsb-release gnupg libmariadbclient-dev-compat texlive-latex-base texlive-fonts-recommended texlive-fonts-extra texlive-latex-extra; \
    gcsFuseRepo=gcsfuse-`lsb_release -c -s`; \
    echo "deb http://packages.cloud.google.com/apt $gcsFuseRepo main" | \
    tee /etc/apt/sources.list.d/gcsfuse.list; \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | \
    apt-key add -; \
    apt-get update; \
    apt-get install -y gcsfuse \
    && apt-get clean

RUN R -e 'install.packages(c("shiny","plotly","dygraphs","shinyWidgets","BiocManager","plyr","dplyr","zoo","ggpubr","grid","gridExtra","gtable","shinydashboard","reactable","bcrypt","shinyBS","shinyjs","shinyFeedback","shinycssloaders","shinyAce","jsonlite","magrittr","knitr","DT","readxl","survival","survminer","multcomp","validate","purrr","stringr","shinyalert","kableExtra","devtools","ipc","promises","future"))'

RUN R -e "BiocManager::install(c('httr', 'yaml', 'sevenbridges'))"

RUN mkdir -p /var/lib/shiny-server/bookmarks && chown shiny:shiny /var/lib/shiny-server/bookmarks

RUN mkdir -p /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server

ENV MNT_DIR /srv/shiny-server
ENV PORT 3838

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 3838

CMD ["/entrypoint.sh"]
