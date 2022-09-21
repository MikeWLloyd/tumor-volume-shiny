# Base Dockerfile for Rshiny application
#
# Adjust version of container to match desired R version

FROM rocker/shiny:4.2

# Install any prerequisites with package manager. This will depend on R packags required.

RUN apt-get update && apt-get install -y curl libssl-dev libcurl4-gnutls-dev libxml2-dev lsb-release gnupg libmariadbclient-dev-compat; \
    gcsFuseRepo=gcsfuse-`lsb_release -c -s`; \
    echo "deb http://packages.cloud.google.com/apt $gcsFuseRepo main" | \
    tee /etc/apt/sources.list.d/gcsfuse.list; \
    curl https://packages.cloud.google.com/apt/doc/apt-key.gpg | \
    apt-key add -; \
    apt-get update; \
    apt-get install -y gcsfuse \
    && apt-get clean

RUN R -e 'install.packages(c("shiny","plotly","dygraphs","shinywidgets","BiocManager","plyr","dplyr","zoo","ggpubr","grid","gridExtra","gtable","shinydashboard","reactable","bcrypt","shinyBS","shinyjs","shinyFeedback","shinycssloaders","shinyAce","jsonlite","magrittr","knitr","DT","readxl","survival","survminer","multcomp","validate","purrr","stringr"))'

RUN R -e "BiocManager::install(c('httr', 'yaml', 'sevenbridges'))"

RUN mkdir -p /var/lib/shiny-server/bookmarks && chown shiny:shiny /var/lib/shiny-server/bookmarks

# Copy application source code into /srv/shiny-server in container
#COPY . /srv/shiny-server/
RUN mkdir -p /srv/shiny-server
RUN chown -R shiny:shiny /srv/shiny-server
# If this app has an renv (highly recommended), install the dependencies into R

ENV MNT_DIR /srv/shiny-server
ENV PORT 3838

COPY entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

EXPOSE 3838

CMD ["/entrypoint.sh"]
