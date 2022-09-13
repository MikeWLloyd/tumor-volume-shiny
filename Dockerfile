FROM rocker/shiny:4.0.4

RUN apt-get update && apt-get install -y \
    git libtiff-dev libjpeg-dev curl && \
    R -e "install.packages(c('shiny','plotly', 'dygraphs', 'BiocManager','shinyWidgets','shinydashboard','shinyjs','shinyBS','shinyAce','shinyFeedback','shinycssloaders', 'multcomp','validate','survminer','survival','readxl','DT','knitr','dplyr','plyr', 'ggpubr','grid','gridExtra','gtable','reactable','bcrypt','jsonlite','magrittr'))"

RUN R -e "BiocManager::install(c('httr', 'yaml', 'sevenbridges'))"

RUN mkdir -p /build_zone

COPY . /build_zone

WORKDIR /build_zone

EXPOSE 3838

CMD ["Rscript", "-e", "source('app.R'); runApp(host='0.0.0.0', port=3838)"]
