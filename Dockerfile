# https://cran.r-project.org/bin/macosx/big-sur-arm64/base/
# R 4.3.0: 2023-04-22

FROM rocker/r-ver:4.3.0

# use mirrors
RUN sed -i -e 's/http:\/\/archive\.ubuntu\.com\/ubuntu\//mirror:\/\/mirrors\.ubuntu\.com\/mirrors\.txt/' /etc/apt/sources.list

# install OS dependencies including java and python 3
RUN apt-get update && apt-get install -y openjdk-11-jdk liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv git \
    # rjava
    libssl-dev libcurl4-openssl-dev  libpcre2-dev libicu-dev \
    # xml2
    libxml2-dev \
    # sodium
    libsodium-dev \
    # systemfonts
    libfontconfig1-dev \
    # textshaping
    libharfbuzz-dev libfribidi-dev \
    #ragg
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    xdg-utils \
#    libssl-dev \
#    libxt-dev \
#    libssh2-1-dev \
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# timezone
ENV TZ=Europe/Helsinki
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

# ARG REPO='https://packagemanager.rstudio.com/cran/__linux__/jammy/2023-05-03'
# ARG REPO='https://packagemanager.posit.co/cran/__linux__/jammy/2023-05-31'
# ARG REPO='https://packagemanager.posit.co/cran/__linux__/jammy/2023-06-14'
ARG REPO='https://packagemanager.posit.co/cran/__linux__/jammy/2023-08-31'

RUN R -e "install.packages( \
c('openssl', \
'httr', \
'xml2', \
'remotes', \
'gitcreds', \
'Andromeda', \
'anytime', \
'apexcharter', \
'askpass', \
'assertthat', \
'backports', \
'base64enc', \
'base64url', \
'BH', \
'bigrquery', \
'bit', \
'bit64', \
'blob', \
'brew', \
'brio', \
'bslib', \
'cachem', \
'callr', \
'checkmate', \
'cli', \
'clipr', \
'colorspace', \
'commonmark', \
'cpp11', \
'crayon', \
'credentials', \
'curl', \
'DatabaseConnector', \
'DBI', \
'dbplyr', \
'desc', \
'devtools', \
'diffobj', \
'digest', \
'downlit', \
'dplyr', \
'ellipsis', \
'evaluate', \
'fansi', \
'farver', \
'fastmap', \
'filelock', \
'fontawesome', \
'fs', \
'future', \
'gargle', \
'generics', \
'gert', \
'ggiraph', \
'ggplot2', \
'ggplotify', \
'ggrepel', \
'gh', \
'gitcreds', \
'globals', \
'glue', \
'gridGraphics', \
'gtable', \
'here', \
'highr', \
'hms', \
'htmltools', \
'htmlwidgets', \
'httpuv', \
'httr', \
'httr2', \
'ini', \
'ipc', \
'isoband', \
'jquerylib', \
'jsonlite', \
'knitr', \
'labeling', \
'later', \
'lifecycle', \
'listenv', \
'lubridate', \
'magrittr', \
'memoise', \
'mime', \
'miniUI', \
'munsell', \
'openssl', \
'openxlsx', \
'ParallelLogger', \
'parallelly', \
'pillar', \
'pkgbuild', \
'pkgconfig', \
'pkgdown', \
'pkgload', \
'plogr', \
'pool', \
'praise', \
'prettyunits', \
'processx', \
'profvis', \
'progress', \
'promises', \
'ps', \
'purrr', \
'R6', \
'ragg', \
'rapidjsonr', \
'rappdirs', \
'rcmdcheck', \
'RColorBrewer', \
'Rcpp', \
'reactable', \
'reactR', \
'readr', \
'rematch2', \
'remotes', \
'rJava', \
'RJSONIO', \
'rlang', \
'rmarkdown', \
'roxygen2', \
'rprojroot', \
'RSQLite', \
'rstudioapi', \
'rversions', \
'sass', \
'scales', \
'sessioninfo', \
'settings', \
'shiny', \
'shinycustomloader', \
'shinydashboard', \
'shinyjs', \
'shinyWidgets', \
'snow', \
'sourcetools', \
'SqlRender', \
'stringi', \
'stringr', \
'sys', \
'systemfonts', \
'testthat', \
'textshaping', \
'tibble', \
'tidyr', \
'tidyselect', \
'timechange', \
'tinytex', \
'tippy', \
'triebeard', \
'txtq', \
'tzdb', \
'urlchecker', \
'urltools', \
'usethis', \
'utf8', \
'uuid', \
'validate', \
'vctrs', \
'viridisLite', \
'vroom', \
'waldo', \
'whisker', \
'withr', \
'xfun', \
'xml2', \
'xopen', \
'xtable', \
'yaml', \
'yulab.utils', \
'zip'), repos = '$REPO', dependencies = TRUE)"

ARG GITHUB_PAT=""

# github
RUN R -e "remotes::install_github('OHDSI/CohortGenerator@main')"
RUN R -e "remotes::install_github('ohdsi/Eunomia@main')"
RUN R -e "remotes::install_github('OHDSI/FeatureExtraction@main')"
RUN R -e "remotes::install_github('OHDSI/ResultModelManager@main')"
RUN R -e "remotes::install_github('ohdsi/ROhdsiWebApi@main')"
RUN R -e "remotes::install_github('javier-gracia-tabuenca-tuni/HadesExtras')"

# the app
ADD . /root/CohortOperations2

WORKDIR /root/CohortOperations2
RUN rm renv.lock; rm .Rprofile

ADD keys /root/CohortOperations2/keys

RUN mkdir -p /root/hades/jdbc_drivers
RUN unzip /root/CohortOperations2/bq_drivers_1.2.14.zip -d /root/hades/jdbc_drivers

EXPOSE 8787
#EXPOSE 8888

#CMD /bin/bash

#ENTRYPOINT ["/usr/local/bin/R", "-e", \
#            "devtools::load_all('.'); \
#            run_app(testthat::test_path('config', 'test_config_eunomia.yml'), options = list(host='0.0.0.0', port=8787))"]

#ENTRYPOINT ["/usr/local/bin/R", "-e", \
#            "devtools::load_all('.'); \
#            run_app(testthat::test_path('config', 'test_config_devatlas_hs.yml'), options = list(host='0.0.0.0', port=8787))"]

ENTRYPOINT ["/usr/local/bin/R", "-e", \
            "devtools::load_all('.'); \
            run_app(testthat::test_path('config', 'test_config_sandbox6.yml'), options = list(host='0.0.0.0', port=8787))"]

## login as root into running container
#
# docker exec -u root -t -i <container-id> /bin/bash
#
