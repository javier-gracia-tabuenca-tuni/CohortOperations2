# syntax=docker/dockerfile:1
FROM --platform=linux/amd64 rocker/rstudio:4.3.0

# install OS dependencies including java and python 3
RUN apt-get update && apt-get install -y openjdk-11-jdk liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv git \
    # rjava
    libssl-dev libcurl4-openssl-dev  libpcre2-dev libicu-dev \
    # xml2
    libxml2-dev \
    # sodium
    libsodium-dev\
    # systemfonts
    libfontconfig1-dev \
    # textshaping
    libharfbuzz-dev libfribidi-dev\
    #ragg
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev\
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# install utility R packages
RUN install2.r \
	openssl \
	httr \
	xml2 \
	remotes \
	gitcreds \
&& rm -rf /tmp/download_packages/ /tmp/*.rds

# Create user and move to users directory
# Create a new user named "myuser" with UID and GID set to 1000
#RUN useradd -ms /bin/bash shiny
#USER shiny

# clone repo
RUN git clone https://github.com/javier-gracia-tabuenca-tuni/CohortOperations2.git /root/CohortOperations2

WORKDIR /root/CohortOperations2

COPY GITHUBPAT.txt /tmp/GITHUBPAT.txt

# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
#RUN R -e "renv::restore()"
# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
RUN --mount=type=secret,id=build_github_pat \
	cp /usr/local/lib/R/etc/Renviron ./Renviron \
        && echo "GITHUB_PAT=$(cat /tmp/GITHUBPAT.txt)" >> ./Renviron \
        && R -e "renv::restore()" \
        && rm ./Renviron


# install the jdbc drivers for database access using the OHDSI DatabaseConnector R package
ENV DATABASECONNECTOR_JAR_FOLDER="/root/hades/jdbc_drivers/bigquery"
# installing the latest drivers gives errors
# RUN R -e "DatabaseConnector::downloadJdbcDrivers('bigquery')"

RUN mkdir -p /root/hades/jdbc_drivers
COPY ./bq_drivers_1.2.14.zip /root/hades/jdbc_drivers
RUN unzip /root/hades/jdbc_drivers/bq_drivers_1.2.14.zip

EXPOSE 8787
EXPOSE 8888

ENTRYPOINT ["/usr/local/bin/R", "-e", "devtools::load_all('.');run_app(testthat::test_path('config', 'test_config_devatlas.yml'), options = list(port=8888))"]
