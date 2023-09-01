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
USER root
RUN useradd -ms /bin/bash shiny
USER shiny

# clone repo
RUN git clone https://github.com/javier-gracia-tabuenca-tuni/CohortOperations2.git /home/shiny/CohortOperations2

WORKDIR /home/shiny/CohortOperations2
RUN git pull

COPY GITHUBPAT.txt /tmp/GITHUBPAT.txt

# install dependencies
#ARG GITHUB_PAT
#ENV GITHUB_PAT $GITHUB_PAT

# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
#RUN R -e "renv::restore()"
# install OHDSI HADES R packages from CRAN and GitHub, temporarily adding a GitHub Personal Access Token (PAT) to the Renviron file
RUN --mount=type=secret,id=build_github_pat \
	cp /usr/local/lib/R/etc/Renviron ./Renviron \
        && echo "GITHUB_PAT=$(cat /tmp/GITHUBPAT.txt)" >> ./Renviron \
        && R -e "renv::status()" \
        && rm ./Renviron

#ENV GITHUB_PAT ''

RUN echo $GITHUB_PAT

# install the jdbc drivers for database access using the OHDSI DatabaseConnector R package
ENV DATABASECONNECTOR_JAR_FOLDER="/home/shiny/hades/jdbc_drivers"
RUN R <<EOF
library(DatabaseConnector);
downloadJdbcDrivers('bigquery');
EOF

EXPOSE 8787




# # Install Rserve server and client
# RUN install2.r \
# 	Rserve \
# 	RSclient \
# && rm -rf /tmp/download_packages/ /tmp/*.rds

# # Rserve configuration
# COPY Rserv.conf /etc/Rserv.conf
# COPY startRserve.R /usr/local/bin/startRserve.R
# RUN chmod +x /usr/local/bin/startRserve.R

# EXPOSE 8787
# EXPOSE 6311

# # install supervisor process controller
# RUN apt-get update && apt-get install -y supervisor

# # start Rserve & RStudio using supervisor
# RUN echo "" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "[supervisord]" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "nodaemon=true" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "[program:Rserve]" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "command=/usr/local/bin/startRserve.R" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "[program:RStudio]" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "command=/init" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "stdout_logfile=/var/log/supervisor/%(program_name)s.log" >> /etc/supervisor/conf.d/supervisord.conf \
# 	&& echo "stderr_logfile=/var/log/supervisor/%(program_name)s.log" >> /etc/supervisor/conf.d/supervisord.conf

# CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord.conf"]
