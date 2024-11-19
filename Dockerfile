FROM rocker/verse:4.4.2
RUN apt-get update && apt-get install -y  git libcurl4-openssl-dev libgit2-dev libssl-dev libx11-dev make zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://packagemanager.posit.co/cran/latest'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_local(".", upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');jotto::run_app();"
