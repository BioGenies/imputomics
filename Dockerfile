FROM rocker/r-base:4.3.0
RUN apt-get update -y && apt-get install -y  make pandoc zlib1g-dev libicu-dev cmake libcurl4-openssl-dev git libssl-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libfontconfig1-dev libxml2-dev libgit2-dev libjpeg-dev libpng-dev libtiff-dev && rm -rf /var/lib/apt/lists/*
RUN apt-get update -y && [ $(which google-chrome) ] || apt-get install -y gnupg curl
RUN [ $(which google-chrome) ] || curl -fsSL -o /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN [ $(which google-chrome) ] || DEBIAN_FRONTEND='noninteractive' apt-get install -y /tmp/google-chrome.deb
RUN apt-get install -y libcurl4-openssl-dev libssl-dev
RUN [ $(which google-chrome) ] || apt-get install -y gnupg curl
RUN [ $(which google-chrome) ] || curl -fsSL -o /tmp/google-chrome.deb https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb
RUN [ $(which google-chrome) ] || DEBIAN_FRONTEND='noninteractive' apt-get install -y /tmp/google-chrome.deb
RUN apt-get install -y libcurl4-openssl-dev libssl-dev libicu-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(renv.config.pak.enabled = FALSE, repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_version("renv", version = "0.17.0")'
COPY renv.lock renv.lock
RUN R -e 'renv::restore()'
