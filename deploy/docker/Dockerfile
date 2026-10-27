# IPM - Interviewer Performance Monitoring for MICS : Docker image, built from this repository
# From the repository root:
#   docker build -f deploy/docker/Dockerfile -t ipm-app:latest .
#   docker run --rm -p 3838:3838 --tmpfs /tmp:size=2g ipm-app:latest      ->  http://<server>:3838
FROM rocker/r-ver:4.4.2

# System libraries for haven, xml2/officer, ragg/systemfonts/textshaping, and zip for the results download
RUN apt-get update && apt-get install -y --no-install-recommends \
      libxml2-dev libfontconfig1-dev libfreetype6-dev libpng-dev libjpeg-dev libtiff5-dev libwebp-dev \
      libharfbuzz-dev libfribidi-dev zlib1g-dev zip unzip fonts-liberation \
    && rm -rf /var/lib/apt/lists/*

# R packages (pre-compiled binaries from the Posit Package Manager snapshot configured in rocker/r-ver).
# Kept in a separate layer: rebuilding after a code change only reinstalls ipmR (seconds).
RUN install2.r --error --skipinstalled \
      shiny bslib DT htmltools data.table haven ggplot2 officer flextable openxlsx yaml tidyselect scales ragg \
      jsonlite httr2 commonmark \
    && rm -rf /tmp/downloaded_packages

# The package itself, from the checked-out repository
COPY . /tmp/ipmR-src
RUN R CMD INSTALL --no-docs /tmp/ipmR-src && rm -rf /tmp/ipmR-src

# Run as an unprivileged user; all session data live in /tmp (mount it as tmpfs = RAM only)
RUN useradd --create-home --shell /usr/sbin/nologin ipm
USER ipm
# AI (optional): set ANTHROPIC_API_KEY / IPM_AI_OLLAMA_URL / IPM_AI_OPENAI_URL / IPM_AI_PROVIDER / IPM_AI_MODEL at run time
# (compose 'environment:'), never in the image
ENV IPM_IDLE_MINUTES=30 \
    R_USER_CACHE_DIR=/tmp/.cache \
    XDG_CACHE_HOME=/tmp/.cache
EXPOSE 3838
HEALTHCHECK --interval=60s --timeout=10s --start-period=60s CMD R -q -e "invisible(readLines(url('http://127.0.0.1:3838'), 1))" || exit 1
CMD ["R", "--no-save", "-q", "-e", "options(shiny.port = 3838, shiny.host = '0.0.0.0'); shiny::runApp(system.file('app', package = 'ipmR'), launch.browser = FALSE)"]
