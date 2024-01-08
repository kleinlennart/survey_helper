#!/bin/bash


# use instead of background jobs

# appDir = getwd() -> needs to be launched from within this folder!
# port = getOption("shiny.port"),
# test.mode
# host
# To allow other clients to connect, use the value "0.0.0.0" instead

# keep window open
# http://127.0.0.1:4700/

# R -e "shiny::runApp(
#         port = 4700,
#         launch.browser = FALSE,
#         display.mode = 'normal'
#       )"

cd app
Rscript shiny-run.R
