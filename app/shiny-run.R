## https://github.com/sol-eng/background-jobs/tree/main/shiny-job

# This script is used to run the application defined in app.R in the background
options(shiny.autoreload = TRUE)
# shiny::runApp()

# set appDir when directly running from home, otherwise leave to pwd if run.sh used
test <- shiny::runApp(
  # appDir = "app",
  port = 4700,
  launch.browser = FALSE,
  display.mode = "normal"
)

# shiny::runGist("35a333d23c88650d05d3f5e89eeb1239")

# rstudioapi::viewer("http://127.0.0.1:4700")

# rsconnect::showLogs("app")

# rstudioapi::translateLocalUrl("http://127.0.0.1:4700", absolute = TRUE)
