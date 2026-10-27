# Deploy the IPM app to shinyapps.io
# 1. Create a free account at https://www.shinyapps.io, then Account > Tokens > Show > copy the setAccountInfo() line.
# 2. Open this folder in RStudio (or setwd() to it), paste your setAccountInfo() line below, and run the script.

install.packages(c("rsconnect", "shiny", "bslib", "DT", "data.table", "haven", "ggplot2", "officer",
                   "flextable", "openxlsx", "yaml", "tidyselect", "scales", "ragg", "jsonlite", "httr2", "commonmark"))

rsconnect::setAccountInfo(name = "<ACCOUNT>", token = "<TOKEN>", secret = "<SECRET>")

rsconnect::deployApp(
  appDir = ".",
  appName = "mics-ipm",
  appTitle = "IPM - Interviewer Performance Monitoring for MICS",
  appFiles = c("app.R", "app_core.R", list.files("R", full.names = TRUE))
)

# After the first deployment, in the shinyapps.io dashboard (Applications > mics-ipm > Settings):
#  - General > Instance size: "xlarge" (2 GB) or more for complete national datasets; "large" (1 GB) is enough during fieldwork.
#  - Advanced > Connection timeout / Read timeout: raise to 3600 so long report builds are not cut.
#  - Users > Authentication: make the app private and invite users (paid plans) - recommended for survey microdata.
