
# LUXRUG Dashboard

## Objective
The LUXRUG Dashboard is designed to provide comprehensive analytics and visualizations for sales data. It aims to offer insights into revenue trends, patterns, and geographical distribution of product sales, facilitating better business decisions.

## Features
- **Revenue Tracking**: Visualize overall revenue over time and by specific patterns.
- **Detailed Analysis**: Break down revenue by product pattern, color, and size.
- **Geographical Insights**: Examine the spatial distribution of sales with choropleth maps.

## Prerequisites
Before you can run the dashboard locally, you need to have R and Shiny installed on your system. Additionally, ensure the following R packages are installed:
- shiny
- bslib
- hrbrthemes
- statebins
- forcats
- RColorBrewer
- knitr
- dplyr
- tidyr
- stringr
- ggplot2

You can install these packages using the following R command:
```r
install.packages(c("shiny", "bslib", "hrbrthemes", "statebins", "forcats", "RColorBrewer", "knitr", "dplyr", "tidyr", "stringr", "ggplot2"))
```

## Running Locally
1. Clone the repository or download the project files to your local machine.
2. Open the R project file `LUXRUG_dashboard.Rproj`.
3. Run the `project.R` file in RStudio or a similar R environment to launch the Shiny app.
4. The dashboard will be hosted locally on your default browser at `http://127.0.0.1:XXXX` where `XXXX` is the port number provided in the R console.

## Accessing Remotely
The dashboard is deployed on ShinyApps.io and can be accessed via the following URL: https://ziyanw.shinyapps.io/LUXRUG_dashboard/ This link will take you directly to the dashboard, where you can interact with the data visualizations remotely.

## Data
The dashboard uses the following datasets:
- `revenue_summary_tidied.rds`: Contains cleaned and summarized revenue data.
- Sales orders data extracted from `.txt` files such as `order_202410.txt` and `order_202411.txt`.

Ensure that these data files are correctly placed in the `www/` directory of your project (or as specified in your Shiny app's file paths) before running the dashboard.

## Support
For support, please open an issue in the project repository or contact the project maintainer at [zy189002671@gmail.com].