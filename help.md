###  Purpose

**App** is a **Shiny** web application that allows the visualisation of data hosted by the [Malaria Atlas Project](https://map.ox.ac.uk/).

The application generates district-level summary statistics for a range of malaria indicators/malariometric data, as available by MAP. The aggregated district-level statistics enable the interpretation of disaggregated, high-spatial resolution trends (5 km x 5 km), at the administrative level.

The application allows user interaction and creates interactive visualizations such as maps displaying mean values for each district selected by the user.


### Dependencies


**App** has been developed using **R** and **Shiny** and is dependent on the following software and **R** packages:


|  |   |
--- | ----
**Software**   | 
R  | Language and environment for statistical computing and graphics|
**R packages** |
[raster](https://cran.r-project.org/web/packages/raster/raster.pdf)| Reading, writing, manipulating, analyzing and modeling of gridded spatial data. The package implements basic and high-level functions. Processing of very large files is supported.|
[shiny](https://cran.r-project.org/web/packages/shiny/shiny.pdf)| Makes it incredibly easy to build interactive web applications with R. Automatic 'reactive' binding between inputs and outputs and extensive prebuilt widgets make it possible to build beautiful, responsive, and powerful applications with minimal effort.|
[RColorBrewer](https://cran.r-project.org/web/packages/RColorBrewer/RColorBrewer.pdf)| Provides color schemes for maps (and other graphics) designed by Cynthia Brewer as described at http://colorbrewer2.org |
[malariaAtlas](https://cran.r-project.org/web/packages/malariaAtlas/malariaAtlas.pdf)| A suite of tools to allow you to download all publicly available parasite rate survey points, mosquito occurrence points and raster surfaces from the 'Malaria Atlas Project'servers as well as utility functions for plotting the downloaded data.|
[shinydashboard](https://cran.r-project.org/web/packages/shinydashboard/shinydashboard.pdf)| Create dashboards with 'Shiny'. This package provides a theme on top of 'Shiny', making it easy to create attractive dashboards.|
[rmarkdown](https://cran.r-project.org/web/packages/rmarkdown/rmarkdown.pdf)| Convert R Markdown documents into a variety of formats including HTML, MS Word, PDF, and Beamer.|
[stringr](https://cran.r-project.org/web/packages/stringr/stringr.pdf)| A consistent, simple and easy to use set of wrappers around the fantastic 'stringi' package.|
[shinyalert](https://cran.r-project.org/web/packages/shinyalert/shinyalert.pdf)| Easily create pretty popup messages (modals) in 'Shiny'. A modal can contain text, images, OK/Cancel buttons, an input to get a response from the user, and many more customizable options.|
[shinyBS](https://cran.r-project.org/web/packages/shinyBS/shinyBS.pdf)| Adds additional Twitter Bootstrap components to Shiny.|
[shinythemes](https://cran.r-project.org/web/packages/shinythemes/shinythemes.pdf) | Themes for use with Shiny. Includes several Bootstrap themes from http://bootswatch.com/, which are packaged for use with Shiny applications.|
 
