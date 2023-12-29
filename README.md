# Data-Analysis-in-R
This is the code I wrote for the data analysis of my master thesis

# Getting Started

This guide will help you get started with this thesis project by installing its dependencies from the DESCRIPTION file and running the main script. Follow the steps below to begin working on the project.

## Prerequisites

Before you start, ensure that you have R and RStudio installed on your system. You can download R from [https://cran.r-project.org/](https://cran.r-project.org/) and RStudio from [https://www.rstudio.com/](https://www.rstudio.com/).

## Instructions


### 1. Install Dependencies

The project's dependencies are listed in the DESCRIPTION file. To install these dependencies, we will use the `remotes` package.

1. Open RStudio or your R console.

2. In RStudio, open your project directory by clicking `File` > `Open Project` and selecting your project's directory.

3. In the R console or RStudio's R script editor, run the following file to install the dependencies:

```R
source("src/install.R")
```

This will read the DESCRIPTION file in your project directory and install the necessary packages from CRAN or other specified repositories.

### 2. Run the Main Script

The main script is named `main.R`. To run it, use the following command in RStudio or your R console:

```R
source("src/main.R")
```

This command will execute the `main.R` script, which is the entry point for the project.
