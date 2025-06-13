# Package installation
# install.packages("readxl", dependencies = TRUE)        # Read Excel files
# install.packages("writexl", dependencies = TRUE)       # Write Excel files
# install.packages("openxlsx", dependencies = TRUE)      # Open Excel files
# install.packages("dplyr", dependencies = TRUE)         # Data manipulation
# install.packages("writexl", dependencies = TRUE)       # Save data back to Excel
# install.packages("tidyverse", dependencies = TRUE)     # Data processing
# install.packages("rstatix", dependencies = TRUE)       # Basic statistical tests
# install.packages("psych", dependencies = TRUE)         # Psychometrics and survey analysis
# install.packages("lubridate", dependencies = TRUE)     # Handling of dates
# install.packages("ggplot2", dependencies = TRUE)       # Enhanced data visualisation
# install.packages("purrr", dependencies = TRUE)         # Function application to objets
# install.packages("afex", dependencies = TRUE)          # ANOVAs
# install.packages("afex", dependencies = TRUE)          # Post hoc comparisons among groups after fitting a model
# install.packages("ez", dependencies = TRUE)            # Easy analysis and visualization of factorial experiments
# install.packages("ggpubr", dependencies = TRUE)        # Easy-to-use functions for creating and customizing 'ggplot2'- based publication ready plots
# install.packages("tidyr", dependencies = TRUE)         # Transforms data frames to and from tidy data
# install.packages("glue", dependencies = TRUE)          # Implementation of interpreted string literals
# install.packages("Matrix", dependencies = TRUE)        # Sparse and dense matrix classes and methods


# Packages loading
  library(readxl)
  library(writexl)
  library(openxlsx)
  library(dplyr)
  library(tidyverse)
  library(rstatix)
  library(psych)
  library(lubridate)
  library(ggplot2)
  library(purrr)
  library(afex)
  library(emmeans)
  library(ez)
  library(ggpubr)
  library(tidyr)
  library(glue)

  
# Solving the issue with the 'Matrix' package
  # remotes::install_version("Matrix", version = "1.5-4", repos = "http://cran.us.r-project.org")

  
  