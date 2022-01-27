# package that enables us to download packages from GitHub directly
install.packages('devtools')
library(devtools)

# for the package ruODK we have to enable the ropensci universe 
# this has to be done manually
options(repos = c(ropensci = 'https://ropensci.r-universe.dev',
                  CRAN = 'https://cloud.r-project.org'))
#  now it can be installed
install.packages('ruODK')

# now we are ready to download and install repvisforODK
devtools::install_github('swisstph/repvisforODK', force = TRUE)

