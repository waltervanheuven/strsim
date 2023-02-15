# strsim

[R](https://www.r-project.org) package with string similarity functions relevant
for psycholinguistic research (e.g. neighbourhood density, OLD20).

## Installation

```R
library(devtools)
devtools::install_github("waltervanheuven/strsim")
library(strngrams)
```

## Usage

```R
library(readxl)

# download SUBTLEX-UK database (van Heuven et al., 2014) that contains a large list of English words
download.file(url='https://psychology.nottingham.ac.uk/subtlex-uk/SUBTLEX-UK.xlsx.zip',
              destfile='SUBTLEX-UK.xlsx.zip',
              method='curl')
# unzip file              
unzip("SUBTLEX-UK.xlsx.zip")
# load xlsx
subtlex_uk <- read_xlsx("SUBTLEX-UK.xlsx")

# Orthographic neighbours of the word book
coltheartN("book", subtlex_uk$Spelling, show=T)

# OLD20 value and the 20 orthographic closest words of the word book
# based on the Levenshtein distance
old20("book", subtlex_uk$Spelling, show=T)

# calculate OLD20 for a list of words
calculate_old20(c("room", "tree"), subtlex_uk$Spelling)

# calculate_old20 function utilises multiple cores to improve performance
# when calculating old20 for large number of words, e.g.
old20 <- calculate_old20(subtlex_uk$Spelling, subtlex_uk$Spelling)
```
