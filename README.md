
# vlaplr

Vlaplr is an interface to the API of the Flemish parliament.

## Installation

You can install the released version of vlaplr from this repo with:

``` r
require(devtools)
install_github("datamarinier/vlaplr")
```

## Usage

Everything is structured around the plenary sessions of parliament. The
first step is to select the plenary sessions that occurred during a
certain period. This can be done with the function search\_plenary. This
function returns a plenary\_object (a dataframe) with all the necessary
variables to describe the activities in that session (e.g. debates or
parliamentary questions). With this object the function search\_terms
enables you to search all the spoken words in the Flemish Parliament.

To boost performance it is possible to divide the calls over several
workers, enabling to shorten your waiting time.

``` r
library(vlaplr)
procedure_object <- search_plenary(date_range_from = "2021-01-01",
                                      date_range_to= "2021-03-31",
                                      use_parallel=TRUE)
                                      
search_terms(procedure_object = procedure_object,
             search_terms =  c("statistiek","welzijn"),
             type = c("all"),
             use_parallel=TRUE )
             
#or when you prefer to use pipes:

search_plenary("2021-01-01", "2021-03-31",  use_parallel=TRUE) %>%
  search_terms(c("statistiek","welzijn"), c("all"), use_parallel=TRUE )
```
