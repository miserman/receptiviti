# Receptiviti
An R package to process text with the [Receptiviti](https://www.receptiviti.com) API.

## Installation
Download R from [r-project.org](https://www.r-project.org), then install the package from an R console:

```R
# install.packages('remotes')
remotes::install_github('miserman/receptiviti')
```

And load the package:
```R
library(receptiviti)
```

## API Access
To access the API, you will need to load your key and secret, as found on your [dashboard](https://dashboard.receptiviti.com).

You can enter these as arguments in each function, but by default they will be looked for in these environment variables:
```sh
RECEPTIVITI_KEY="32lettersandnumbers"
RECEPTIVITI_SECRET="56LettersAndNumbers"
```

You can store these in your R environment file permanently:
```R
# opens ~/.Renviron; after editing, save and restart R
usethis::edit_r_environ()
```

Or set them temporarily:
```R
Sys.setenv(
  RECEPTIVITI_KEY = "32lettersandnumbers",
  RECEPTIVITI_SECRET = "56LettersAndNumbers"
)
```
