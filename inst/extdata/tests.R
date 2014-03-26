options()[c("default.datadir", "repo.config")]
## valid datadir, and missing config
##$default.datadir
##[1] "/etc/repo/data"

##$<NA>
##NULL

library(repomania)
options()[c("default.datadir", "repo.config")]
## valid datadir as before, valid config

## $default.datadir
## [1] "/etc/repo/data"

## $repo.config
## [1] "/etc/R/R/library/repomania/extdata/raad_repo_config.json"


