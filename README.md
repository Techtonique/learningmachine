# learningmachine

**R version**

[![learningmachine status badge](https://techtonique.r-universe.dev/badges/learningmachine)](https://techtonique.r-universe.dev/learningmachine) [![CodeFactor](https://www.codefactor.io/repository/github/techtonique/learningmachine/badge)](https://www.codefactor.io/repository/github/techtonique/learningmachine) [![HitCount](https://hits.dwyl.com/Techtonique/learningmachine.svg?style=flat-square)](http://hits.dwyl.com/Techtonique/learningmachine) 


## Note to self or developers 

```R
# refactor
devtools::check(".", vignettes=FALSE)
#cmd1 <- "Rscript -e try(utils::remove.packages(learningmachine), silent=TRUE)"
cmd2 <- "Rscript -e devtools::install('.')"
cmd3 <- "Rscript -e devtools::load_all('.')"
system2(c("R", cmd2, cmd3))
```

