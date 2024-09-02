# Materials for Parag & Lambert (2024) paper on epidp
This repostory uses [renv](https://rstudio.github.io/renv/articles/renv.html) to ensure that the environment used by someone wishing to rerun the analysis is the same as that used to originally run it. First clone the repository, open up the R project then run:

```r
renv::restore()
```

We use targets to ensure that the data analysis pipeline is reproducible. To rerun our analyses, run the following:

```r
targets::tar_make()
```
