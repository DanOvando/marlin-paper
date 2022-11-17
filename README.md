
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Policy Implications of Coupled Economic and Ecological Dynamics in Aquatic Systems

Paper Introducing the `marlin` package.

## Reproducing results on UNIX machines

This repo is equipped with a `make` file. To reproduce results this way

1.  Make sure you have make installed

2.  Open a terminal and make sure the working directory of the terminal
    is set to the location of this repo

3.  Make sure you have all dependencies installed by opening R and
    running `renv::restor()`

4.  In the terminal, run `RUN_NAME:MyRunName FORMAT:MyFormat make` where
    MyRunName is the name of the folder you want to create to store
    results (e.g. v0.1) and FORMAT is one of pdf, html, docx

5.  Wait for a bit! This command will run all the required scripts and
    knit the paper.
