RUN_NAME = v1.0

documents/marlin-paper.pdf: results/$(RUN_NAME)/experiements.rds results/$(RUN_NAME)/casestudy.rds
	Rscript -e "rmarkdown::render('documents/marlin-paper.Rmd', quiet = T,params = list(run_name = '$(RUN_NAME)'))"

results/$(RUN_NAME)/experiements.rds:
	Rscript scripts/01_run_experiments.R

results/$(RUN_NAME)/casestudy.rds: data/matrices
	Rscript scripts/02_run_case_study.R

