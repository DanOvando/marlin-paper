RUN_NAME = v1.0

documents/marlin-paper.pdf: results/$(RUN_NAME)/compact_experiment_results.rds results/$(RUN_NAME)/optimized_network.rds
	Rscript -e "rmarkdown::render('documents/marlin-paper.Rmd', quiet = T,params = list(run_name = '$(RUN_NAME)'))"

results/$(RUN_NAME)/optimized_network.rds: data/matrices
	Rscript scripts/02_run_case_study.R

results/$(RUN_NAME)/compact_experiment_results.rds:
	Rscript scripts/01_run_experiments.R
