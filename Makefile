03_marlin_paper.pdf: results/$(RUN_NAME)/blue_water_experiments.rds results/$(RUN_NAME)/coral_mpa_experiments.rds
	quarto render 03_marlin_paper.qmd -P run_name:$(RUN_NAME) --to $(FORMAT)

results/$(RUN_NAME)/blue_water_experiments.rds: data/species-matrices
	Rscript scripts/02_blue_water_case_study.R

results/$(RUN_NAME)/coral_mpa_experiments.rds:
	Rscript scripts/01_coral_case_study.R
