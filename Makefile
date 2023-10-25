results/$(RUN_NAME)/marlin_paper_si_$(RUN_NAME).$(FORMAT):  results/$(RUN_NAME)/marlin_paper_$(RUN_NAME).$(FORMAT) results/$(RUN_NAME)/blue_water_experiments.rds results/$(RUN_NAME)/coral_mpa_experiments.rds
	quarto render 04_marlin_paper_si.qmd --no-cache -P run_name:$(RUN_NAME) --to pdf --output marlin_paper_si_$(RUN_NAME).pdf
	mv marlin_paper_si_$(RUN_NAME).pdf results/$(RUN_NAME)/

results/$(RUN_NAME)/marlin_paper_$(RUN_NAME).$(FORMAT): results/$(RUN_NAME)/blue_water_experiments.rds results/$(RUN_NAME)/coral_mpa_experiments.rds
	quarto render 03_marlin_paper.qmd --no-cache -P run_name:$(RUN_NAME) --to $(FORMAT) --output marlin_paper_$(RUN_NAME).$(FORMAT)
	mv marlin_paper_$(RUN_NAME).$(FORMAT) results/$(RUN_NAME)/

results/$(RUN_NAME)/coral_mpa_experiments.rds:
	Rscript 01_coral_case_study.R

results/$(RUN_NAME)/blue_water_experiments.rds:
	Rscript 02_blue_water_case_study.R

