#!/bin/bash
#SBATCH --mail-type=ALL
#SBATCH --mail-user=ieva.skarda@york.ac.uk
#SBATCH --time=2:00:00
#SBATCH  --mem=2G 
#SBATCH  -o out/out_%j.log
#SBATCH  -e err/err_%j.log
#SBATCH --job-name=LifeSim 
#SBATCH --ntasks=1                             
#SBATCH --cpus-per-task=1                      
#SBATCH --account=che-microee-2020
#SBATCH --array=1-500
Rscript Run_pol_all.R ${SLURM_ARRAY_TASK_ID}