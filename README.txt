This is the code to run the simulation model LifeSim, on which more detals can be found in the manuscript ''LifeSim: A Lifecourse Dynamic Microsimulation Model of the Millennium Birth Cohort in England'' available at https://www.medrxiv.org/content/10.1101/2021.02.12.21251642v2  (further, the LifeSim paper). 

**********************************************************************
**********************************************************************
DESCRIPTION OF THE FILES IN THE LIFESIM SIMULATOR

Run_LifeSim.R - this file contains the code to run the LifeSim simulation; it saves the simulated output in the local directory '\res'. The output is saved either (a) as 500 separate files (each file containing simulation output for 200 individuals) when using high-performance computing (HPC) cluster such as Viking at york.ac.uk, or (b) as a single file (containing output for all 100,000 individuals) when using a standard PC. The user needs to follow the instructions in the code and comment out the relevant lines, depending on whether they use a HPC cluster or standard PC. The user can also set the total of number of individuals to be simulated to any other chosen number.

Format_LifeSim_output.R - this file contains the code to combine the output files saved in '\res' into a single R dataframe -- 
"final_dataset/LifeSim_lives.RData".

run_lifesim.sh - this file contains the bash-script to launch a batch job on the Viking HPC cluster.

'\scenarios\no_policy' - this file contains the code for simulating the lives of the cohort at baseline (assuming no policy). It is not necessary to run this file separately, as it is automatically run by Run_LifeSim.R. 

The folder '\target_data' contains the target datasets (see table 2 in the LifeSim paper for a list of sources).

The folder should also contain the initial dataset file '\target_data\initialdata.R' using data from the Millenium Cohort Study (MCS). Please read below on how to create this file.
**********************************************************************
**********************************************************************
CREATING THE INITAL DATASET FILE

Before running the simulation, the initial dataset file 'initialdata.R' should be created as an R dataframe in the local directory '\target_data\'. This file can be created using data from the MCS (see the Table 1 in the LifeSim  paper).

The MCS dataset  can be dowloaded from the UK Data Service at https://ukdataservice.ac.uk/, and more info  about the data is available at the Centre for Longitudinal Studies MCS website -- https://cls.ucl.ac.uk/cls-studies/millennium-cohort-study/

The dataframe 'initialdata.R' should contain individual level data on the following variables with the specific variable names as the first row:

sex - indicator if child is male
nchild - number of children in the family (sweep 1)
sep -  Household income quintile (sweep 1)
wealth - total parental assets, £ (sweep 5)
amrutter9  - 9-item Rutter malaise inventory score (sweep 1)
mhealth_p2 - indicator if Rutter malaise inventory score is 4 or above (sweep 1)
kessler - 6-item Kessler psychological distress scale score (sweep 3)
income_p - OECD equivalised household income after taxes and benefits, £ (sweep 1)
edu_p2 - Indicator if parent has a university degree (NVQ 4 or above) (sweep 1)
em_wellb - SDQ: Emotional symptoms subscale (sweep 3)
smokes14 - Indicator if child smokes/has smoked at 14
depr_p -  OECD below 60% median poverty indicator 

sdq_cond2 - SDQ conduct problem score (sweep 2)
sdq_cond3 - SDQ conduct problem score (sweep 3)
sdq_cond4 - SDQ conduct problem score (sweep 4)
sdq_cond5 - SDQ conduct problem score (sweep 5)
sdq_cond6 - SDQ conduct problem score (sweep 6)

impact2 - SDQ impact score (sweep 2)
impact3 - SDQ impact score (sweep 3)
impact4 - SDQ impact score (sweep 4)

g2 - congitive skills measure (sweep 2)
g3 - congitive skills measure (sweep 3)
g4 - congitive skills measure (sweep 4)
g5 - congitive skills measure (sweep 5)
g6 - congitive skills measure (sweep 6)

fovwt1 - Overall Weight (inc NR adjustment) single country analyses (sweep 6)

Note: The cognitive skills measure for each sweep is an age-specific common
factor extracted from the various cognitive skills measures in MCS, including the British Ability
Scales II, Bracken School Readiness Assessment, National Foundation for Educational Research
(NFER) Progress in Maths, Cambridge Neuropsychological Test Automated Battery tests and
Applied Psychology Unit.
**********************************************************************
**********************************************************************
STEPS TO EXECUTE THE CODE

The code needs to be executed in the following order:

1)Create the intial dataset file '\target_data\initialdata.R' with the variables constructed using the MCS data as desribed above
2)Run Run_LifeSim.R (using R)
3)Run Format_LifeSim_output.R (using R)

The final output file "final_dataset/LifeSim_lives.RData" will then be generated. 

