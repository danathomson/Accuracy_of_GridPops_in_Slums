# Accuracy_of_GridPops_in_Slums


The datasets and code in this repository correspond papers published by Dana R Thomson and colleauges about the accuracy of gridded population datasets in "slums" and other deprived urban areas.

* **Paper 1:** Evaluating the Accuracy of Gridded Population Estimates in Slums: A Case Study in Nigeria and Kenya (2021): https://doi.org/10.3390/urbansci5020048
* **Paper 2:** Improving the accuracy of gridded population estimates in cities and slums to monitor SDG 11: Evidence from a simulation study in Namibia (2022): https://doi.org/10.1016/j.landusepol.2022.106392
* **Paper 3:** Assessing the accuracy of gridded population data in “slums” for SDG11 monitoring using community-generated data across diverse LMIC cities (Forthcoming in 2023)
  * **part1_gen_data.R:** Code to generate gridded and KYC population estimates, by settlement and city. *OUTPUT*=slums_v10.csv
  * **part2_kyc_accuracy.R:** Code to analyze accuracy of gridded population datasets, by settlement and city. *OUTPUT*=error_cause_data.csv
  * **part3_SDG11.R:** Code to genereate SDG 11.1.1 estimates from gridded population estimates vs DHS surveys . *OUTPUT*=sdg.csv
