UMSP Data Summary and Variable Generation

This is a description of the variables generated from the UMSP dataset for analysis. The dataset includes key metrics such as malaria incidence, proportions of suspected malaria cases, and testing rates over different regions and months. The following steps outline the process of calculating and summarizing these variables.
1. Data Loading and Preprocessing
The UMSP dataset was loaded from a Stata (.dta) file located on the desktop. The following parameters were used to load the data: The dataset was also preprocessed to ensure the 'Region' variable was treated as a factor, and a list of the unique levels of 'Region' was generated.
2. Data Grouping and Calculation of Variables
The data was grouped by 'Region' and 'monthyear', and the following new variables were generated using aggregation functions:
a) Malaria Incidence per 1000: This variable was calculated as the mean of the 'MI1000' variable for each region and monthyear. The result provides the average number of malaria cases per 1000 population within each grouping. The 'na.rm = TRUE' argument ensures that missing values are ignored during the calculation.
b) Proportion Suspected Malaria: This variable is the ratio of 'malariasuspected' to 'visits' for each region and monthyear. It represents the proportion of visits that were suspected to be malaria cases.
c) Proportion Suspected Malaria Tested: This variable was computed as the ratio of 'TPRdenom' to 'malariasuspected' for each region and monthyear. It indicates the proportion of suspected malaria cases that were tested.
d) Proportion Tested with RDT: This variable was calculated by dividing the sum of 'RDT' by 'TPRdenom' for each region and monthyear. It represents the proportion of tests conducted using Rapid Diagnostic Tests (RDTs) among all malaria tests.

3. Formulas
3.1. Malaria Incidence per 1000
Formula: 
Malaria Incidence per 1000 = mean(MI1000)
Where MI1000 is the malaria incidence rate per 1000 individuals.
3.2. Proportion Suspected Malaria
Formula: 
Proportion Suspected Malaria = sum(malariasuspected) / sum(visits)
Where `malariasuspected` is the number of suspected malaria cases, and `visits` is the total number of patient visits.
3.3. Proportion Suspected Malaria Tested
Formula: 
Proportion Suspected Malaria Tested = sum(TPRdenom) / sum(malariasuspected)
Where `TPRdenom` is the number of tests performed for suspected malaria cases, and `malariasuspected` is the number of suspected malaria cases.
4. Data Renaming and Ordering
The newly calculated variables were renamed to improve readability. The following renaming was performed:
a) 'Malaria_Incidence_Per_1000' was renamed to 'Malaria Incidence per 1000'.
b) 'Proportion_Suspected_Malaria' was renamed to 'Proportion Suspected Malaria'.
c) 'Proportion_Suspected_Malaria_Tested' was renamed to 'Proportion Suspected Malaria Tested'.
d) 'Proportion_Tested_With_RDT' was renamed to 'Proportion Tested with RDT'.
Finally, the data was arranged by 'Region' and 'monthyear' to facilitate better visualization and analysis of trends.
