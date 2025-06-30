# Reproduction Code for:  
**"Efficient Electrification and a Warming Climate Could Act Together to Keep Energy Burdens in Check"**

This repository contains the code used to reproduce the results presented in the paper:  
*"Efficient Electrification and a Warming Climate Could Act Together to Keep Energy Burdens in Check."*

---

## Overview of Scripts

### Energy Consumption Simulation (Four Scenarios)

The following R scripts simulate household electricity consumption under four combinations of HVAC systems and climate scenarios:

- `Step3a_CurrentHVAC_HistoricalWeather.R`  
- `Step3b_CurrentHVAC_FutureWeather.R`  
- `Step3c_FutureHVAC_HistoricalWeather.R`  
- `Step3d_FutureHVAC_FutureWeather.R`  

**Scenarios Modeled:**
- Current HVAC with historical climate  
- Current HVAC with future climate  
- Future HVAC with historical climate  
- Future HVAC with future climate  

---

### Data Preparation for IPF

- `Read_MicroData.R`: Reads American Housing Survey (AHS) microdata to initialize the Iterative Proportional Fitting (IPF) method.  
- `Process_CSV.R`: Processes American Community Survey (ACS) data to calculate marginal category probabilities.  
- `IPF_Probability_Table.py`: Performs multi-dimensional IPF to generate the final joint probability table.

---

### Energy Burden Calculation
CHHW: Current HVAC with Historical Weather;  
CHFW: Current HVAC with Future Weather;  
FHHW: Future HVAC with Historical Weather;  
FHFW: Future HVAC with Future Weather;  


- `Stepn_Energy_Burden_CHFW.R`:  
  Computes the annual electricity bill for each household based on the fitted probability table.  
  Upsamples the dataset to create a simulated population of 10,000 households.

- `Stepn_Energy_Burden_CHFW12months.R`:  
  Calculates monthly energy burdens.  
  Also upsamples to a 10,000-household simulated population.

---

### Visualization Scripts

- `Plot_Box_2month_violin.R`:  
  Generates violin plots of monthly energy burdens for **July** and **January** (as shown in the paper).

- `Plot_Box_2month_Top10_violin.R`:  
  Plots monthly energy burdens for the **top 10% highest-burden households** in **July** and **January**.

- `Plot_Box_1year_violin.R`:  
  Creates violin plots for **annual energy burdens**.

---

## Contact

For questions about this code, please contact yiminghit@gmail.com.
