## Testing commands for the two candidate voting functions

## Returned as a vector. The first entry is the power of test, 
## second entry is mean sample size of test

#pT level 0.7, 0.6, 0.55 respectively. pR level 0.55
power_sample_1 = tc_simulation(0.7, 0.55, 100, 0.05)

power_sample_2 = tc_simulation(0.6, 0.55, 100, 0.05)

power_sample_3 = tc_simulation(0.55, 0.55,100, 0.05)

#pT level 0.55. pR level 0.7, 0.6, 0.55 respectively

power_sample_4 = tc_simulation(0.55, 0.7, 100, 0.05)

power_sample_5 = tc_simulation(0.55, 0.6, 100, 0.05)

power_sample_6 = tc_simulation(0.55, 0.55,100, 0.05)