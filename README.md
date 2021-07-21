# weighted-likelihood

The goal of this repository is to explore options for producing unbiased population estimates using weighted survey data, including a full report describing the methods and findings (Leasure et al 2021). The published report with supplementary data is available from <a href="https://doi.org/10.5258/SOTON/WP00706" target="_blank">https://doi.org/10.5258/SOTON/WP00706</a>.

This repository includes:
1. code to simulate population counts in x units
2. code simulate sampling from the population using weighted or random sampling
3. code to fit models (stan and jags) to the sample data using weighted-likelihood, weighted-precision, and unweighted approaches
4. code to assess results with figures and summary statistics
5. a report to present the findings

Original idea for weighted likelihood approach came from this post by Martyn Plummer on the JAGS forum:  
https://sourceforge.net/p/mcmc-jags/discussion/610037/thread/50365933/

Further discussion from Carpenter, Goodrich, and Gelman on the Stan Forum:  
https://discourse.mc-stan.org/t/survey-weighted-regression/1654

A relevant paper from Gelman:  
https://projecteuclid.org/euclid.ss/1190905511
