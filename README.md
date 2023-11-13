# 3spire_socialsimulationconference
This repository contains the model I used for the paper published proceeding the Social Simulation Conference 2023, and the analyses I ran on the model output data.  
  
The model: 20230722SSCversion.nlogo  
The analysis: Figures_and_tables.R  

The model uses the data in the Netlogo_Input folder.
The analysis is run on data in the Netlogo_Output folder. **Be aware** that if you run the model with the same parameters (b = 0.45, optimism = 1.05), the original output file used to create the results presented in the paper will be deleted. 

### Selection and calibration of the aspiration adaptation formula
Since we have no time series data with aspiration data, we had to make assumptions about how aspirations would adapt over time. To simulate the adaptation of aspirations over time, then, we needed data on initial aspirations, a formula determining how aspirations adapt over time (per timestep) and values for the parameters in that formula. 


Initial aspirations were collected through household surveys, as described in the conference proceeding: https://ssc23-sphsu.online/wp-content/uploads/2023/09/SSC2023_paper_64.pdf. The formula determining how aspirations adapt over time was based on an empirical exploration of aspirations over time by Theresa K. Lant (1992):  https://www.jstor.org/stable/2632681. She concluded that: “The aspiration formation process seems to be best described as a process of adjustment in response to past aspirations and past performance, suggesting that adaptive, history dependent models are more accurate descriptors of aspiration formation than a rational model of expectation formation. […].”. The formula she found to describe the process best was:  

Yt = α0 + α1Yt-1 + α2[Zt-1 – Yt-1] + є  

where Yt is current aspiration, Yt-1 is last period’s aspiration, and Zt-1 is the actual performance achieved in the last period. She also conclude that: “There is also evidence of systematic bias in the aspiration formation process, with a tendency toward optimism. This tendency to set aspirations consistently higher than actual performance was indicated by the prevalence of a positive constant term and the tendency for average aspirations to exceed average performance”. We therefore renamed one of the constant terms in the formula that could capture this bias optimism.


For an aspirational dimension a, the current (t = t) aspiration level ALt thus is calculated as following:  

ALa t = optimism * ALa t-1 – b*AGa t-1  

where ATa t (Yt in Lant’s formula) is the aspiration threshold of an aspirational dimension a at time t, AGa t-1 is the aspiration gap, i.e. the difference between the aspired threshold and the actual outcome along an aspirational dimension a (Zt-1 – Yt-1 in Lant’s formula), optimism is a variable that determines how optimistic (or pessimistic) household heads are (α1 in Lant’s formula), and b is a constant that reflects how much of the aspiration gap should be closed each year (α2 in Lant’s formula). The constant term α0 is omitted (set to zero) in this formula for the sake of simplicity, but this assumption should be revisited in later revisions of the model. If time series data are available, then α0 , optimism and b should be calibrated to reflect local aspiration adaptation patterns. In the absence of such data, however, we choose to do a pattern oriented calibration where we minimised the occurrence of extended illogical or extreme adaptation patterns. We did so by checking for the aspiration phenomena that are not strange in and of themselves when they happen once, but do become strange when they occur again and again across consecutive timesteps:  

1)	ALa t < ALa t-1 & ALa t < AOa t-1  

2)	 ALa t > ALa t-1 & ALa t < AOa t-1  

3)	ALa t = ALa t-1  

4)	ALa t > AOa t-1  

5)	ALa t > max ( ALa 0)  

6)	ALa t > min ( ALa 0)  


where AOa t-1 is the performance achieved along aspiration dimension a at time t – 1.


To find the most sensible parameterisations for b and optimism, we ran the model with b ranging from zero (meaning aspiration gap will not close at all) to one (meaning aspiration gap will close in one timestep) with an increment of 0.05, and optimism ranging from 0.5 (meaning agents think they can only do half as well as they did last year) to 1.5 (meaning agents think they can do twice as well as last year) with an increment of 0.05. For each household, the maximum number of consecutive strange phenomena were stored for each combination of b and optimism. The error sum that we aimed to minimise, was the sum of the maximum number of consecutive strange phenomena across households. This error sum was the lowest when b = 0.45 and optimism = 1.05.
