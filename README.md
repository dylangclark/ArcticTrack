# ArcticTrack
Modelling land use change in Inuit Nunangat
Methodology – Projecting changes in land-use days for Inuit Nunangat through 2100


FINAL RESULTS AVAILABLE ON SHINY HERE: https://climatechoices.shinyapps.io/ArcticTrack/


Objectives
•	Assess how climate change may impact the viability/amount of days that people are able to travel on the land, ice, and water across Inuit Nunangat
•	Better understand how the skill level of land users implicates their sensitivity to changes in trail conditions
•	Assess where changes in trail use may be happening quickest and how global emissions scenarios will change impacts
•	Assess how travel may be pushed to shift between trail types as environmental conditions change
Approach
Task 1: Develop daily timeseries of weather and ice conditions
Assumptions/Limitations
a)	Visibility is going to shift over the next century, however, we do not know how. The relationship between visibility and other climatic shifts will be highly local. Yet, visibility is important it is important to show what influence variation will have. Therefore, we do not attempt to augment visibility over time. There are no visibility climate projections available.
b)	Ice and weather conditions can vary substantially over small distances. Due to the nature of climate modelling and because at the time of this study ECCC has not produced downscaled global climate models for the required data, we had to make generalizations about the conditions over large areas

Data sources
c)	Visibility: Synthetic timeseries – we randomly selected daily visibility values from the 30 years of data from each region. The visibility timeseries is static for all GCMs and both emissions scenarios
d)	Temp, precip, wind, and ice: CMIP6 GCM data – data from 5 GCMs was available for all of the variables.




Data preparation and timeseries creation
e)	Daily temperature, precipitation, and wind timeseries based on average of all GCM grid cell within 100km radius of each community
i)	Separate timeseries for each GCM (5) and RCP scenario (2)
f)	Static visibility timeseries for each region based on a historic timeseries (length dependent on historic weather availability, no less than 5 year average & no greater than 30 year avg)
i)	For each region, up to a decade of visibility data will be gathered from all communities. Years with poor data will be removed. 
ii)	Using a random generator, we created a synthetic timeseries using the historic observations. We compared the histogram between historic and the generated visibility data to confirm similar distributions. Note that this approach assumed no autocorrelation.
g)	Sea ice concentration and sea ice thickness data came from CMIP6 gridded data. In order to ensure that there were enough points of observation for each region, the grid area was extended to a 150km radius around each community (see appendix map)
i)	Grid cells sampled for regions:
(1)	
Task 2: Model the number of ‘good’ and ‘bad’ days for various land users and trail types
Assumptions/Limitations
a)	Future thresholds will not change from the present-day ranges
b)	Small adjustments were made to the thresholds used in Ford, 2019 to accommodate the change in sea ice area (Ford, 2019 used point specific sea ice data) – these criteria used are in appendix 1
Adapting criteria from Ford, 2019
c)	We will run the IF/THEN statement through the timeseries that is developed for each community (10 timeseries * 7 regions)
d)	We will run 3 IF/THEN statements for each of the different ‘land user types’
Task 3: Analyze temporal and spatial trends
Assumptions/Limitations
a)	Each GCM is internal to itself – a daily temperature produced by CanESM2 RCA4 is incongruous with the precipitation projected by another model that day
b)	To look at changes over time, we need to aggregate to decadal periods, however, we need to analyze ‘good’ and ‘bad’ days first, otherwise the extremes will get averaged out.
Timeseries analysis
c)	We will examine the high- and low-end estimates for each emissions scenario (18 runs for each RCP) and each Land User Type
d)	‘Good days’ and ‘bad days’ are be tabulated for each region, high-end estimate, low-end estimate, emission scenario, and Land User Type for each year (2010 to 2100).
e)	‘Good days’ and ‘bad days’ will be averaged for 10 year periods for each community
f)	Trends will be examined by community, then aggregated into high and low bound estimates for each RCP and Land User Type by region
