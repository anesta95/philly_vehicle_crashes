# Philly Vehicle Crash Analysis

This repo contains the code used to do an analysis of where vehicle
crashes in the city of Philadelphia have changed from pre-pandemic years of 
2016 to 2019 compared to 2020 to 2023.

According to [`Reportable Crash Statistics`](https://crashinfo.penndot.pa.gov/PCIT/welcome.html) 
data from the PA Department of Transportation, total crashes in Philly have 
dropped off over 12 percent from the '16 to '19 average compared to the '20 to '23 average.
Crashes with reported injuries have also declined from an average of 8008.5 in 2016 - 2019
to 6023.5 in 2020 to 2023, an almost 25% reduction.

The `crash_analysis.R` file uses crash data from the PA Department of Transportation
via their [custom query tool](https://crashinfo.penndot.pa.gov/PCIT/queryTool.html)
to look at what Census tracts in Philly saw the most declines in injury-related
vehicle crashes per capita between '16 and '16 compared to '20 and '23.

Other data analysis shows that motorcycle crashes were the only major
category of vehicle crashes that increased from averages before the 
COVID-19 pandemic to after. A cluster grouping of crashes with an injury or 
fatality that involved a motorcycle in [QGIS](https://qgis.org/)
revealed which parts of Philadelphia were the most dangerous for motorcycle
riders in the map `philly_motorcycle_injury_fatal_accident_20_23.png`.

This data analysis was used to create these two [Datawrapper](https://www.datawrapper.de/)
[chloropleth](https://www.datawrapper.de/_/ptCWL/?v=2) and [locator](https://www.datawrapper.de/_/0EeMD/) maps to visualize
this analysis.
