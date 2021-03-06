---
title: "Severe Weather:  Top Ten Most Dangerous & Damaging"
author:  Mark Sucato
output: 
  html_document:
    keep_md: true
---

# Severe Weather:  Top Ten Most Dangerous & Damaging

## Author:  Mark Sucato

### Synopsis

Analysis of the U.S. National Oceanic and Atmospheric Administration's (NOAA) *Storm Data* database of severe
 weather events recorded between 1950 to November 2011 indicates tornados present the most dangerous threat to
humans for both injury and death.  Total casualties from tornados exceed the combined total casualties of every
 other type of weather in the top ten most dangerous types of weather. Similar analysis indicates hail presents
the greatest potential economic damage.  This damage is overwhelming dominated by crop damage.  Economic losses 
from crop damage wildly exceed those from property damage for every type of weather in the top ten most damaging
types of weather.   

### Data Processing

This analysis examines the U.S. National Oceanic and Atmospheric Administration's (NOAA) *Storm Data* database
 of severe weather events recorded between 1950 to November 2011. *Storm Data* includes data collected from a 
variety of sources, including the media, law enforcement and/or other government agencies, private companies, 
and individuals.  While *Storm Data* attempts to record events as accurately as possible, the National Weather 
Service does not guarantee the accuracy or validity of the data.  

The dataset is available via NOAA website.  For this analysis, a separate copy maintained by Coursera was used 
for all analysis.


```r
library(tidyverse)
library(knitr)
library(RColorBrewer)
library(cowplot)
library(kableExtra)

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, "repdata_data_StormData.csv.bz2")
downloadTime <- Sys.time()
stormData <- read.csv("repdata_data_StormData.csv.bz2")
```

The Coursera-maintained dataset can be found at https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2 .  The file used for this analysis was downloaded on
2021-05-20 13:55:59. 

A cursory examination of the *Storm Data* weather event categorizations reveals numerous examples of the same 
type of event described in different terms.  For example, *heat* events could be described as *heat*, *extreme
heat*, *excessive heat*, *heat wave* or *record heat* potentially in both upper and lower case letters.  This 
analysis attempts to group all events pertient to the top ten most dangerous or damaging types of weather.  For
 both dangerous and damaging weather events, the data is grouped by event type and then summed.  In the case of 
dangerous weather events, totals for injuries, deaths and casualties, defined here as either injury or death,
are calculated. In the case of damaging weather events, totals for property damage, crop damage and total
damage are calculated.  For the damaging data, the raw data is adjusted for a thousands, millions or billions 
explanatory variable and then recalculated in trillions of dollars for presentation purposes. 


```r
stormData1 <- stormData %>%
	mutate(EVTYPE = recode(EVTYPE, "EXCESSIVE HEAT" = "HEAT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "EXTREME HEAT" = "HEAT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "HEAT WAVE" = "HEAT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "Heat Wave" = "HEAT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "RECORD HEAT" = "HEAT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "RIP CURRENTS" = "RIP CURRENT")) %>%
	mutate(EVTYPE = recode(EVTYPE, "THUNDERSTORM WIND" = "TSTM WIND")) %>%
	mutate(EVTYPE = recode(EVTYPE, "THUNDERSTORM WINDS" = "TSTM WIND")) %>%
	mutate(EVTYPE = recode(EVTYPE, "ICE STORM" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "BLIZZARD" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "HEAVY SNOW" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WINTER STORM" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WINTER WEATHER/MIX" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WINTERY MIX" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WINTRY MIX" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WINTER WEATHER MIX" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "EXTREME COLD/WIND CHILL" = "EXTREME COLD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "COLD/WIND CHILL" = "EXTREME COLD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "COLD" = "EXTREME COLD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "GLAZE" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "ICE" = "WINTER WEATHER")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WILD/FOREST FIRE" = "WILDFIRE")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WILD FIRES" = "WILDFIRE")) %>%
	mutate(EVTYPE = recode(EVTYPE, "HIGH WINDS" = "HIGH WIND")) %>%
	mutate(EVTYPE = recode(EVTYPE, "STRONG WIND" = "HIGH WIND")) %>%
	mutate(EVTYPE = recode(EVTYPE, "WIND" = "HIGH WIND")) %>%
	mutate(EVTYPE = recode(EVTYPE, "DENSE FOG" = "FOG")) %>%
	mutate(EVTYPE = recode(EVTYPE, "HEAVY SURF/HIGH SURF" = "HIGH SURF")) %>%
	mutate(EVTYPE = recode(EVTYPE, "FLOODING" = "FLOOD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "RIVER FLOOD" = "FLOOD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "FLASH FLOODING" = "FLASH FLOOD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "URBAN/SML STREAM FLD" = "FLOOD")) %>%
	mutate(EVTYPE = recode(EVTYPE, "HURRICANE/TYPHOON" = "HURRICANE"))

stormCasualty <- stormData1 %>%
	mutate(Casualties = INJURIES + FATALITIES) %>%
	group_by(EVTYPE) %>%
	summarise(Total = sum(Casualties), Injured = sum(INJURIES),
		Killed = sum(FATALITIES)) %>%
	arrange(desc(Total))    

costCodes <- c("k", "K", "m", "M", "b", "B")
kCode <- c("k", "K")
mCode <- c("m", "M")
trueCost <- function(money, code) {
	if (code %in% kCode)
		return(money * 1000)
	else if (code %in% mCode)
		return(money * 1000000)
	else return(money * 1000000000)
}
stormDamage <- stormData1 %>%
	filter(PROPDMGEXP %in% costCodes | CROPDMGEXP %in% costCodes) %>%
	filter(PROPDMG + CROPDMG > 0) %>%
	mutate(propertyDamage = trueCost(PROPDMG, PROPDMGEXP)) %>%
	mutate(cropDamage = trueCost(CROPDMG, CROPDMGEXP)) %>%
	mutate(totalDamage = propertyDamage + cropDamage) %>%
	group_by(EVTYPE) %>%
	summarise(Total = sum(totalDamage)/1E12, Property = sum(propertyDamage)/1E12,
		Crops = sum(cropDamage)/1E12) %>%
	arrange(desc(Total))
 
top10casualty <- head(stormCasualty, 10)
top10Damage <- head(stormDamage, 10)
```
 
### Results

The top ten most dangerous and/or damaging weather events are shown below in **Figures 1** and **2**.  In
the case of most dangerous weather, casualties from tornados exceed the combined total casualties from all other 
types of weather in the top ten most dangerous weather events.  In the case of most damaging weather, 
damage from hail far exceeds any other event in the top ten.   


```r
kable(top10casualty, col.names = c("Event Type", "Total Casualties", "Injured", "Killed"), 
	caption = "Figure 1: Most Dangerous Weather") %>%
		kable_styling(full_width = FALSE, position = "float_left")
kable(top10Damage, col.names = c("Event Type", "Total Damage ($T)", "Property Damage($T)",
	 "Crop Damage ($T)"), digits = 5, caption = "Figure 2: Most Damaging Weather") %>%
		kable_styling(full_width = FALSE, position = "left")
```

<table class="table" style="width: auto !important; float: left; margin-right: 10px;">
<caption>Figure 1: Most Dangerous Weather</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Event Type </th>
   <th style="text-align:right;"> Total Casualties </th>
   <th style="text-align:right;"> Injured </th>
   <th style="text-align:right;"> Killed </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 96979 </td>
   <td style="text-align:right;"> 91346 </td>
   <td style="text-align:right;"> 5633 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HEAT </td>
   <td style="text-align:right;"> 12319 </td>
   <td style="text-align:right;"> 9209 </td>
   <td style="text-align:right;"> 3110 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 10054 </td>
   <td style="text-align:right;"> 9353 </td>
   <td style="text-align:right;"> 701 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 7378 </td>
   <td style="text-align:right;"> 6872 </td>
   <td style="text-align:right;"> 506 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WINTER WEATHER </td>
   <td style="text-align:right;"> 6688 </td>
   <td style="text-align:right;"> 6090 </td>
   <td style="text-align:right;"> 598 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LIGHTNING </td>
   <td style="text-align:right;"> 6046 </td>
   <td style="text-align:right;"> 5230 </td>
   <td style="text-align:right;"> 816 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 2782 </td>
   <td style="text-align:right;"> 1785 </td>
   <td style="text-align:right;"> 997 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HIGH WIND </td>
   <td style="text-align:right;"> 2214 </td>
   <td style="text-align:right;"> 1805 </td>
   <td style="text-align:right;"> 409 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WILDFIRE </td>
   <td style="text-align:right;"> 1696 </td>
   <td style="text-align:right;"> 1606 </td>
   <td style="text-align:right;"> 90 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HURRICANE </td>
   <td style="text-align:right;"> 1446 </td>
   <td style="text-align:right;"> 1321 </td>
   <td style="text-align:right;"> 125 </td>
  </tr>
</tbody>
</table>

<table class="table" style="width: auto !important; ">
<caption>Figure 2: Most Damaging Weather</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> Event Type </th>
   <th style="text-align:right;"> Total Damage ($T) </th>
   <th style="text-align:right;"> Property Damage($T) </th>
   <th style="text-align:right;"> Crop Damage ($T) </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> HAIL </td>
   <td style="text-align:right;"> 579.57697 </td>
   <td style="text-align:right;"> 0.00069 </td>
   <td style="text-align:right;"> 579.57628 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TSTM WIND </td>
   <td style="text-align:right;"> 194.68163 </td>
   <td style="text-align:right;"> 0.00265 </td>
   <td style="text-align:right;"> 194.67898 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLASH FLOOD </td>
   <td style="text-align:right;"> 184.32796 </td>
   <td style="text-align:right;"> 0.00145 </td>
   <td style="text-align:right;"> 184.32651 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> FLOOD </td>
   <td style="text-align:right;"> 177.68363 </td>
   <td style="text-align:right;"> 0.00095 </td>
   <td style="text-align:right;"> 177.68268 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TORNADO </td>
   <td style="text-align:right;"> 100.02173 </td>
   <td style="text-align:right;"> 0.00321 </td>
   <td style="text-align:right;"> 100.01852 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DROUGHT </td>
   <td style="text-align:right;"> 33.89862 </td>
   <td style="text-align:right;"> 0.00000 </td>
   <td style="text-align:right;"> 33.89862 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HIGH WIND </td>
   <td style="text-align:right;"> 20.96016 </td>
   <td style="text-align:right;"> 0.00045 </td>
   <td style="text-align:right;"> 20.95971 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HEAVY RAIN </td>
   <td style="text-align:right;"> 11.12285 </td>
   <td style="text-align:right;"> 0.00005 </td>
   <td style="text-align:right;"> 11.12280 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HURRICANE </td>
   <td style="text-align:right;"> 10.13781 </td>
   <td style="text-align:right;"> 0.00002 </td>
   <td style="text-align:right;"> 10.13779 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> WILDFIRE </td>
   <td style="text-align:right;"> 8.55386 </td>
   <td style="text-align:right;"> 0.00012 </td>
   <td style="text-align:right;"> 8.55374 </td>
  </tr>
</tbody>
</table>

```r
casualty <- ggplot(top10casualty, aes(x = reorder(EVTYPE, -Total), y = Total, fill = EVTYPE)) +
	theme_bw() +
	scale_fill_brewer(palette = "Spectral") +
	geom_bar(stat = "identity") +
	labs(x = "Event Type", y = "Total Casualties") +
	labs(title = "Figure 3a: Top 10 Most Harmful Weather Event Categories") +
	labs(subtitle = "Total Injuries and Deaths") +
	theme(legend.title = element_blank()) +
	theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
	theme(legend.position = "bottom")

damage <- ggplot(top10Damage, aes(x = reorder(EVTYPE, -Total), y = Total, fill = EVTYPE)) +
	theme_bw() +
	scale_fill_brewer(palette = "Paired") +
	geom_bar(stat = "identity") +
	labs(x = "Event Type", y = "Total Damage ($ Trillion)") +
	labs(title = "Figure 3b: Top 10 Most Economically Damaging Weather Event Categories") +
	labs(subtitle = "Total Property & Crop Damage") +
	theme(legend.title = element_blank()) +
	theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
	theme(legend.position = "bottom")

plot_grid(casualty, damage)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)
 
Shown graphically in **Figure 3**, the difference in magnitude of tornado casualties relative to other weather 
events is especially striking. Similarly, the differences in magnitude between the top five most damaging events 
reative to the next five is notable.  

### Conclusions and Recommendations

Deaths and injuries far exceed those from any other weather event, providing ample justification to continuing 
efforts to both improve building codes for structures in tornado-susceptible areas and develop more effective tornado
warning systems.  

While similar disparities in magnitude exist for the most damaging weather data, the splits between property and
crop damage shown in **Figure 2** are especially large.  While pictures and stories featuring property damage
typically dominate news reporting of severe weather events, crop damage totals wildly exceed those from property 
damage.  

Noted earlier, this analysis attempts to account for titling inaccuracies in the most dangerous and damaging types
of weather.  This effort was a "80% solution" at best.  A more rigorous attempt to simplify and standardize event
titles would likely improve future analyses.  Seemingly such an effort could occur either through more standardized
reporting techniques like drop-down menu options or more complete post-collection data wrangling. 
