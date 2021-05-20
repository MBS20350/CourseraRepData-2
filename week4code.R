## Reproducible Research Week Four Project
## Mark Sucato

library(tidyverse)
library(knitr)
library(RColorBrewer)
library(cowplot)

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
download.file(fileURL, "repdata_data_StormData.csv.bz2")
downloadTime <- Sys.time()
stormData <- read.csv("repdata_data_StormData.csv.bz2")

# Across the United States, which types of events (as indicated in the
#   EVTYPE variable) are most harmful with respect to population health?

sum(is.na(stormData$EVTYPE) == 1) +
	sum(is.na(stormData$FATALITIES) == 1) +
	sum(is.na(stormData$INJURIES) == 1)

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
    
top10casualty <- head(stormCasualty, 10)
kable(top10casualty)

casualty <- ggplot(top10casualty, aes(x = reorder(EVTYPE, -Total), y = Total, fill = EVTYPE)) +
	theme_bw() +
	scale_fill_brewer(palette = "Spectral") +
	geom_bar(stat = "identity") +
	labs(x = "Event Type", y = "Total Casualties") +
	labs(title = "Top 10 Most Harmful Weather Event Categories") +
	labs(subtitle = "Total Injuries and Deaths") +
	theme(legend.title = element_blank()) +
	theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
	theme(legend.position = "bottom")

# Across the United States, which types of events have the greatest economic
#   consequences?

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

top10Damage <- head(stormDamage, 10)
kable(top10Damage)

damage <- ggplot(top10Damage, aes(x = reorder(EVTYPE, -Total), y = Total, fill = EVTYPE)) +
	theme_bw() +
	scale_fill_brewer(palette = "Paired") +
	geom_bar(stat = "identity") +
	labs(x = "Event Type", y = "Total Damage ($ Trillion)") +
	labs(title = "Top 10 Most Economically Damaging Weather Event Categories") +
	labs(subtitle = "Total Property & Crop Damage") +
	theme(legend.title = element_blank()) +
	theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
	theme(legend.position = "bottom")

plot_grid(casualty, damage)


