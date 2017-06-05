# Dot Map Plus

This repository documents my efforts to improve upon dot map visualizations. The 'analysis' folder includes code examples that will download all the data and shapefiles 

Dot maps excel at communicating granularity in a spatial context. Dot aesthetics can be color coded, size adjusted, and shape adjusted to communicate a range of variables. Despite attractive flexibility, they also present several interpretation challenges.

1. Dots can stack or overlap, making density and aesthetics difficult to perceive.
2. Because stock visualization software frequently draw dots in order of factored categories, the later factor levels are more prominent than earlier levels leading to misinterpretation.
3. It is difficult to comprehend how many dots are being viewed as the number increases. 10 vs 100 may be possible, but what about 1 million vs 10 million?
4. It's difficult for humans to compare the relative share of dots belonging to different categories.
5. Color blindness add interpretation difficulties.

## Conceptualizing What Dot Aesthetics Can Communicate

The most basic dot map will have one dot for each unit of some dataset.

One dot could equal:

* One person
* One household
* One city
* One business
* One event

A minimalist dot map shows spatial distribution and frequency. Dot aesthetics encode additional information, but at the cost of increased complexity.

### Color

* Different hues to convey a categorical variable
* Different shades to convey an ordinal or continuous variable

### Size

* Scale size to convey a continuous variable

### Shape

* Different shapes to convey a categorical variable

