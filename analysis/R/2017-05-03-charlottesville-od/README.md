# Charlottesville Area Commuters

This analysis spawned from my recent work with the University of [Virginia's Weldon Cooper Center for Public Service's Demographics Research Group](http://demographics.coopercenter.org/) looking at neighborhood level population flows. It uses US Census [LEHD origin-destination employment statistics](https://lehd.ces.census.gov/data/), specifically 2014 primary jobs to make sure each dot is one person and each person in the universe can only have one dot. [A blog post with these maps is located here](http://statchatva.org/2017/05/08/visualizing-commuter-flows-in-the-charlottesville-area/).

This script will

1. Download the necessary LODES data, Shapefiles, and other administrative information into an "input" folder in the project directory, creating folders as necessary according to my organization preferences.
2. Identify the top 7 localities contributing to the Charlottesville workforce.
3. Generate a population data set (that I call "actors") from the LODES data. Each row is one person assigned an age category, industry category, income category, Residence category, workplace category, and a random longitude and latitude for work and home locations sampled form within the matched census block geography. This takes a while to run.
4. Create dot maps of the region. One for residence, one for workplace, and 48 for an interpolated commute required for animating. The "camera" list in the script controls the center long/lat point and zoom level of the map.
5. Create dot maps of Charlottesville. One for residence, one for workplace, and 48 for an interpolated commute required for animating. The "camera" list in the script controls the center long/lat point and zoom level of the map.

## Questions and Comments

I will be happy to answer any questions or comments asked publicly so that others may benefit, either opened as an issue on Github or directed to my [twitter account](https://twitter.com/StephenHolz)