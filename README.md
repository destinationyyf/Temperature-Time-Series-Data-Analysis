# Temperature-Time-Series-Data-Analysis

This is a data analysis project using time dependent temperature data. It basically consists of two parts: the descriptive part offers insight
about the data structure and trending, followed by global warming discussion; the model part build seasonal ARIMA model to fit the data and 
make forecast in the incoming 5 years.

The difference between this project and others is that this is an progressive data analysis project, which has no newly proposed algorithm 
and every analytic step relies on the former results. Therefore, it is hard (or barely meaningful) to wrap the codes in functions.

The repo contains 7 files:

`Temperature.txt` is the data source. At the time I was working on the project it had only data ends at summer 2017, now it has 3 more seasonal temperatures;

`Time Series Data Analysis.Rmd` is the main RMarkdown file, it describes in detail the entire analytic procedure;

`Time Series Data Analysis.R` is the Rcode extracted from `.Rmd` file, the reason is mentioned above;

`Time Series Data Analysis.pdf` is the pdf version generated from `.Rmd`;

The others are plots being used in the analysis.
