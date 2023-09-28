
# Notes on the 2020 BSAI flathead sole assessment # 

Cole Monnahan

## Background ##

I took over this assessment from Carey McGilliard in the 2020
assessment cycle. Carey has a set of code that works for multiple
assessments. I had a hard time getting it to work so I made some
organizational changes based loosely on the Transparent
Assessment Framework (TAF) from ICES
(https://www.ices.dk/data/assessment-tools/Pages/transparent-assessment-framework.aspx). I
do as possible using R scripts so that it is reproducible and
transparent. Most importantly were to completely automate the
steps to pull data and process them into suitable inputs into the
stock assessment. There are a few minor data sources that are
done manually (e.g., weekly catches), but very few. Some things
like setting up the proj model, running models, etc. are not
quite as automated but close.

## How to reproduce the assessment ##

The steps are to get the data, process the model inputs, run the
model, and generate outputs.

### Data generation ###
The script `data.R` creates a static copy of often dynamic data
sources like the AFSC and AKFIN databases. These are stored in
the data folder with no processing or cleaning. They are the
initial raw data files.

All that should be needed is to create database connections to
AFSC and AKFIN, and then source the file. See file for some small
exceptions like weekly catches.

### Processing inputs ###
Sourcing the `input.R` file will read in the static raw data from
the data folder, process it, make some exploratory plots and
checks and write SS model input files. This assessment only had
one model version so it is quite simple. I didn't quite break
dependence on Carey's "newsbss" package so there are a few links
to a modified version "colesbss." This script will write the SS
data file. 

### Running the model ###
After regenerating the inputs, the script
`model_runs/compare_models.R` has some code to run models, made
comparisons and plots and such via `r4ss`. Proj needs to be done
manually, as does the SIS calculation and SARA outputs, although
the latter has a script I got from Steve. 

### Creating Report ###
Run the script `report.R` to generate figures and tables in the
report directory. Final calculations and table formatting are
done in the 'report' spreadsheet which is copied into Word, in
addition to 'tables_results' spreadsheet which is for the model
output. Most figures are made by either running the
`make_figures.R` script or copying appropriate r4ss files over. 

## To do for next time
Things to do for next time:

* Add ghost age comps for survey back in, since used in figures
* Add ghost len comps for fishery
* Explore the recdevs being turned off last three years
* Why age 3+ biomass?
* That recr dist warning in SS
* Impact of the bias adjustment.. should I even use it?
* There are some lingering Carey calculations that I'd like to
remove, particularly in processing the fishery data. It seems
like she pulls the data multiple times for different uses to
process it.

## Issues with approach ##
Combining the two surveys via a linear model is not ideal. This
is discussed in the SAFE a bit with some other suggestions for
improvements. Since the AI is such a small part of the biomass it
probably won't make a difference so probably the best thing to do
is work on the catchability:temperature relationship via a VAST
model. 

-------------------------------------------------------------------------------



