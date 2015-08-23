## Bodo Winter
## August 23, 2015
## Exploratory analysis of Rick's first results: analysis of hand-coded bends

##------------------------------------------------------------------
## Preprocessing:
##------------------------------------------------------------------

## Load in data:

setwd('/Users/teeniematlock/Desktop/research/robustness/rick_iterated_learning/')
bends <- read.csv("number_of_bends.csv")

## Packages:

library(reshape2)
library(ggplot2)
library(dplyr)
library(mgcv)
library(itsadug)

## Make wide format into long format:

blong <- melt(bends,
	id.vars = c('type_unique','participant','type','chain','alpha'))

## Rename columns:

blong <- rename(blong, generation = variable, bends = value)

## Create additional columns needed:

blong <- mutate(blong, generation = as.numeric(gsub('g', '', as.character(generation))),
	unique_ppt = as.factor(paste0(chain, generation, participant)),
	log_count = log(bends + 1))



##------------------------------------------------------------------
## Plotting & descriptive statistics:
##------------------------------------------------------------------

## Plot log counts by generation and alpha:

quartz('', 9, 5)
ggplot(blong, aes(x = generation, y = log_count, col = alpha)) +
	geom_point(shape=16) + geom_smooth() + facet_wrap(~ alpha)

## Means:

aggregate(log_count ~ alpha, blong, mean)
aggregate(bends ~ alpha, blong, mean)



##------------------------------------------------------------------
## Fit a simple GAM:
##------------------------------------------------------------------

## Center variables:

blong <- mutate(blong,
	generation_c = generation - mean(generation, na.rm = T),
	alpha_c = alpha - mean(alpha, na.rm = T))

## Build and inspect model (for now without random effects):

xmdl <- bam(bends ~ te(generation_c, alpha_c),
	data = blong, family = 'poisson', )
summary(xmdl)
# (beware: this GAM violates everything in the book)

## Visualize interaction:

quartz('', 8, 6)
pvisgam(xmdl, select = 1, rug = F, main = 'Interaction',
	cex.axis = 2, cex.lab = 2)

## Check for autocorrelation:

acf_resid(xmdl)		# not so much
# (however, the model does not know about the nested structures yet)



