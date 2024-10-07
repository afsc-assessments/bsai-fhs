## exploration of new ISS values
## this uses the afscISS package https://afsc-assessments.github.io/afscISS/articles/get_output.html
## According to the Vignette these values could replace what is served by gapindex
## given this is a potentailly large change from the original input going to examine carefully before deciding
## what do do; likely will either not use or wholesale replace (i.e. I won't only update the sample sizes)


## NOTES
## Comps are FHS only and BS  only (AI is only used for biomass) (10130)
## Will only have data thru 2023 for caal and marginal lengths
## recall marginal ages are ghosted
## length comps are female then male, l to rate
## caals are entered as sex specific with dummy values
## ages are 1:21; lengths are in weird bins unclear if we can back them out

## download data
inputN_length <- afscISS::get_ISS(species = 10130,
                 region = 'ebs', ## comps are EBS only in this model
                 comp = 'length',
                 sex_cat = 1:2, ## keep sexes separate
                 spec_case = NULL)

inputN_caal <- afscISS::get_ISS(species = 10130,
                                  region = 'ebs', ## comps are EBS only in this model
                                  comp = 'caal',
                                  sex_cat = 1:2,
                                  spec_case = NULL)


tail(iss)

ass <- afscISS::get_ISS(species = 10130:10140,
                        region = 'ebs', ## comps are EBS only in this model
                        comp = 'age',
                        sex_cat = 1,
                        spec_case = NULL)
tail(ass)

acomp <- afscISS::get_comp(species = 10130,
                           region = 'ebs', ## comps are EBS only in this model
                           comp = 'caal',
                           sex_cat = 1, #when sex_cat = 12 this will return values of 1 (males) and 2 (females) in the sex column where the proportions will sum to 1 across both sexes
                           spec_case = NULL)



icomp <- afscISS::get_comp(species = 10130:10140,
                        region = 'ebs', ## comps are EBS only in this model
                        comp = 'length',
                        sex_cat = 12, #when sex_cat = 12 this will return values of 1 (males) and 2 (females) in the sex column where the proportions will sum to 1 across both sexes
                        spec_case = NULL)

icomp %>% filter(year == 1982 & )
