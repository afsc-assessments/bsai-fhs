### Switched to query written by Rebecca and modified by Cole in
### 10/2020
## 10129 is the complex together, otherwise call them individually
qsurv <- "select a.SPECIES_CODE, b.SPECIES_NAME, b.COMMON_NAME,
a.YEAR, a.STRATUM, round(MEANWGTCPUE,4) meanwgtcpue, round(VARMNWGTCPUE,10) varmnwgtcpue,
round(BIOMASS,2) biomass, round(VARBIO,4) varbio, round(LOWERB,2) lowerb,
round(UPPERB,2) upperb, DEGREEFWGT, round(MEANNUMCPUE,4) meannumcpue, round(VARMNNUMCPUE,10) varmnnumcpue,
round(POPULATION) population, round(VARPOP,4) varpop, round(LOWERP) lowerp, round(UPPERP) upperp,
DEGREEFNUM,HAULCOUNT,CATCOUNT,NUMCOUNT,LENCOUNT
from haehnr.biomass_ebs_standard_grouped a, racebase.species b
where a.species_code=b.species_code and a.species_code = 10129
and a.stratum=999
order by a.species_code, a.year,stratum"
## I need the species split for a table. This is not used in the assessment.
qsurvspp <- "select a.SPECIES_CODE, b.SPECIES_NAME, b.COMMON_NAME,
a.YEAR, a.STRATUM, round(MEANWGTCPUE,4) meanwgtcpue, round(VARMNWGTCPUE,10) varmnwgtcpue,
round(BIOMASS,2) biomass, round(VARBIO,4) varbio, round(LOWERB,2) lowerb,
round(UPPERB,2) upperb, DEGREEFWGT, round(MEANNUMCPUE,4) meannumcpue, round(VARMNNUMCPUE,10) varmnnumcpue,
round(POPULATION) population, round(VARPOP,4) varpop, round(LOWERP) lowerp, round(UPPERP) upperp,
DEGREEFNUM,HAULCOUNT,CATCOUNT,NUMCOUNT,LENCOUNT
from haehnr.biomass_ebs_standard a, racebase.species b
where a.species_code=b.species_code and a.species_code in (10130,10140)
and a.stratum=999
order by a.species_code, a.year,stratum"

qsurvAI <- "select t.*, s.common_name
from ai.biomass_total t, racebase.species s
where t.species_code in (10130,10140) and t.species_code=s.species_code and year>1980
order by t.species_code,t.year\n"