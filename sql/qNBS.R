qnbs <- "
select a.SPECIES_CODE, b.SPECIES_NAME, b.COMMON_NAME,
a.YEAR, a.STRATUM, round(MEANWGTCPUE,4) meanwgtcpue, round(VARMNWGTCPUE,10) varmnwgtcpue,
round(BIOMASS,2) biomass, round(VARBIO,4) varbio, round(LOWERB,2) lowerb,
round(UPPERB,2) upperb, DEGREEFWGT, round(MEANNUMCPUE,4) meannumcpue, round(VARMNNUMCPUE,10) varmnnumcpue,
round(POPULATION) population, round(VARPOP,4) varpop, round(LOWERP) lowerp, round(UPPERP) upperp,
DEGREEFNUM,HAULCOUNT,CATCOUNT,NUMCOUNT,LENCOUNT
from haehnr.biomass_nbs_safe a, racebase.species b
where a.species_code=b.species_code and a.species_code in (10130,10140)
and a.stratum=999
order by a.species_code, a.year,stratum
"