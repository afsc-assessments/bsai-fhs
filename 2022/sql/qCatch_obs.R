## Catches by species as reported by observer haul by
## haul. These are used in the tables in the SAFE.
qcatch_obs <- paste0("SELECT OBSINT.DEBRIEFED_SPCOMP.SPECIES,\n ",
                 "OBSINT.DEBRIEFED_SPCOMP.PERCENT_RETAINED,\n ",
                 "OBSINT.DEBRIEFED_SPCOMP.EXTRAPOLATED_WEIGHT,\n ",
                 "OBSINT.DEBRIEFED_HAUL.PERMIT,\n ",
                 "OBSINT.DEBRIEFED_HAUL.CRUISE,\n ",
                 "OBSINT.DEBRIEFED_HAUL.HAUL,\n ",
                 "OBSINT.DEBRIEFED_HAUL.NMFS_AREA,\n ",
                 "OBSINT.DEBRIEFED_SPCOMP.YEAR,\n ",
                 "OBSINT.DEBRIEFED_HAUL.GEAR_TYPE\n ",
                 "FROM OBSINT.DEBRIEFED_SPCOMP\n ",
                 "INNER JOIN OBSINT.DEBRIEFED_HAUL\n ",
                 "ON OBSINT.DEBRIEFED_HAUL.HAUL_JOIN    = OBSINT.DEBRIEFED_SPCOMP.HAUL_JOIN\n ",
                 "WHERE OBSINT.DEBRIEFED_SPCOMP.SPECIES in (103, 145)\n ",
                 "AND OBSINT.DEBRIEFED_HAUL.NMFS_AREA BETWEEN '500' AND '544'\n "
)