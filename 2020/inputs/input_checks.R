## Some quick code to see what is different with the new data.


message("Doing checks comparing old and new dat files...")

options(digits=7)

## Catches
g <- rbind(cbind(version='2018', SS_dat$catch),
           cbind(version='2020', dat$catch)) %>%
  ggplot(aes(year, catch, color=version)) + geom_line(lwd=1.5, alpha=.5) +
  geom_point() + coord_cartesian(xlim=c(1964,NA))
ggsave('inputs/input_checks/catch_comparison.png', g, width=7, height=5)
## Index
g <- rbind(cbind(version='2018', SS_dat$CPUE),
           cbind(version='2020', SS_index)) %>%
  ggplot(aes(year, log(obs), ymax=log(obs)+1.96*se_log, ymin=log(obs)-1.96*se_log,  color=version)) + geom_line(lwd=1.5, alpha=.5) +
  geom_pointrange(alpha=.5) + labs(y='log-index')
ggsave('inputs/input_checks/index_comparison.png', g, width=7, height=5)

### Fishery lcomps
## Note I'm looking at all years even those with negative fleets
## to turn them off in SS. Will do that below.
x0 <- rbind(cbind(version='2018', filter(SS_dat$lencomp, FltSvy %in% c(-1,1))),
           cbind(version='2020', SS_lcomps_fishery)) %>%
  rename(year=Yr) %>% select(-Seas, -FltSvy, -Part, -Gender)
g <- ggplot(x0, aes(year, Nsamp, color=version))+
  geom_line(lwd=1, alpha=.5) + geom_point(alpha=.5) + scale_y_log10()
ggsave('inputs/input_checks/lcomps_fishery_Nsamp_comparison.png', g, width=7, height=5)
x1 <- x0 %>% select(-Nsamp) %>% # drop Nsamp since messes up when not matching
  pivot_longer(cols=-(1:2), values_to='proportion', names_to='lbin') %>%
  ## convert to lengths and sexes
  mutate(sex=substr(lbin,0,1), length=as.numeric(substr(lbin,2,10))) %>%
  filter(proportion>0) %>% select(-lbin)
## pivot wide again to compare data versions
x2 <- x1 %>% pivot_wider(names_from=version,
                        values_from=proportion, names_prefix='v',
                        values_fill=0) %>%
  mutate(diff=v2020-v2018) %>% filter(abs(diff)>1e-5)
g <- ggplot(x2, aes(length, y=diff, color=sex)) + geom_line() +
  facet_wrap('year') + ylab('Difference in proportions (v2020-v2018)')
ggsave('inputs/input_checks/lcomps_fishery_comparison.png', g, width=7, height=4)

## survey lcomps
x0 <- rbind(cbind(version='2018', filter(SS_dat$lencomp, FltSvy %in% c(-2,2))),
           cbind(version='2020', SS_lcomp_survey)) %>%
  rename(year=Yr) %>% select(-Seas, -FltSvy, -Part, -Gender)
g <- ggplot(x0, aes(year, Nsamp, color=version))+
  geom_line(lwd=1, alpha=.5) +
  geom_point(alpha=.5) + scale_y_log10()
ggsave('inputs/input_checks/lcomps_survey_Nsamp_comparison.png', g, width=7, height=5)
x1 <- x0 %>% select(-Nsamp) %>% # drop Nsamp since messes up when not matching
  pivot_longer(cols=-(1:2), values_to='proportion', names_to='lbin') %>%
  ## convert to lengths and sexes
  mutate(sex=substr(lbin,0,1), length=as.numeric(substr(lbin,2,10))) %>%
  filter(proportion>0) %>% select(-lbin)
## pivot wide again to compare data versions
x2 <- x1 %>% pivot_wider(names_from=version,
                        values_from=proportion, names_prefix='v',
                        values_fill=0) %>%
  mutate(diff=v2020-v2018) %>% filter(abs(diff)>1e-7)
g <- ggplot(x2, aes(length, y=diff, color=sex)) + geom_line() +
  facet_wrap('year') + ylab('Difference in proportions (v2020-v2018)')
ggsave('inputs/input_checks/lcomps_survey_comparison.png', g, width=7, height=4)

### Survey conditional age at length (CAAL) checks
## Turns out the Nsamp column here doesn't match which obscures
## what I want to see, so drop it recognizing that this is a
## lingering issue. Carey's seem
temp <- rbind(cbind(version='v2018', filter(SS_dat$agecomp, FltSvy==2)),
              cbind(version='v2020', SS_caal_survey)) %>%
  rename(length=Lbin_lo, sex=Gender, year=Yr) %>%
  select(-Lbin_hi, -Seas, -FltSvy, -Part, -Ageerr,
         -Nsamp, -starts_with('m')) %>%
  pivot_longer(cols=-(1:4), values_to='count', names_to='age', names_prefix='f') %>%
  mutate(age=as.numeric(age)) %>%
## Pivot wider so I can compare bin by bin for the two data
## versions
  filter(count>0) %>%
  pivot_wider(names_from='version', values_from='count') %>%
  ## Tweak to get a missing category
  mutate(year=year, diff=ifelse(is.na(v2020-v2018), 0,  v2020-v2018),
         missing=case_when(is.na(v2018) ~ 'v2018',
                           is.na(v2020) ~ 'v2020',
                           TRUE ~ 'no'))
g <- ggplot(filter(temp, sex==1 & missing!='no'), aes(age, length, color=missing)) +
  geom_point(alpha=.5) + facet_wrap('year') + ylim(6,54) + xlim(1,21)
ggsave('inputs/input_checks/caal_comparison_missing_female.png',g,  width=10, height=6)
g <- ggplot(filter(temp, sex==2 & missing!='no'), aes(age, length, color=missing)) +
  geom_point(alpha=.5) + facet_wrap('year') + ylim(6,54) + xlim(1,21)
ggsave('inputs/input_checks/caal_comparison_missing_male.png',g,  width=10, height=6)
write.csv(temp, file='inputs/input_checks/caal_survey_check.csv')


## Fishery age comps
x0 <- rbind(cbind(version='2018', filter(SS_dat$agecomp, FltSvy %in% c(-1,1))),
           cbind(version='2020', SS_agecomps_fishery)) %>%
  rename(year=Yr) %>% select(-Ageerr, -Lbin_lo, -Lbin_hi, -Seas, -FltSvy, -Part, -Gender)
g <- ggplot(x0, aes(year, Nsamp, color=version))+
  geom_line(lwd=1, alpha=.5) +
  geom_point(alpha=.5) + scale_y_log10()
ggsave('inputs/input_checks/agecomps_fishery_Nsamp.png', g, width=7, height=5)
g <- x0 %>% select(-Nsamp) %>% # If Nsamp different it breaks this
  pivot_longer(cols=-(1:2), values_to='proportion',
               names_to='agebin') %>%
  ## convert to ages and sexes
  mutate(sex=substr(agebin,0,1), age=as.numeric(substr(agebin,2,10))) %>%
  filter(proportion>0) %>% select(-agebin) %>%
  ## pivot wide again to compare data versions
  pivot_wider(names_from=version, values_from=proportion,
              names_prefix='v', values_fill=0) %>%
  mutate(diff=v2020-v2018) %>% filter(abs(diff)>1e-7) %>%
ggplot(aes(age, y=diff, color=sex)) + geom_line() +
  facet_wrap('year') + ylab('Difference in proportions (v2020-v2018)')
ggsave('inputs/input_checks/agecomps_fishery_comparison.png', g, width=7, height=4)
