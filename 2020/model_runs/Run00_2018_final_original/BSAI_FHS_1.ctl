#V3.30.07.01-trans;_2017_08_07;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_data_and_control_files: BSAI_FHS_1.dat // BSAI_FHS_1.ctl
#V3.30.07.01-trans;_2017_08_07;_Stock_Synthesis_by_Richard_Methot_(NOAA)_using_ADMB_11.6
#_user_support_available_at:NMFS.Stock.Synthesis@noaa.gov
#_user_info_available_at:https://vlab.ncep.noaa.gov/group/stock-synthesis
0  # 0 means do not read wtatage.ss; 1 means read and use wtatage.ss and also read and use growth parameters
1  #_N_Growth_Patterns
1 #_N_platoons_Within_GrowthPattern 
#_Cond 1 #_Morph_between/within_stdev_ratio (no read if N_morphs=1)
#_Cond  1 #vector_Morphdist_(-1_in_first_val_gives_normal_approx)
#
2 # recr_dist_method for parameters:  2=main effects for GP, Settle timing, Area; 3=each Settle entity; 4=none when N_GP*Nsettle*pop==1
1 # not yet implemented; Future usage: Spawner-Recruitment: 1=global; 2=by area
1 #  number of recruitment settlement assignments 
0 # unused option
#GPattern month  area  age (for each settlement assignment)
 1 1 1 0
#
#_Cond 0 # N_movement_definitions goes here if Nareas > 1
#_Cond 1.0 # first age that moves (real age at begin of season, not integer) also cond on do_migration>0
#_Cond 1 1 1 2 4 10 # example move definition for seas=1, morph=1, source=1 dest=2, age1=4, age2=10
#
1 #_Nblock_Patterns
1 # 1 #_blocks_per_pattern 
## begin and end years of blocks
1964 1987
#1988 2007
#
# controls for all timevary parameters 
1 #_env/block/dev_adjust_method for all time-vary parms (1=warn relative to base parm bounds; 3=no bound check)
#  autogen
0 0 0 0 1 # autogen: 1st element for biology, 2nd for SR, 3rd for Q, 4th reserved, 5th for selex
# where: 0 = autogen all time-varying parms; 1 = read each time-varying parm line; 2 = read then autogen if parm min==-12345
# 
#
# setup for M, growth, maturity, fecundity, recruitment distibution, movement 
#
0 #_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate
  #_no additional input for selected M option; read 1P per morph
1 # GrowthModel: 1=vonBert with L1&L2; 2=Richards with L1&L2; 3=age_specific_K; 4=not implemented
3 #_Growth_Age_for_L1
21 #_Growth_Age_for_L2 (999 to use as Linf)
-999 #_exponential decay for growth above maxage (fixed at 0.2 in 3.24; value should approx initial Z; -999 replicates 3.24)
0  #_placeholder for future growth feature
0 #_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)
0 #_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A); 4 logSD=F(A)
2 #_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern; 4=read age-fecundity; 5=disabled; 6=read length-maturity
3 #_First_Mature_Age
1 #_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4)eggs=a+b*L; (5)eggs=a+b*W
0 #_hermaphroditism option:  0=none; 1=female-to-male age-specific fxn; -1=male-to-female age-specific fxn
1 #_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)
#
#_growth_parms
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env_var&link	dev_link	dev_minyr	dev_maxyr	dev_PH	Block	Block_Fxn		
	0.1	0.3	0.2	0	0.8	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Fem_GP_1
	2	30	17.4629	0	0.2	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Fem_GP_1
	25	80	44.2606	0	0.2	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Fem_GP_1
	0.01	2	0.129943	0	0.8	0	1	0	0	0	0	0	0	0	#	VonBert_K_Fem_GP_1
	0.001	0.3	0.153146	0	0.8	0	2	0	0	0	0	0	0	0	#	CV_young_Fem_GP_1
	0.001	0.3	0.0761065	0	0.8	0	2	0	0	0	0	0	0	0	#	CV_old_Fem_GP_1
	0	0.5	2.98E-06	0	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Fem
	2.5	4.5	3.3268	0	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Fem
	0	20	9.70203	0	0.8	0	-3	0	0	0	0	0	0	0	#	Mat50%_Fem
	-20	0	-0.942711	0	0.8	0	-3	0	0	0	0	0	0	0	#	Mat_slope_Fem
	-3	3	1	0	0.8	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_inter_Fem
	-3	4	0	0	0.8	0	-3	0	0	0	0	0	0	0	#	Eggs/kg_slope_wt_Fem
	0.1	0.3	0.2	0	0.8	0	-3	0	0	0	0	0	0	0	#	NatM_p_1_Mal_GP_1
	1	45	17.6651	0	0.8	0	1	0	0	0	0	0	0	0	#	L_at_Amin_Mal_GP_1
	20	70	37.8498	0	0.8	0	1	0	0	0	0	0	0	0	#	L_at_Amax_Mal_GP_1
	0.05	0.4	0.1704	0	0.8	0	1	0	0	0	0	0	0	0	#	VonBert_K_Mal_GP_1
	0.001	0.3	0.15749	0	0.8	0	2	0	0	0	0	0	0	0	#	CV_young_Mal_GP_1
	0.001	0.3	0.0733591	0	0.8	0	2	0	0	0	0	0	0	0	#	CV_old_Mal_GP_1
	0	0.5	2.98E-06	0	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_1_Mal
	2.5	4.5	3.3268	0	0.2	0	-3	0	0	0	0	0	0	0	#	Wtlen_2_Mal
	-4	4	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_GP_1
	-4	4	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_Area_1
	-4	4	0	0	0	0	-4	0	0	0	0	0	0	0	#	RecrDist_timing_1
	1	1	1	1	1	0	-1	0	0	0	0	0	0	0	#	CohortGrowDev
	0.000001	0.999999	0.5	0.5	0.5	0	-99	0	0	0	0	0	0	0	#	FracFemale_GP_1
#
#_no timevary MG parameters
#
#_seasonal_effects_on_biology_parms
 0 0 0 0 0 0 0 0 0 0 #_femwtlen1,femwtlen2,mat1,mat2,fec1,fec2,Malewtlen1,malewtlen2,L1,K
#_ LO HI INIT PRIOR PR_SD PR_type PHASE
#_Cond -2 2 0 0 -1 99 -2 #_placeholder when no seasonal MG parameters
#
#_Spawner-Recruitment
3 #_SR_function: 2=Ricker; 3=std_B-H; 4=SCAA; 5=Hockey; 6=B-H_flattop; 7=survival_3Parm; 8=Shepard_3Parm
0  # 0/1 to use steepness in initial equ recruitment calculation
0  #  future feature:  0/1 to make realized sigmaR a function of SR curvature
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	-2.49224	17.5078	13.1939	14.9919	10	0	1	0	0	0	0	0	0	0	#	SR_LN(R0)
	0.2	1	1	0.999	0.05	0	-4	0	0	0	0	0	0	0	#	SR_SCAA_null
	0	2	0.5	0.5	0.8	0	-4	0	0	0	0	0	0	0	#	SR_sigmaR
	-14	4	0	0	1	0	-1	0	0	0	0	0	0	0	#	SR_regime
	0	0	0	0	0	0	-99	0	0	0	0	0	0	0	#	SR_autocorr
1 #do_recdev:  0=none; 1=devvector; 2=simple deviations
1973 # first year of main recr_devs; early devs can preceed this era
2014 # last year of main recr_devs; forecast devs start in following year
2 #_recdev phase 
1 # (0/1) to read 13 advanced options
1963 #_recdev_early_start (0=none; neg value makes relative to recdev_start)
2 #_recdev_early_phase
-1 #_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)
 1 #_lambda for Fcast_recr_like occurring before endyr+1
 1916.1157 #_last_early_yr_nobias_adj_in_MPD                   
 2010.9021 #_first_yr_fullbias_adj_in_MPD                      
 2011.0000 #_last_yr_fullbias_adj_in_MPD                       
 2013      #_first_recent_yr_nobias_adj_in_MPD                 
 0.6377    #_max_bias_adj_in_MPD (1.0 to mimic pre-2009 models) 0 #_period of cycles in recruitment (N parms read below)
 0 #period of cycle in recruitment (N parms read below)
 -15 #min rec_dev
 15 #max rec_dev
 0 #_read_recdevs
#_end of advanced SR options
#
#_placeholder for full parameter lines for recruitment cycles
# read specified recr devs
#_Yr Input_value
#
# all recruitment deviations
#  1961R 1962R 1963R 1964R 1965R 1966R 1967R 1968R 1969R 1970R 1971R 1972R 1973R 1974R 1975R 1976R 1977R 1978R 1979R 1980R 1981R 1982R 1983R 1984R 1985R 1986R 1987R 1988R 1989R 1990R 1991R 1992R 1993R 1994R 1995R 1996R 1997R 1998R 1999R 2000R 2001R 2002R 2003R 2004R 2005R 2006R 2007R 2008R 2009R 2010R 2011R 2012R 2013F 2014F 2015F 2016F 2017F
#  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
# implementation error by year in forecast:  0
#
#Fishing Mortality info 
0 # F ballpark
-1984 # F ballpark year (neg value to disable)
3 # F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)
4 # max F or harvest rate, depends on F_Method
# no additional F input needed for Fmethod 1
# if Fmethod=2; read overall start F value; overall phase; N detailed inputs to read
4 # if Fmethod=3; read N iterations for tuning for Fmethod 3
# 0.2 1 0 # overall start F value; overall phase; N detailed inputs to read
#Fleet Yr Seas F_value se phase (for detailed setup of F_Method=2; -Yr to fill remaining years)

#
#_initial_F_parms; count = 1
#_ LO HI INIT PRIOR PR_SD  PR_type  PHASE
0 1 0.2 0 99 0 1 # InitF_seas_1_flt_1Fishery
#2017 2017
# F rates by fleet
# Yr:  1977 1978 1979 1980 1981 1982 1983 1984 1985 1986 1987 1988 1989 1990 1991 1992 1993 1994 1995 1996 1997 1998 1999 2000 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017
# seas:  1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# Fishery 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2 0.2
#
#_Q_setup
#_   fleet      link link_info  extra_se   biasadj     float  #  fleetname
         2         1         0         0         0         0  #  Survey
-9999 0 0 0 0 0
#
#_Q_parms(if_any);Qunits_are_ln(q)
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	-1	1	0	0	0.5	0	-2	0	0	0	0	0	0	0	#	LnQ_base_Survey(2)
#_no timevary Q parameters
#
#_size_selex_types
#_discard_options:_0=none;_1=define_retention;_2=retention&mortality;_3=all_discarded_dead;_4=define_dome-shaped_retention
#_Pattern Discard Male Special
 1 0 3 0 # 1 Fishery
 0 0 0 0 # 2 Survey
#
#_age_selex_types
#_Pattern Discard Male Special
0 0 0 0 # 1 Fishery
20 0 3 0 # 2 Survey
#	Fishery	selex																
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#	parm_name
	0	80	37.5624	25	5	0	1	0	0	0	0	0	1	2	#	SizeSel_P1_Fishery(1)
	0	80	10.309	0.1	5	0	1	0	0	0	0	0	1	2	#	SizeSel_P2_Fishery(1)
	-80	80	0	0.1	5	0	2	0	0	0	0	0	1	2	#	SizeSel_P1_Male_Offset_Fishery(1)
	-80	80	0	0.1	5	0	2	0	0	0	0	0	1	2	#	SizeSel_P2_Male_Offset_Fishery(1)
	0	1	1	1	5	0	-3	0	0	0	0	0	0	0	#	SizeSel_P3_Male_Offset_Fishery(1)
#Survey	selex																	
#	0	80	23.6752	25	5	0	2	0	0	0	0	0	0	0	#	SizeSel_P1_Survey(2)		
#	0	80	11.1613	0.1	5	0	2	0	0	0	0	0	0	0	#	SizeSel_P2_Survey(2)		
#	-80	80	0	0.1	5	0	2	0	0	0	0	0	0	0	#	SizeSel_P1_Male_Offset_Survey(2)		
#	-80	80	0	0.1	5	0	2	0	0	0	0	0	0	0	#	SizeSel_P2_Male_Offset_Survey(2)		
#	0	1	1	1	5	0	3	0	0	0	0	0	0	0	#	SizeSel_P3_Male_Offset_Survey(2)		
##SURVEY:	FEMALES	AGE-BASED DOUBLE NORMAL										
#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	env-var	use_dev	dev_mnyr	dev_mxyr	dev_PH	Block	Blk_Fxn	#
	1	40	15	15	5	0	2	0	0	0	0	0	0	0	#PEAK
	-5	25	12	5	5	0	-3	0	0	0	0	0	0	0	#TOP
	-4	4	1.6	1.6	5	0	2	0	0	0	0	0	0	0	#ASC_WIDTH
	-2	10	3	3	5	0	-3	0	0	0	0	0	0	0	#DSC_WIDTH
	-1000	1	-1000	-999	5	0	-4	0	0	0	0	0	0	0	#INIT
	-5	5	999	999	5	0	-4	0	0	0	0	0	0	0	#FINAL
	#														
	##...SURVEY:	DO_MALE	(AS	OFFSET)											
	-15	15	0	0	5	0	3	0	0	0	0	0	0	0	#PEAK
	-15	15	0	0	5	0	3	0	0	0	0	0	0	0	#ASC_WIDTH
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#DSC_WIDTH
	-15	15	0	0	5	0	-3	0	0	0	0	0	0	0	#FINAL
	-15	15	1	0	5	0	-4	0	0	0	0	0	0	0	#APICAL

#_	LO	HI	INIT	PRIOR	PR_SD	PR_type	PHASE	#	parm_name										
#	1964	to	1987	then	1988	to	2007	
	0	80	37.5624	25	5	0	2	#	SizeSel_P1_Fishery(1)_1964to1987										
#	0	80	37.5624	25	5	0	2	#	SizeSel_P1_Fishery(1)_1988to2008										
																			
	0	80	10.309	0.1	5	0	2	#	SizeSel_P2_Fishery(1)_1964to1987										
#	0	80	10.309	0.1	5	0	2	#	SizeSel_P2_Fishery(1)_1988to2008										
																			
	-40	40	0	0.1	5	0	2	#	SizeSel_P1_Male_Offset_Fishery(1)_1964to1987										
#	-40	40	0	0.1	5	0	2	#	SizeSel_P1_Male_Offset_Fishery(1)_1988to2008										
																			
	-40	40	0	0.1	5	0	2	#	SizeSel_P2_Male_Offset_Fishery(1)_1964to1987										
#	-40	40	0	0.1	5	0	2	#	SizeSel_P2_Male_Offset_Fishery(1)_1988to2008										

#
0   #  use 2D_AR1 selectivity(0/1):  experimental feature
#_no 2D_AR1 selex offset used
#
# Tag loss and Tag reporting parameters go next
0  # TG_custom:  0=no read; 1=read if tags exist
#_Cond -6 6 1 1 2 0.01 -4 0 0 0 0 0 0 0  #_placeholder if no parameters
#
# no timevary parameters
#
#
# Input variance adjustments factors: 
 #_1=add_to_survey_CV
 #_2=add_to_discard_stddev
 #_3=add_to_bodywt_CV
 #_4=mult_by_lencomp_N
 #_5=mult_by_agecomp_N
 #_6=mult_by_size-at-age_N
 #_7=mult_by_generalized_sizecomp
#_Factor  Fleet  Value
4 1 0.06   # Fishery Length
4 2 0.33  # Survey Length
5 1 0.14  # Fishery Age
5 2 0.15   # Survey Age
 -9999   1    0  # terminator
#
10 #_maxlambdaphase
1 #_sd_offset
# read 5 changes to default Lambdas (default value is 1.0)
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=SizeFreq; 7=sizeage; 8=catch; 9=init_equ_catch; 
# 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin; 17=F_ballpark
#like_comp fleet  phase  value  sizefreq_method
# 4 1 1 0.03 1
# 5 1 1 0.78 1
# 4 2 1 0.98 1
# 5 2 1 0.79 1
# 8 1 1 50 1
-9999  1  1  1  1  #  terminator
#
# lambdas (for info only; columns are phases)
#  0 0 0 0 0 0 0 0 0 0 #_CPUE/survey:_1
#  1 1 1 1 1 1 1 1 1 1 #_CPUE/survey:_2
#  0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 0.42 #_lencomp:_1
#  2.199 2.199 2.199 2.199 2.199 2.199 2.199 2.199 2.199 2.199 #_lencomp:_2
#  0.521 0.521 0.521 0.521 0.521 0.521 0.521 0.521 0.521 0.521 #_agecomp:_1
#  0.93 0.93 0.93 0.93 0.93 0.93 0.93 0.93 0.93 0.93 #_agecomp:_2
#  1 1 1 1 1 1 1 1 1 1 #_init_equ_catch
#  1 1 1 1 1 1 1 1 1 1 #_recruitments
#  1 1 1 1 1 1 1 1 1 1 #_parameter-priors
#  1 1 1 1 1 1 1 1 1 1 #_parameter-dev-vectors
#  1 1 1 1 1 1 1 1 1 1 #_crashPenLambda
#  0 0 0 0 0 0 0 0 0 0 # F_ballpark_lambda
0 # (0/1) read specs for more stddev reporting 
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages
 # placeholder for vector of selex bins to be reported
 # placeholder for vector of growth ages to be reported
 # placeholder for vector of NatAges ages to be reported
999

