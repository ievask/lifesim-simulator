library(R6)
library(tidyverse)

 Person= R6Class("Person",
                 public = list(
                   initialize = function(rdf, mortality_probs, life_expectancy, i, 
                                         targetdata_for_mort,targetdata_sexage, average_data_total, 
                                         targetdata_trends,  eq5d, hcosts, numbergen, no_recip, no_total) {
                     private$mortality_probs = mortality_probs
                     private$life_expectancy = life_expectancy
                     private$targetdata_for_mort = targetdata_for_mort
                     private$targetdata_sexage = targetdata_sexage
                     private$average_data_total = average_data_total
                     private$targetdata_trends = targetdata_trends
                     private$eq5d = eq5d
                     private$hcosts = hcosts
                     private$life_history = tibble(male = if(rdf$sex[i]==1) ('TRUE') else('FALSE'),
                                                   rich = if(rdf$sep[i]>3) ('TRUE') else('FALSE'), 
                                                   sep_b = rdf$sep[i],
                                                   sep = rdf$sep[i],
                                                   age=0,
                                           life_stage = private$get_life_stage(0),
                                           edu_p = rdf$edu_p2[i],
                                           kessler_p = rdf$kessler[i],
                                           rutter_p = rdf$amrutter[i],
                                           mhealth_p2 = rdf$mhealth_p2[i],
                                           recip=rdf$recip[i],
                                           sdqcond3 = (rdf$sdq_cond3[i]) ,
                                           cognitive_capital = (rdf$g2[i]),
                                           social_emotional_capital = (10-rdf$sdq_cond2[i])/10,
                                           ch_unh_beh = rdf$smokes14[i],
                                           min_cons=10000,
                                           pov_line=14673,
                                           no_recip=no_recip,
                                           no_total=no_total,
                                           pr_unh_beh_np=0,
                                           pr_unh_beh_p=0,
                                           pr_unh_beh=0,
                                          unh_beh=0,
                                          pr_chd=0,
                                          pr_chd_trend=0,
                                          chd=0,
                                          pr_mhealth=0,
                                          pr_mhealth_trend=0,
                                          mhealth = if(rdf$em_wellb[i]>=5) 1 else 0,
                                          teq5d=filter(eq5d, AGE==0,  sex==rdf$sex[i], SEP==rdf$sep[i])$EQ5D,
                                           health=filter(eq5d, AGE==0,  sex==rdf$sex[i], SEP==rdf$sep[i])$EQ5D,
                                           thcosts=filter(hcosts, AGE==0,  sex==rdf$sex[i], SEP==rdf$sep[i])$HCOSTS,
                                           ahcosts=filter(hcosts, AGE==0,  sex==rdf$sex[i], SEP==rdf$sep[i])$HCOSTS,
                                          pr_edu=0,
                                          edu = 0, 
                                          pr_employed=0,
                                          employed=0,
                                          cum_unemployed=0,
                                          cum_employed=0,
                                          pot_earnings = 0,
                                          earnings = 0,
                                          income_p=(rdf$income_p[i])*1000,
                                          consumption=max((rdf$income_p[i])*1000, 10000),
                                          consumption_op=max((rdf$income_p[i])*1000, 10000),
                                          depr=rdf$depr_p[i],
                                          no_siblings=rdf$nchild[i]-1,
                                          wealth = (rdf$wealth[i])/(rdf$nchild[i]),
                                          cd=0,
                                          pr_prison=0,
                                          prison=0,
                                          pr_res=0,
                                          res=0,
                                          public_costs=0,
                                          pension=0,
                                          taxes=0,
                                          benefits=0,
                                          gov_budget=0,
                                          op_cost=0,
                                          dsocial_emotional_capital=0,
                                          dcognitive_capital=0,
                                          dunh_beh=0,
                                          dconsumption=0,
                                          dearnings=0,
                                          dmhealth=0,
                                          dchd=0,
                                          ddepr=0,
                                          demployed=0,
                                          cum_unh_beh=unh_beh,
                                          sex=rdf$sex[i],
                                          dprison=0,
                                          cognitive_capital3 = (rdf$g3[i]),
                                          cognitive_capital4 = (rdf$g4[i]),
                                          cognitive_capital5= (rdf$g5[i]),
                                          cognitive_capital6 = (rdf$g6[i]),
                                          effect=0,
                                          effect2=0,
                                          social_emotional_capital3 = (10-rdf$sdq_cond3[i])/10,
                                          social_emotional_capital4 = (10-rdf$sdq_cond4[i])/10,
                                          social_emotional_capital5 = (10-rdf$sdq_cond5[i])/10,
                                          social_emotional_capital6 = (10-rdf$sdq_cond6[i])/10,
                                          impact2 =  rdf$impact2[i],
                                          impact3 =  rdf$impact3[i],
                                          impact4 = rdf$impact4[i],
                                          impact=0,
                                          sd_soc5=((filter(average_data_total,AGE==5, SEX==1)$sd_soc)*(filter(average_data_total,AGE==5, SEX==1)$num)+(filter(average_data_total,AGE==5, SEX==0)$sd_soc)*(filter(average_data_total,AGE==5, SEX==0)$num))/(filter(average_data_total,AGE==5, SEX==1)$num+filter(average_data_total,AGE==5, SEX==0)$num),
                                          sd_imp5=((filter(average_data_total,AGE==5, SEX==1)$sd_imp)*(filter(average_data_total,AGE==5, SEX==1)$num)+(filter(average_data_total,AGE==5, SEX==0)$sd_imp)*(filter(average_data_total,AGE==5, SEX==0)$num))/(filter(average_data_total,AGE==5, SEX==1)$num+filter(average_data_total,AGE==5, SEX==0)$num),
                                          check=0,
                                          smr.mhealth=0,
                                          mp.chd=0
                                          )
                     return(invisible(self))
                   },
                   print = function(...) { cat("Current Status:\n")
                     glimpse(private$life_history)
                     return(invisible(self))
                   },
                   get_life_history = function(){
                     return(private$life_history)
                   },
                   live_life = function(){
                     previous_year = tail(private$life_history,1)
                     current_year = previous_year
                      while(current_year$life_stage != "dead" & current_year$age < 100){
                       current_year$age = previous_year$age + 1
                       current_year$sd_soc5=previous_year$sd_soc5
                       current_year$cognitive_capital3=previous_year$cognitive_capital3
                       current_year$cognitive_capital4=previous_year$cognitive_capital4
                       current_year$cognitive_capital5=previous_year$cognitive_capital5
                       current_year$cognitive_capital6=previous_year$cognitive_capital6
                       current_year$social_emotional_capital3=previous_year$social_emotional_capital3
                       current_year$social_emotional_capital4=previous_year$social_emotional_capital4
                       current_year$social_emotional_capital5=previous_year$social_emotional_capital5
                       current_year$social_emotional_capital6=previous_year$social_emotional_capital6
                       current_year$impact2=previous_year$impact2
                       current_year$impact3=previous_year$impact3
                       current_year$impact4=previous_year$impact4
                       current_year$income_p=previous_year$income_p
                       current_year$ch_unh_beh=previous_year$ch_unh_beh
                       current_year$sex=previous_year$sex
                       current_year$male=previous_year$male
                       current_year$rich=previous_year$rich
                       current_year$recip=previous_year$recip
                       current_year$pov_line=previous_year$pov_line
                       current_year$no_recip=previous_year$no_recip
                       current_year$no_total=previous_year$no_total
                       current_year$sdqcond3=previous_year$sdqcond3
                       current_year$sep_b=previous_year$sep_b
                       current_year$edu_p=previous_year$edu_p
                       current_year$mhealth_p2=previous_year$mhealth_p2
                       current_year$rutter_p=previous_year$rutter_p
                       current_year$kessler_p=previous_year$kessler_p
                       current_year$no_siblings=previous_year$no_siblings
                       current_year$check=runif(1)
                       current_year$min_cons=previous_year$min_cons
                       #Trends and variance
                       current_year$trend.earn=filter(private$targetdata_trends, AGE==current_year$age, SEX==current_year$sex)$trend.earn.an
                       current_year$trend.unemployed=filter(private$targetdata_trends, AGE==current_year$age, SEX==current_year$sex)$trend.unemployed
                       current_year$trend.chd.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$CUM.TR.CHD
                       current_year$trend.mhealth.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$CUM.TR.MHEALTH
                       current_year$trend.smokes.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$CUM.TR.SMOKES
                       current_year$trend.chd=filter(private$targetdata_sexage, AGE==current_year$age,  MALE==current_year$male)$CUM.TR.CHD
                       current_year$trend.mhealth=filter(private$targetdata_sexage, AGE==current_year$age,  MALE==current_year$male)$CUM.TR.MHEALTH
                       current_year$trend.smokes=filter(private$targetdata_sexage, AGE==current_year$age,  MALE==current_year$male)$CUM.TR.SMOKES
                       current_year$sd.cog=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$sd_cog
                       current_year$sd.soc=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$sd_soc
                       current_year$se.earn=filter(private$targetdata_trends, AGE==current_year$age, SEX==current_year$sex)$se.earn
                       current_year$se.tr.earn=filter(private$targetdata_trends, AGE==current_year$age, SEX==current_year$sex)$se.tr.earn
                        #Target data
                       current_year$target.cog=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_cog
                       current_year$target.soc=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_soc
                       current_year$target.cd=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_cd
                       current_year$target.mhealth=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_mhealth
                       current_year$target.ch_smokes=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_ch_smokes
                       current_year$target.edu=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_edu*0.8
                       current_year$target.prison=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_prison
                       current_year$target.res=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_care
                       current_year$target.depr=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_depr
                       current_year$target.smokes=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_smokes
                       current_year$target.employed=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_employed
                       current_year$target.earnings=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_earnings
                       current_year$target.chd=filter(private$average_data_total, AGE==current_year$age,  MALE==current_year$male)$target_chd
                       current_year$target.chd.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$CHD
                       current_year$target.mhealth.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$MHEALTH
                       current_year$target.smokes.imd=filter(private$targetdata_for_mort, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$SMOKES
                       
                       #Health costs    
                       current_year$thcosts=filter(private$hcosts, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$HCOSTS
                       #EQ5D   
                       current_year$teq5d=filter(private$eq5d, AGE==current_year$age,  MALE==current_year$male, SEP==current_year$sep)$EQ5D
                       #Opportunity costs    
                       if(current_year$age<5 | current_year$age>14){current_year$op_cost=0
                       }else {current_year$op_cost=(2000*current_year$no_recip)/(current_year$no_total*10)}

                       if(current_year$age<15){current_year$smr.mhealth=0
                       }else if (current_year$age>=15 & current_year$age<=44){current_year$smr.mhealth=3.21
                       }else if (current_year$age>=45 & current_year$age<=64){current_year$smr.mhealth=1.75
                       }else {current_year$smr.mhealth=1.18}
                     #Probability of dying from CHD  

                       if(current_year$age<16){current_year$mp.chd=0
                       }else if (current_year$age>=16 & current_year$age<=24 & current_year$male=="TRUE"){current_year$mp.chd=0.19
                       }else if (current_year$age>=16 & current_year$age<=24 & current_year$male=="FALSE"){current_year$mp.chd=0.06
                       }else if (current_year$age>=25 & current_year$age<=34 & current_year$male=="TRUE"){current_year$mp.chd=1.24
                       }else if (current_year$age>=25 & current_year$age<=34 & current_year$male=="FALSE"){current_year$mp.chd=0.49
                       }else if (current_year$age>=35 & current_year$age<=44 & current_year$male=="TRUE"){current_year$mp.chd=2.81
                       }else if (current_year$age>=35 & current_year$age<=44 & current_year$male=="FALSE"){current_year$mp.chd=1.26
                       }else if (current_year$age>=45 & current_year$age<=54 & current_year$male=="TRUE"){current_year$mp.chd=1.83
                       }else if (current_year$age>=45 & current_year$age<=54 & current_year$male=="FALSE"){current_year$mp.chd=1.13
                       }else if (current_year$age>=55 & current_year$age<=64 & current_year$male=="TRUE"){current_year$mp.chd=1.61
                       }else if (current_year$age>=55 & current_year$age<=64 & current_year$male=="FALSE"){current_year$mp.chd=1.43
                       }else if (current_year$age>=65 & current_year$age<=74 & current_year$male=="TRUE"){current_year$mp.chd=2.07
                       }else if (current_year$age>=65 & current_year$age<=74 & current_year$male=="FALSE"){current_year$mp.chd=1.95
                       }else if (current_year$age>=75 & current_year$male=="TRUE"){current_year$mp.chd=5.31
                       }else {current_year$mp.chd=8.82}
                       
                      current_year$pdeath= (filter(private$mortality_probs, AGE==current_year$age, MALE==current_year$male,
                              SEP==current_year$sep)$PROB_MORT)*(1+(current_year$smr.mhealth-1)*(current_year$mhealth))+(current_year$mp.chd/100)*(current_year$chd-current_year$target.chd.imd)

                       if(runif(1)<( (filter(private$mortality_probs, AGE==current_year$age, MALE==current_year$male,
                                            SEP==current_year$sep)$PROB_MORT)*(1+(current_year$smr.mhealth-1)*(current_year$mhealth))+(current_year$mp.chd/100)*(current_year$chd-current_year$target.chd.imd))){
                         current_year$life_stage = "dead"
                       } else {
                         current_year$life_stage = private$get_life_stage(current_year$age)
                         current_year$effect=private$get_effect(previous_year$effect, current_year$age ,current_year$sex, current_year$kessler_p, current_year$sdqcond3, current_year$sd_soc5, current_year$recip)
                         current_year$effect2=private$get_effect2(previous_year$effect2, current_year$age ,current_year$sex, current_year$kessler_p, current_year$sdqcond3, current_year$sd_soc5, current_year$sd_imp5, current_year$recip)
                         current_year$social_emotional_capital=private$get_social_emotional_capital(previous_year$social_emotional_capital, current_year$social_emotional_capital3, current_year$social_emotional_capital4, current_year$social_emotional_capital5, current_year$social_emotional_capital6, current_year$age, current_year$life_stage, current_year$effect)
                         current_year$impact=private$get_impact(  previous_year$impact,  current_year$impact2,   current_year$impact3,   current_year$impact4,  current_year$age,  current_year$life_stage, current_year$effect2 )
                         current_year$pr_cd=private$get_pr_cd( current_year$social_emotional_capital,current_year$impact, current_year$life_stage )
                         current_year$cd=private$get_cd( current_year$pr_cd,  current_year$life_stage )
                         current_year$dcd=private$get_dcd(previous_year$cd, current_year$cd)
                         current_year$cognitive_capital=private$get_cognitive_capital(previous_year$cognitive_capital, current_year$cognitive_capital3, current_year$cognitive_capital4, current_year$cognitive_capital5, current_year$cognitive_capital6, current_year$age, current_year$life_stage)
                         current_year$dsocial_emotional_capital=private$get_dsocial_emotional_capital(previous_year$social_emotional_capital, current_year$social_emotional_capital )
                         current_year$dcognitive_capital=private$get_dcognitive_capital(previous_year$cognitive_capital, current_year$cognitive_capital )
                         current_year$pr_mhealth=private$get_pr_mhealth(current_year$target.mhealth, previous_year$pr_mhealth, previous_year$cd, previous_year$dcd, previous_year$demployed, previous_year$ddepr, current_year$sex, current_year$life_stage, current_year$age, current_year$target.cd )
                         current_year$pr_mhealth_trend=private$get_pr_mhealth_trend(current_year$pr_mhealth, current_year$trend.mhealth)
                         current_year$mhealth=private$get_mhealth(current_year$pr_mhealth_trend, current_year$life_stage)
                         current_year$dmhealth=private$get_dmhealth(previous_year$mhealth, current_year$mhealth )
                         
                         current_year$pr_edu=private$get_pr_edu(current_year$target.edu, previous_year$cognitive_capital, previous_year$social_emotional_capital, previous_year$mhealth, current_year$age,  current_year$sd.soc, current_year$sd.cog, current_year$target.soc, current_year$target.cog, current_year$target.mhealth)
                         current_year$edu=private$get_edu(previous_year$edu, current_year$pr_edu, current_year$age)
                         
                         current_year$pr_prison=private$get_pr_prison(current_year$target.prison, previous_year$pr_prison, previous_year$cd, current_year$life_stage, current_year$age, previous_year$mhealth, previous_year$dmhealth, current_year$target.cd, current_year$target.mhealth )
                         current_year$prison=private$get_prison(current_year$pr_prison, current_year$life_stage)
                         current_year$dprison=private$get_dprison(previous_year$prison, current_year$prison)
                         
                         current_year$pr_res=private$get_pr_res(current_year$target.res, previous_year$pr_res, current_year$life_stage, current_year$age,  current_year$mhealth, current_year$dmhealth, current_year$target.mhealth)
                         current_year$res=private$get_res(current_year$pr_res, current_year$life_stage)


                         current_year$pr_employed=private$get_pr_employed(current_year$target.employed, previous_year$pr_employed, previous_year$social_emotional_capital, previous_year$cognitive_capital, current_year$age, current_year$sd.soc, current_year$sd.cog, current_year$target.soc, current_year$target.cog, current_year$trend.unemployed)
                         current_year$employed=private$get_employed(current_year$pr_employed, current_year$life_stage, current_year$prison)
                         current_year$demployed=private$get_demployed(previous_year$employed, current_year$employed)
                         current_year$cum_unemployed=private$get_cum_unemployed(current_year$life_stage, previous_year$cum_unemployed, current_year$employed)
                         current_year$cum_employed=private$get_cum_employed(previous_year$cum_employed, current_year$employed)
                         current_year$sd_earn=private$get_sd_earn(current_year$age, current_year$life_stage, current_year$se.earn,current_year$se.tr.earn)
                           
                         current_year$pot_earnings = private$get_pot_earnings(current_year$target.earnings, previous_year$pot_earnings, current_year$life_stage, current_year$age , current_year$sex, previous_year$cognitive_capital, previous_year$social_emotional_capital, current_year$edu,  current_year$sd.soc, current_year$sd.cog, current_year$target.soc, current_year$target.cog, current_year$target.edu , current_year$trend.earn,  current_year$sd_earn, current_year$employed) 
                         current_year$earnings = private$get_earnings(current_year$pot_earnings, current_year$life_stage,  current_year$employed)
                         current_year$dearnings=private$get_dearnings(previous_year$earnings, current_year$earnings)
                         current_year$pension=private$get_pension(current_year$life_stage, current_year$cum_employed )
                         
                         current_year$interest=private$get_interest(previous_year$wealth)
                         
                         current_year$taxes=private$get_taxes(current_year$life_stage, current_year$earnings, current_year$interest, current_year$pension )
                         
                         current_year$savings=private$get_savings(current_year$life_stage, current_year$earnings,   current_year$interest, current_year$taxes, previous_year$consumption )
                          
                          current_year$wealth = private$get_wealth(previous_year$wealth,  current_year$earnings, current_year$interest, current_year$income_p, current_year$consumption,   current_year$life_stage, current_year$taxes, current_year$res, current_year$savings, current_year$pension, current_year$min_cons)                         
                          
                          
                          current_year$benefits=private$get_benefits(current_year$life_stage, current_year$earnings, current_year$income_p, previous_year$wealth,current_year$interest, current_year$min_cons, current_year$pension,current_year$taxes,current_year$res )
                          
                         current_year$consumption = private$get_consumption(current_year$age, current_year$life_stage, current_year$earnings, previous_year$consumption, current_year$taxes, previous_year$wealth, current_year$interest, current_year$res, current_year$min_cons, current_year$savings, current_year$pension)
                         current_year$consumption_op = private$get_consumption_op(current_year$age, current_year$life_stage, current_year$earnings, previous_year$consumption_op, current_year$taxes, previous_year$wealth, current_year$interest, current_year$res, current_year$min_cons, current_year$savings, current_year$pension,  current_year$op_cost, previous_year$op_cost)
                         
                         current_year$dconsumption=private$get_dconsumption(previous_year$consumption, current_year$consumption )

                         current_year$sep=private$get_sep(current_year$consumption, current_year$savings, current_year$interest, current_year$sep_b, current_year$age)
                         
                         
                          current_year$depr=private$get_depr(current_year$life_stage, previous_year$depr,   current_year$consumption, current_year$pov_line)
                          current_year$ddepr=private$get_ddepr(previous_year$depr, current_year$depr)
                          
                         current_year$pr_unh_beh_np = private$get_pr_unh_beh_np(current_year$target.smokes,  previous_year$pr_unh_beh_np, current_year$depr, current_year$ddepr, current_year$mhealth, current_year$dmhealth, current_year$ch_unh_beh, current_year$edu,  current_year$age, current_year$sex, current_year$target.ch_smokes, current_year$target.edu,    current_year$target.mhealth,  current_year$target.depr)
                         
                         
                         current_year$pr_unh_beh_p =private$get_pr_unh_beh_p(current_year$age, current_year$sex, previous_year$pr_unh_beh_p,  current_year$prison, current_year$target.prison, current_year$dprison)
                         current_year$pr_unh_beh=private$get_pr_unh_beh(current_year$pr_unh_beh_np,  current_year$pr_unh_beh_p,   current_year$trend.smokes)
                         
                         current_year$unh_beh = private$get_unh_beh(current_year$pr_unh_beh, current_year$age)
                         current_year$dunh_beh=private$get_dunh_beh(previous_year$unh_beh, current_year$unh_beh )
                         current_year$cum_unh_beh=private$get_cum_unh_beh(current_year$unh_beh, previous_year$cum_unh_beh)
                         
                         current_year$pr_chd=private$get_pr_chd(current_year$target.chd, previous_year$pr_chd, current_year$unh_beh, current_year$dunh_beh,  current_year$depr,  current_year$ddepr,  current_year$age,  current_year$sex, current_year$target.smokes, current_year$target.depr )
                         current_year$pr_chd_trend=private$get_pr_chd_trend(current_year$pr_chd, current_year$trend.chd)
                         current_year$chd=private$get_chd(current_year$pr_chd_trend)
                         
                          current_year$health=private$get_health(current_year$teq5d, current_year$chd, current_year$target.chd.imd, current_year$mhealth, current_year$target.mhealth.imd)
                          current_year$ahcosts=private$get_ahcosts(current_year$thcosts, current_year$chd, current_year$target.chd.imd, current_year$mhealth, current_year$target.mhealth.imd)
                          
                          
                          current_year$cd_costs=private$get_cd_costs(current_year$life_stage, current_year$age, current_year$cd)
                          current_year$mhealth_costs=private$get_mhealth_costs(current_year$mhealth)
                          current_year$chd_costs=private$get_chd_costs(current_year$chd)
                          current_year$prison_costs=private$get_prison_costs(current_year$life_stage, current_year$prison)
                          current_year$res_costs=private$get_res_costs(current_year$life_stage, current_year$res, current_year$wealth, current_year$consumption)
                          
                          current_year$public_costs=private$get_public_costs(current_year$cd_costs, current_year$mhealth_costs, current_year$chd_costs, current_year$prison_costs, current_year$res_costs)
                          
                          current_year$gov_budget=private$get_gov_budget(previous_year$gov_budget, current_year$taxes, current_year$benefits, current_year$public_costs)
                           
                          }
                       private$life_history = private$life_history %>% bind_rows(current_year)
                       previous_year = current_year
                     }
                     return(invisible(self))
                   }
                 ),
                 
                 private = list(
                   rdf=NULL,
                   mortality_probs = NULL,
                   life_expectancy = NA,
                   life_history = NULL,
                   targetdata_for_mort=NULL,
                   targetdata_sexage=NULL,
                   average_data_total=NULL,
                   targetdata_trends=NULL,
                   eq5d=NULL,
                   hcosts=NULL,
                   no_recip=NULL,
                   no_total=NULL,
                   get_life_stage = function(age){
                     return(case_when(
                       age < 5 ~ "pre_school_years",
                       age >= 5 & age <= 18 ~ "school_years",
                       age >= 19 & age <= 69 ~ "working_years",
                       age > 69 ~ "retirement"
                     ))
                   },
                   
                   
           #Examples of policy effects on SDQ conduct and impact scores (if modelling policy)      
                   get_effect=function(previous_effect, age, sex, kessler_p, sdqcond3, sd_soc5,  recip){
                     if(age==5){effect=0*max(0, ((0.46+(3.4/37)*(kessler_p/3.45-12.2/10.5)+(1.74/37)*(sdqcond3/(sd_soc5*10)-139.4/37))*sd_soc5+0*rnorm(1, mean=0, sd=sd_soc5*0.46/4))*recip)
                     }else {
                       effect=previous_effect  
                     }   
                     return(effect)},
                   
                   
                   get_effect2=function(previous_effect2, age, sex, kessler_p, sdqcond3, sd_soc5, sd_imp5,  recip){
                     if(age==5){effect2=0*max(0, ((0.46+(3.4/37)*(kessler_p/3.45-12.2/10.5)+(1.74/37)*(sdqcond3/(sd_soc5*10)-139.4/37))*sd_imp5+0*rnorm(1, mean=0, sd=sd_imp5*0.46/4))*recip)
                     }else {
                       effect2=previous_effect2  
                     }   
                     return(effect2)},
                   
                   
                    #Social-emotional capital (1-SDQ conduct problems score)
                   
                   get_social_emotional_capital = function( previous_social_emotional_capital, social_emotional_capital3,  social_emotional_capital4,  social_emotional_capital5,  social_emotional_capital6, age, life_stage, effect){
                     if(life_stage != "school_years"){ 
                       social_emotional_capital=previous_social_emotional_capital
                     }else {
                       if(age <7) { social_emotional_capital=max(0, min(1, social_emotional_capital3+effect)) }
                       else if(age <11) { social_emotional_capital=max(0, min(1, social_emotional_capital4+effect))}
                       else if( age <14) { social_emotional_capital=max(0, min(1, social_emotional_capital5+effect))}
                       else  {social_emotional_capital=max(0, min(1, social_emotional_capital6+effect))}}
                     return(social_emotional_capital)
                   }, 
                   #Impact of problems
                   get_impact = function( previous_impact, impact2,  impact3,  impact4, age, life_stage, effect2 ){
                     if(life_stage != "school_years"){
                       impact=previous_impact
                     }else {if(age <7) { impact=max(0, impact3-effect2) }
                       else if(age <11) { impact=max(0, impact4-effect2) }
                       else if(age <14) { impact=max(0, impact4-effect2) }
                       else  { impact=max(0, impact4-effect2) } }
                     return(impact)
                   }, 
                   
                   #Prob. of developing conduct disorder 
                   get_pr_cd = function( social_emotional_capital, impact, life_stage ){
                     if(life_stage != "school_years"){pr_cd =0.06} 
                     else if(social_emotional_capital<=0.5 & impact>=2 ){ pr_cd =0.61 
                     }else if(social_emotional_capital<=0.6){ pr_cd=0.31 } 
                     else{pr_cd=0.06}
                     return(pr_cd)
                   },
                   #Conduct disorder
                   get_cd = function( pr_cd, life_stage ){
                     if(life_stage != "school_years"){cd =0
                     } else if (runif(1)< pr_cd){cd=1} else {cd=0}
                     return(cd)
                   },
                   get_dcd=function(previous_cd, cd ){
                     
                     return(cd-previous_cd)
                   } ,
                   
                   #Cognitive skills
                   get_cognitive_capital = function( previous_cognitive_capital, cognitive_capital3,  cognitive_capital4,  cognitive_capital5,  cognitive_capital6, age, life_stage ){
                     if(life_stage != "school_years"){
                       cognitive_capital=previous_cognitive_capital
                     }else {if(age <7) { cognitive_capital=cognitive_capital3 }
                       else if(age <11) { cognitive_capital=cognitive_capital4 }
                       else if(age <14) { cognitive_capital=cognitive_capital5 }
                       else  { cognitive_capital=cognitive_capital6 } }
                     return(cognitive_capital)
                   }, 

                   
                   get_dsocial_emotional_capital = function(previous_social_emotional_capital, social_emotional_capital){
                     return(social_emotional_capital-previous_social_emotional_capital)
                   },
                   get_dcognitive_capital = function(previous_cognitive_capital, cognitive_capital){
                     return(cognitive_capital-previous_cognitive_capital)
                   },
                   
                     
                   #Prob. of developing depression              
                   get_pr_mhealth = function(target.mhealth, previous_pr_mhealth, previous_cd, previous_dcd, previous_demployed, previous_ddepr, sex, life_stage, age, target.cd ){
                     if(life_stage=="pre_school_years"){ pr_mhealth=0}
                     else if(age==5  ) { pr_mhealth=max(0, min(1, (((target.mhealth/(1-target.mhealth))*exp(log(3.63)*(previous_cd-target.cd) ))^(-1)+1)^(-1)))
                     }else if(age>5 & life_stage=="school_years") { pr_mhealth=max(0, min(1, (((previous_pr_mhealth/(1-previous_pr_mhealth))*exp(log(3.63)*previous_dcd))^(-1)+1)^(-1)))
                     }else if(life_stage=="working_years"){pr_mhealth=max(0, min(1, (((previous_pr_mhealth/(1-previous_pr_mhealth))*exp(log(((2.05-1)*sex+1)*((1.72-1)*(1-sex)+1))*(-previous_demployed))*exp(log(1.24)*previous_ddepr))^(-1)+1)^(-1)))
                     # }else if(life_stage=="working_years"& previous_demployed>=0){pr_mhealth=max(0, min(1, (((previous_pr_mhealth/(1-previous_pr_mhealth))*exp(log(((0.87-1)*sex+1)*((0.79-1)*(1-sex)+1))*previous_demployed)*exp(log(1.24)*previous_ddepr))^(-1)+1)^(-1)))
                     }else{pr_mhealth=previous_pr_mhealth}
                     return(pr_mhealth)
                   },
                   
                   #Adding trend to depression prob. 
                   get_pr_mhealth_trend = function(pr_mhealth, trend.mhealth){
                     pr_mhealth_trend=pr_mhealth+trend.mhealth
                     return(pr_mhealth_trend)
                   },
                   
                   #Depression
                   get_mhealth = function(pr_mhealth_trend, life_stage){
                     if(life_stage=="pre_school_years"){mhealth=0}
                     else if (runif(1)< pr_mhealth_trend){mhealth=1} else {mhealth=0}
                     return(mhealth)
                   },
                   
                   get_dmhealth  = function(previous_mhealth , mhealth ){
                     return(mhealth-previous_mhealth )
                   },
                   
                   #Prob.  of  university degree (model at age 19)
                   get_pr_edu = function(target.edu, previous_cognitive_capital, previous_social_emotional_capital, previous_mhealth, age, sd.soc, sd.cog, target.soc, target.cog, target.mhealth){
                     if(age==19){pr_edu=max(0, min(1,  target.edu+(0.12/sd.cog)*(previous_cognitive_capital-target.cog)+ (0.02/sd.soc)*(previous_social_emotional_capital-target.soc)-0.04*(previous_mhealth-target.mhealth)))}
                     else  {pr_edu=0}
                     return(pr_edu)},
                   #University degree
                     get_edu = function(previous_edu, pr_edu, age){
                     if(age==19){ if(runif(1) < pr_edu){ return(1)} else {return(0)}}
                     else {return(previous_edu)}},
                 
           #Prob. of going to prison
                   get_pr_prison = function(target.prison, previous_pr_prison, previous_cd, life_stage, age, previous_mhealth, previous_dmhealth, target.cd, target.mhealth ){
                     if(life_stage != "working_years"){pr_prison =0
                     } else if(age==19){ pr_prison=max(0 , min(1, target.prison+0.18*(previous_cd-target.cd)+0.015*(previous_mhealth-target.mhealth)))
                     } else { pr_prison=max(0 , min(1, previous_pr_prison+0.015*previous_dmhealth))}
                     return(pr_prison)
                   },
                   #In prison
                   get_prison = function(pr_prison, life_stage){
                     if(life_stage!= "working_years"){prison=0}
                     else if (runif(1) < pr_prison){ prison=1} else {prison=0}
                     return(prison)},
                   
                   get_dprison=function(previous_prison, prison ){
                     
                     return(prison-previous_prison)
                   } ,
                   
                   
                   #Prob. of being in  care     
                   get_pr_res = function(target.res, previous_pr_res, life_stage, age, mhealth, dmhealth, target.mhealth ){
                     if(life_stage != "retirement"){pr_res =0
                     } else if(age==70){ pr_res=max(0 , min(1, target.res+0.18*(mhealth-target.mhealth)))
                     } else { pr_res=max(0 , min(1, previous_pr_res+0.18*dmhealth))}
                     return(pr_res)
                   },
                   #In care
                   get_res = function(pr_res, life_stage){
                     if(life_stage!= "retirement"){res =0}
                     else if (runif(1) < pr_res){ res=1} else {res=0}
                     return(res)},

                   
                   #Prob. of being employed
                   get_pr_employed=function(target.employed, previous_pr_employed, previous_social_emotional_capital, previous_cognitive_capital, age, sd.soc , sd.cog , target.soc, target.cog, trend.unemployed){
                     if(age<=18 ){pr_employed=0} 
                     else if(age>=70 ){pr_employed=0}
                     else if(age==19){pr_employed=min(1, max(0, target.employed+0.016*(previous_social_emotional_capital-target.soc)/sd.soc+0.021*(previous_cognitive_capital-target.cog)/sd.cog))}
                     else {pr_employed=previous_pr_employed-trend.unemployed}
                     return(pr_employed)
                   },
                  #Being employed
                                    
                   get_employed=function(pr_employed, life_stage, prison){
                     if(life_stage !="working_years"){ employed=0}
                     else if(runif(1)<pr_employed){employed=1} else {employed=0}
                     if( prison==0) {return(employed)} else{return(0)}
                   },
                   
                   get_demployed=function(previous_employed, employed ){
                   
                   return(employed-previous_employed)
                 } ,
                 
                 get_cum_unemployed=function(life_stage,previous_cum_unemployed, employed){
                   if(life_stage =="pre_school_years"|life_stage =="school_years"){ cum_unemployed=0}
                  else if (life_stage =="working_years"){cum_unemployed=previous_cum_unemployed+(1-employed)}
                   else {cum_unemployed=previous_cum_unemployed}
                 },
                 
                 get_cum_employed=function(previous_cum_employed, employed){
                   return(previous_cum_employed+employed)
                 },
                 
                 #Earnings       
                 get_sd_earn = function(age, life_stage, se.earn, se.tr.earn ){
                   if(life_stage !="working_years"){
                     sd_earn=0
                   } else if(age==19){sd_earn=rnorm(1, mean=0, sd=se.earn)
                   } else {sd_earn=rnorm(1, mean=0, sd=se.tr.earn)}
                   return(sd_earn)},
                 
                 get_pot_earnings = function(target.earnings, previous_pot_earnings, life_stage, age , sex, previous_cognitive_capital, previous_social_emotional_capital, edu,  sd.soc, sd.cog, target.soc, target.cog, target.edu, trend.earn, sd_earn, employed ){
                   if(life_stage !="working_years"){
                     pot_earnings=0
                   } else if(age==19  ){ 
                     pot_earnings= (target.earnings+ sd_earn)*exp(log(1.072)*(previous_cognitive_capital- target.cog)/sd.cog+log(1.0040)*(previous_social_emotional_capital- target.soc)/sd.soc+log( 1.17*(sex)+1.37*(1-sex))*(edu-target.edu))
                   } else if(employed==1){pot_earnings=max(0, previous_pot_earnings+trend.earn+sd_earn)
                 } else {pot_earnings=max(0, previous_pot_earnings+sd_earn)}
                   return(pot_earnings)},
                 
                 
                 
                 
                  get_earnings = function(pot_earnings, life_stage, employed){
 if(employed==0){earnings=0}
  else {earnings=pot_earnings}
  return(earnings)
},


get_dearnings=function(previous_earnings, earnings ){
  
  return(earnings-previous_earnings)
} ,

#Pension
get_pension=function(life_stage, cum_employed){
  if(life_stage!="retirement"){ return(0)
  }else if (cum_employed>=10){ return(170*4*12)}
  else {return(0)
  }},
#Interest
get_interest = function(previous_wealth){
  interest=previous_wealth*(0.01)
  return(interest)},

#Taxes

get_taxes=function(life_stage, earnings,  interest, pension){
  if(life_stage=="pre_school_years"|life_stage=="school_years"){return(0)
  }else if(earnings+pension+interest<=11500){return(0)
  }else if(earnings+pension+interest<=45000) {return(0.2*(earnings+pension+interest-11500))
  }else if(earnings+pension+interest<=150000) {return((45000-11500)*0.2+0.4*(earnings+pension+interest-45000))
  }else{return((45000-11500)*0.2+(150000-45000)*0.4+(earnings+pension+interest-150000)*0.45)}
},


#Savings
                  

get_savings=function(life_stage, earnings, interest, taxes, previous_consumption){
  if(life_stage!="working_years"){ return(0)
  }else if(earnings+interest-taxes<=previous_consumption){return(0)
  }else if( ((earnings+interest-taxes-previous_consumption)/(earnings+interest-taxes))<=0.16) {return(earnings+interest-taxes-previous_consumption)
  }else{return(0.16*(earnings+interest-taxes)) }
},



#Wealth

get_wealth = function(previous_wealth,  earnings, interest, income_p, consumption, life_stage, taxes, res, savings, pension, min_cons){
  if( (life_stage=="pre_school_years"|life_stage=="school_years") & income_p<min_cons) {wealth =max(0, previous_wealth+interest+income_p-consumption)}
  else if(life_stage=="retirement" ){wealth=max(0, previous_wealth+interest+pension-taxes-consumption-29270*res)}
  else if(life_stage=="working_years" & interest+earnings-taxes<min_cons ){wealth= max(0, previous_wealth+interest+earnings-taxes-consumption)}
  else {wealth=previous_wealth+savings}
  return(wealth)},

 
#Benefits
get_benefits=function(life_stage, earnings, income_p, previous_wealth,interest, min_cons, pension, taxes, res){
  if(life_stage=="pre_school_years" |life_stage=="school_years"  ){
    return( max(0, min_cons-income_p-previous_wealth-interest))
  } else 
    return( max(0, min_cons-earnings-pension-previous_wealth-interest+taxes+29270*res)) },


#Consumption, no oportunity cost

 
get_consumption = function(age, life_stage, earnings,previous_consumption, taxes, previous_wealth, interest, res, min_cons, savings, pension ){
  if(life_stage=="pre_school_years"|life_stage=="school_years"){
    return( previous_consumption)
  } else if(life_stage=="working_years"){
    return(max(min_cons, interest+earnings-taxes-savings ))} 
  else if(previous_wealth+interest-taxes+pension-res*29270>=previous_consumption){return(previous_consumption)}
  else {return(max( min_cons, previous_wealth+interest-taxes+pension-res*29270))}
},



#Consumption until 19 with + oportunity cost

get_consumption_op = function(age, life_stage, earnings, previous_consumption_op, taxes, previous_wealth, interest, res, min_cons, savings, pension, op_cost, previous_op_cost){
  if(life_stage=="pre_school_years"){
    return(previous_consumption_op)
    }else if(age==5){
      return(previous_consumption_op-op_cost)
      }else if(age<=14){
        return(previous_consumption_op)
      }else if(age==15){
        return(previous_consumption_op+previous_op_cost)
      }else if( age<19){
        return(previous_consumption_op)
    } else {return(NA)}
},
                  
 

get_dconsumption=function(previous_consumption, consumption ){
  dconsumption=consumption-previous_consumption 
  return(dconsumption)
} ,

   #SEP   
                 
                 get_sep=function(consumption, savings, interest, sep_b, age){
                   if (age<19){return(sep_b)}
                   else if(consumption+savings<18000){return(1)
                   }else if(consumption+savings<23000){return(2)
                   }else if(consumption+savings<31000){return(3)
                   }else if(consumption+savings<42000){return(4)
                   }else{return(5)}
                 },
                 


#Poverty

get_depr=function(life_stage, previous_depr, consumption, pov_line){
  if(life_stage=="pre_school_years"|life_stage=="school_years"){return(previous_depr)
    }else if(consumption<pov_line){return(1)
    }else{return(0)}
},


get_ddepr = function(previous_depr, depr){
  return(depr-previous_depr)
},



#Prob. of Smoking  (part 1)
 get_pr_unh_beh_np = function(target.smokes, previous_pr_unh_beh_np, depr, ddepr, mhealth, dmhealth, ch_unh_beh, edu, age, sex, target.ch_smokes, target.edu,  target.mhealth, target.depr ){
  if(age<=18){pr_unh_beh_np=0
  }else if( age==19){  pr_unh_beh_np=max(0 , min(1, (((target.smokes/(1-target.smokes))*exp(log(((3.38-1)*sex+1)*((3.68-1)*(1-sex)+1))*(ch_unh_beh-target.ch_smokes))*
                                                        exp(log(((1.91-1)*sex+1)*((1.81-1)*(1-sex)+1))*(depr-target.depr))*
                                                        exp(log(((3.32-1)*sex+1)*((3.26-1)*(1-sex)+1))*(target.edu-edu))*
                                                        exp(log(2.7)*(mhealth-target.mhealth)))^(-1)+1)^(-1)))
  }else if( age>19 & age<70){pr_unh_beh_np=max(0 , min(1, (((previous_pr_unh_beh_np/(1-previous_pr_unh_beh_np))*
                                                              exp(log(((1.91-1)*sex+1)*((1.81-1)*(1-sex)+1))*(ddepr))*
                                                              exp(log(2.7)*(dmhealth)))^(-1)+1)^(-1)))
  }else{pr_unh_beh_np=max(0 , min(1, (((previous_pr_unh_beh_np/(1-previous_pr_unh_beh_np))*
                                         exp(log(((1.91-1)*sex+1)*((1.81-1)*(1-sex)+1))*(ddepr))*
                                         exp(log(2.7)*(dmhealth)))^(-1)+1)^(-1)))}
  return(pr_unh_beh_np)
},

 #Adding the effect of being in prison on smoking (part 2)

get_pr_unh_beh_p = function(age, sex,   previous_pr_unh_beh_p, prison, target.prison, dprison){
  if(age==19){pr_unh_beh_p=max(0, min(1, ((0.07-1)*sex+1)*((0.06-1)*(1-sex)+1)*(prison-target.prison)))
  }else   {pr_unh_beh_p=max(0, min(1,previous_pr_unh_beh_p+((0.07-1)*sex+1)*((0.06-1)*(1-sex)+1)*(dprison)))}
  return(pr_unh_beh_p)
},

#Total prob. of smoking
get_pr_unh_beh = function(pr_unh_beh_np,  pr_unh_beh_p,  trend.smokes){
  return(max(0, min(1, pr_unh_beh_np+pr_unh_beh_p+trend.smokes)))
},
#Smoking

get_unh_beh = function(pr_unh_beh, age ){
  if(age<=18){unh_beh=0}
 else if(runif(1)<pr_unh_beh) { unh_beh=1 } else { unh_beh=0 }
  return(unh_beh)
},



get_dunh_beh=function(previous_unh_beh, unh_beh ){
  return(unh_beh-previous_unh_beh )
} ,


get_cum_unh_beh=function(unh_beh, previous_cum_unh_beh){
  cum_unh_beh=previous_cum_unh_beh+unh_beh
  return(cum_unh_beh)
},

  #Prob. of CHD
get_pr_chd = function(target.chd, previous_pr_chd, unh_beh, dunh_beh, depr, ddepr, age, sex, target.smokes, target.depr ){
  if(age<19){pr_chd=0
  }else if(age==19 ){pr_chd=max(0 , min(1, (((target.chd/(1-target.chd))*exp(log(2)*(unh_beh-target.smokes))*
                                               exp(log(((1.49-1)*sex+1)*((1.18-1)*(1-sex)+1))*(depr-target.depr)))^(-1)+1)^(-1)))
  }else {pr_chd=max(0 , min(1, (((previous_pr_chd/(1-previous_pr_chd))*exp(log(2)*dunh_beh )*exp(log(((1.49-1)*sex+1)*((1.18-1)*(1-sex)+1))*ddepr))^(-1)+1)^(-1)))}
  return(pr_chd)
},
#Adding trend
get_pr_chd_trend = function(pr_chd, trend.chd){
  pr_chd_trend=pr_chd+trend.chd
  return(pr_chd_trend)
},
#CHD
get_chd = function(pr_chd_trend){
  if(runif(1) < pr_chd_trend){ chd =1 } else {chd =0 }
  return(chd)
},
                   
 
#Health modelled using average EQ5D
 get_health=function(teq5d, chd, target.chd.imd, mhealth, target.mhealth.imd){
new_health=min(1, max(0, teq5d-((teq5d-0.629)*(chd-target.chd.imd)+(mhealth-target.mhealth.imd)*(teq5d-0.603))))
 return(new_health)
                 },
                 
#Health modelled using average EQ5D 
 get_ahcosts=function(thcosts, chd, target.chd.imd,  mhealth, target.mhealth.imd){
 new_ahcosts=max(0, thcosts+(chd-target.chd.imd)*(0.5*678+0.5*947)+(mhealth-target.mhealth.imd)*(4350))
return(new_ahcosts)
 },
                   
#Public Costs

 get_cd_costs=function( life_stage, age, cd ){
   if(life_stage!="school_years"){cd_costs=0
   }else if(age>=5& age<=10){cd_costs=cd*(1113+157+882+23)
   }else if(age>=11& age<=16){cd_costs=cd*(101+63+1202+23)
   }else {cd_costs=cd*(101+63+23)}
   return(cd_costs)
},




get_mhealth_costs=function(mhealth){
  return(mhealth*(4350))
},

get_chd_costs=function(chd){ return(chd*(0.5*678+0.5*947))
},


get_prison_costs=function(life_stage, prison){
  if(life_stage!="working_years"){return(0)}
  else {return(prison*(29556+512+6576))}
  
},




get_res_costs=function(life_stage, res, wealth, consumption){
  if(life_stage!="retirement"){return(0)}
  else if(wealth-consumption<29270*res) {return(res*29270)}
  else {return(res*0)}
  
},


get_public_costs=function( cd_costs, mhealth_costs, chd_costs, prison_costs, res_costs){
  return(cd_costs + mhealth_costs+ chd_costs+prison_costs+res_costs)
  
},


#Total balance
get_gov_budget=function(previous_gov_budget, taxes, benefits, public_costs){
  return(previous_gov_budget+taxes-benefits-public_costs)
}

)
)

