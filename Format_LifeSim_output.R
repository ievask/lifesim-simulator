#SET WD

library(tidyverse)

memory.limit(size=56000)

#Directory where the simulation results are saved
directory1<-"res/"


#Directory where the full final compiled dataset will be saved
dir.create("final_dataset")
 
#set w to equal the total number of output files saved in the "\res" folder, i.e. if using a standard PC then set w=1, if using a multi-cluster computer then w>1

w=1
m=0

for(i in 1:w){
   load(paste(directory1, "norecip-", i, ".RData", sep=""))
   x<-norecips
   x<-bind_rows( x ,.id="id")
   x<-transform(x, id = as.numeric(id)) 
   assign("y_number", summarize(subset(x, age==0), count=n())[[1]])
   
   x<-mutate(x,  POLICY=FALSE, id=id+m)
   m<-y_number+m
   assign(paste("norecip-", i, ".RData", sep=""), x)}
rm(norecips)
all_set1 <- do.call(rbind, lapply( ls(patt="norecip"), get) )
rm(list=ls(pattern="norecip"))


keeps<-c("id",   "recip", "POLICY", "life_stage", "sex", "age", "sep" , "sep_b", "edu_p", 
          "mhealth_p2", "rutter_p", 
         "cognitive_capital", "social_emotional_capital", "impact", "effect", "effect2", "pr_cd", "cd", "pr_edu", "edu",
         "pr_unh_beh",  "pr_unh_beh_np",    "pr_unh_beh_p" , "unh_beh" , "pr_chd","pr_chd_trend", "chd", 
         "pr_mhealth","pr_mhealth_trend" , "mhealth", "health", "pr_employed", 
         "employed" , "earnings", "consumption", "consumption_op", "depr", "wealth", 
         "pension", "savings", "interest",
         "pr_prison" , "prison", "pr_res", "res", "pdeath",
         "taxes" , "benefits",  "check", 
         "cd_costs" , "prison_costs", "res_costs", "chd_costs", "mhealth_costs", "ahcosts", "public_costs"  )


all_set1<-select(all_set1, all_of(keeps) )




see<-subset(all_set1, age==0)
nnr=summarize(see, count=n())[[1]]

m=nnr
for(i in 1:w){
   load(paste(directory1, "recip-", i, ".RData", sep=""))
   x<-recips
   x<-bind_rows( x ,.id="id")
   x<-transform(x, id = as.numeric(id)) 
   assign("y_number", summarize(subset(x, age==0), count=n())[[1]])
   
   x<-mutate(x,  POLICY=FALSE, id=id+m)
   m<-y_number+m
   assign(paste("recip-", i, ".RData", sep=""), x)}
rm(recips)

all_set2 <- do.call(rbind, lapply( ls(patt="recip"), get) )
rm(list=ls(pattern="recip"))

all_set2<-select(all_set2, all_of(keeps) )


see<-subset(all_set2, age==0)
nr=summarize(see, count=n())[[1]]
m=nr+nnr


all_set<-bind_rows(all_set1, all_set2)
all_set1_pol<-mutate(all_set1,  POLICY=TRUE, id=id+m)
m=nr+2*nnr
rm(all_set1)

save(all_set,   "final_dataset/LifeSim_lives.RData" )

########################################################################################################
########################################################################################################
########################################################################################################
########################################################################################################
