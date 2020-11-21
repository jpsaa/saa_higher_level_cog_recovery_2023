
rm(list=ls()) #clear the workspace (removes all variables)

library(psych) ## descriptive stats
library(ggplot2) #figures
library(reshape2) #restructure and aggregate data
library(dplyr) #same but more advanced and for data frames
library(coin) ## wilcoxon signed-pratt test
library(asht) ### confidence intervals for wilcoxon signed-pratt test
library(readr) ### write_csv function
library(quantreg) ###quantile regression
library(glmertree) #### mixed regression trees
library(lme4) ### regressions with gamma
library(optimx) ###optimizers for lme4
library(ggraph) ### trajectory trees
library(igraph) ### trajectory trees
library(party)  ### linear regression trees
library(partykit) ### linear regression trees
library(dataPreparation) 
library(extrafont) ### embed fonts in plots
library(hrbrthemes) ### nice color palettes for figures
library(RColorBrewer) ### nice color palettes for figures

# sessionInfo()

# > sessionInfo()
# R version 3.5.0 (2018-04-23)
# Platform: x86_64-apple-darwin15.6.0 (64-bit)
# Running under: macOS  10.15.5
# 
# Matrix products: default
# BLAS: /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
# LAPACK: /Library/Frameworks/R.framework/Versions/3.5/Resources/lib/libRlapack.dylib
# 
# locale:
#   [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
# 
# attached base packages:
#   [1] stats4    grid      stats     graphics  grDevices utils     datasets  methods   base     
# 
# other attached packages:
#   [1] RColorBrewer_1.1-2    hrbrthemes_0.8.0      extrafont_0.17        dataPreparation_0.4.3 progress_1.2.2       
# [6] stringr_1.4.0         lubridate_1.7.4       party_1.3-0           strucchange_1.5-1     sandwich_2.4-0       
# [11] zoo_1.8-2             modeltools_0.2-21     igraph_1.2.2          ggraph_1.0.1.9999     optimx_2020-4.2      
# [16] glmertree_0.2-0       partykit_1.2-7        mvtnorm_1.0-10        libcoin_1.0-5         lme4_1.1-21          
# [21] Matrix_1.2-18         quantreg_5.54         SparseM_1.77          readr_1.1.1           asht_0.9.4           
# [26] bpcp_1.3.4            exact2x2_1.6.3.1      exactci_1.3-3         ssanv_1.1             coin_1.2-2           
# [31] survival_2.41-3       dplyr_0.8.3           reshape2_1.4.3        ggplot2_3.3.0         psych_1.8.12         


setwd( "/Users/Womps/Documents/U/SkyDrive/Australia Life/PhD year 3/S3 and START/Plots and scripts")

# s3 <- read.csv("singapore_data_july_12_2020.csv")
# 
# st <- read.csv("australia_data_july_12_2020.csv")

### reding data
s3 <- read.csv("singapore_data_sept_22_2020.csv")
st <- read.csv("australia_data_sept_22_2020.csv")

s3 <- s3[s3$stroke_type_p0=="Ischemic",]

#disability (moderate disability or more - all scores 90 or less)
s3 <- s3[s3$bi_prestroke_total>90,]


####checking date of mmse and FAB evaluations####
s3$P0qa8 <- as.Date(s3$P0qa8, "%d-%b-%y")

s3 %>%  filter(P0qg1=="Correct", P0qg2=="Correct", P0qg3=="Correct") %>% select(P0qa8, P0QG3ans, P0QG2ans, P0QG1ans ) %>% apply(2,trimws)





#### START and S3 ####
###time post stroke calculation #####
#### resp date is the date that the person responded to the questionnaires (taken from START variable dictionary)
st$enrol_to_moca_w1 <- as.numeric(
  as.Date(st$resp_date_1_w1, origin =  "1899-12-30") - 
    as.Date(st$calc_onset_time_w1, origin =  "1899-12-30") ) %>%  
  round(2)


### taking out probable ouliers
st$enrol_to_moca_w1 <- ifelse(st$enrol_to_moca_w1 < 0 | 
                                st$enrol_to_moca_w1 > 10 , NA, st$enrol_to_moca_w1)

### getting columns to compare in both datasets ####
### continuous
cols = s3 %>% select(
  age_onset_years,
  nihss_p0_total,
  cci_p0_total,
  onset_to_enrol
) %>%  names()

st[cols] %>% summary
s3[cols] %>% summary

### categorical
cols.cat <- s3 %>% select(
  ethnicity_p0_binary,
  marital_status_p0_binary,
  prev_stroke_p0,
  ht_p0,
  diab_p0_bin,
  ihd_p0,
  current_smoker_baseline,
  gender_p0,
  disab_prestroke,
  depre_baseline# PHQ2 for START and CES-D S3
) %>% names()


### checking categorical columns are factor with two levels
str(st[cols.cat])
str(s3[cols.cat])

#### creating ids for each patient
st$id <- 1:nrow(st)
s3$id <- 1:nrow(s3)

### creating datasets for comparisons #####
all.moca = st %>% filter (!is.na(moca_score_w1) & !is.na(moca_score_mo3) & !is.na(moca_score_mo12))
moca.base = st %>% filter (!is.na(moca_score_w1))
moca.incomp = moca.base %>% filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))
# no.moca = st %>% filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))
all.fab = s3 %>% filter (!is.na(fab_p0_total) & !is.na(fab_p3_total) & !is.na(fab_p12_total))
fab.base = s3 %>% filter (!is.na(fab_p0_total))
fab.incomp = fab.base %>% filter (is.na(fab_p3_total) | is.na(fab_p12_total))
# no.fab = s3 %>% filter (is.na(fab_p0_total) | is.na(fab_p3_total) | is.na(fab_p12_total))



#### Frontal MoCA change #### trails, clock drawing, attentions, and fluency

all.moca$moca_fs_w1 <- all.moca %>% select(grep("moca_w1",names(.))) %>%  
  select(grep("_exe_trail|_clock|_att_total|_lang3|_abs_total",names(.))) %>%
  mutate(moca_fs_w1=rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>% select(moca_fs_w1) %>% unlist


all.moca$moca_fs_mo3 <- all.moca %>% select(grep("moca_mo3",names(.))) %>%  
  select(grep("_exe_trail|_clock|_att_total|_lang3|_abs_total",names(.))) %>%
  mutate(moca_fs_mo3=rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>% select(moca_fs_mo3) %>% unlist


all.moca$moca_fs_mo12 <- all.moca %>% select(grep("moca_mo12",names(.))) %>%  
  select(grep("_exe_trail|_clock|_att_total|_lang3|_abs_total",names(.))) %>%
  mutate(moca_fs_mo12=rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>% select(moca_fs_mo12) %>% unlist

all.moca %>% select(grep("_abs_total",names(.))) %>% describe
all.moca %>% select(grep("_exe_trail",names(.))) %>% describe
all.moca %>% select(grep("_att_total",names(.))) %>% describe



s3 %>%  filter(P0qg1=="Correct", P0qg2=="Correct", P0qg3=="Correct") %>% select(P0qa8, P0QG1ans, P0QG2ans, P0QG3ans ) %>% apply(2,trimws)
all.fab %>%  filter(P0qg1=="Correct", P0qg2=="Correct", P0qg3=="Correct") %>% select(P0qa8, P0QG1ans, P0QG2ans, P0QG3ans ) %>% apply(2,trimws)


### baseline characteristics for Singapore ####
## continuous variables
cols <- s3 %>% select(cols, fab_p0_total, mmse_p0_total) %>% names
all.base=s3 %>%
  select(cols) %>%
  describe(IQR = T) %>% round(2) %>%
  select(n,median,IQR)

comp.num=all.fab %>%
  select(cols) %>%
  describe(IQR = T) %>% round(2) %>%
  select(n,median,IQR)

incomp.num=fab.incomp %>%
  select(cols) %>%
  describe(IQR = T) %>% round(2) %>%
  select(n,median,IQR)

table1_s3=cbind("All"=all.base[,1],
                "Median (IQR)"=paste0(all.base[,2]," (",round(all.base[,3],2),")"),
                "Included"=comp.num[,1],
                "Median (IQR)"=paste0(comp.num[,2]," (",round(comp.num[,3],2),")"),
                "Excluded"=incomp.num[,1],
                "Median (IQR)"=paste0(incomp.num[,2]," (",round(incomp.num[,3],2),")"))
table1_s3=cbind(gsub("_|w1|.x"," ",rownames(comp.num)),table1_s3)

### calculating differences between groups
table1_s3=cbind(table1_s3,
                "Estimate (95% C.I.)"=NA,
                "p-value"=NA)

for (i in seq_along(cols)) {
  table1_s3[i,"Estimate (95% C.I.)"]=paste0(round(wilcox.test(fab.base[,which(names(fab.base)==cols[i])],
                                                              fab.incomp[,which(names(fab.incomp)==cols[i])],
                                                              conf.int = T)$estimate, 3)," (",
                                            round(wilcox.test(fab.base[,which(names(fab.base)==cols[i])],
                                                              fab.incomp[,which(names(fab.incomp)==cols[i])],
                                                              conf.int = T)$conf.int,3)[1],"-",
                                            round(wilcox.test(fab.base[,which(names(fab.base)==cols[i])],
                                                              fab.incomp[,which(names(fab.incomp)==cols[i])],
                                                              conf.int = T)$conf.int,3)[2], ")")
  
  
  
  table1_s3[i,"p-value"]=round(wilcox.test(fab.base[,which(names(fab.base)==cols[i])],
                                           fab.incomp[,which(names(fab.incomp)==cols[i])],
                                           conf.int = T)$p.value,3)
  
}

table1_s3[,"p-value"]=ifelse(table1_s3[,"p-value"]==0,"<.001",table1_s3[,"p-value"])
table1_s3

rm(all.base,comp.num,incomp.num)


### frequencies

all.base=fab.base %>%
  select(cols.cat) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
all.base=trimws(all.base[-which(trimws(all.base[,1])=="NA's"),])

comp=all.fab %>%
  select(cols.cat) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
comp=trimws(comp[-which(trimws(comp[,1])=="NA's"),])

incomp=fab.incomp %>%
  select(cols.cat) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
incomp=trimws(incomp[-which(trimws(incomp[,1])=="NA's"),])


### matrix with values and percentages
freqs=matrix(c(as.numeric(all.base[,2]),round(as.numeric(all.base[,2])/nrow(fab.base)*100,2),
               as.numeric(comp[,2]),round(as.numeric(comp[,2])/nrow(fab.base)*100,2),
               as.numeric(incomp[,2]),round(as.numeric(incomp[,2])/nrow(fab.base)*100,2)),
             ncol=6,
             dimnames = list(comp[,1],
                             c("All","%","Included","%","Excluded", "%%")))
#### assigning categories
fab.base[,cols.cat]=data.frame(apply(fab.base[,cols.cat],2,factor)) # making sure all variables are factorized
freqs=cbind(
  "vars"=rep(cols.cat,lengths(lapply(fab.base[,cols.cat],levels))),
  freqs)

#### keep rows without NAs
freqs=freqs[!is.na(freqs[,"Included"]),]

### Fisher tests 
### applying fisher test to all the variables (in alphabetical order) and getting p-values
p.values=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                                            which(colnames(freqs)=="Excluded"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$p.value)),3)
est=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                                       which(colnames(freqs)=="Excluded"))],
                              freqs[,"vars"]),
                        function(x)
                          fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$estimate)),3)

c.int.lb=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                                            which(colnames(freqs)=="Excluded"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[1])),3)

c.int.ub=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                                            which(colnames(freqs)=="Excluded"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[2])),3)




### copying p-value into the table
freqs=cbind(freqs,"c.int"= paste0(est[match(freqs[,"vars"],
                                            gsub(".odds ratio","",names(est)))], " (",
                                  c.int.lb[match(freqs[,"vars"],names(c.int.lb))],
                                  "-",
                                  c.int.ub[match(freqs[,"vars"],names(c.int.ub))],
                                  ")"),
            "p-value"=p.values[match(freqs[,"vars"],names(p.values))])

### getting rid of duplicated values and adding names to table
freqs[duplicated(freqs[,"c.int"]),c("p-value","c.int")]=""

vars=as.character(freqs[,"vars"])
freqs[,'vars'] = rownames(freqs)

#### making count and percentage into the same column
freqs = matrix(freqs,ncol = ncol(freqs),
               dimnames = list(Variables = vars,
                               "Table 1: Baseline Characteristics"=
                                 c("Categories","All","%","Included","%%", "Excluded","%%%","c.int","p-value")))

freqs[,"All"]=paste0(freqs[,"All"]," (", freqs[,"%%"], "%)")
freqs[,"Included"]=paste0(freqs[,"Included"]," (", freqs[,"%%"], "%)")
freqs[,"Excluded"]=paste0(freqs[,"Excluded"]," (", freqs[,"%%%"], "%)")

### putting numerical values and frequencies together
freqs=freqs[,-grep("%",colnames(freqs))]
colnames(freqs)[grep("c.int",colnames(freqs))]="Estimate (95% C.I.)"
freqs=cbind("Variables"=rownames(freqs),freqs)
rownames(freqs)=rep("",nrow(freqs))


table1_s3=table1_s3[,-grep("Inc|Exc",colnames(table1_s3))]

colnames(table1_s3)[grep("Med",colnames(table1_s3))]=c("Categories","Included", "Excluded")

table1_s3=rbind(freqs,table1_s3)

#### Finalizing the table (removing duplicate rows)
table1_s3[duplicated(table1_s3[,"Variables"]),'Variables']=""

table1_s3

rm(all.base,comp,freqs,incomp,c.int.lb,c.int.ub,est,p.values,vars)


write.csv(table1_s3,"Table1_s3.csv")
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#### table 1 for paper - comparison between cohorts ####
 
#### continuous variables
### describing numerical variables

cols <- cols[-grep("mmse_|fab_",cols)]

s3.comp.num=all.fab %>%
  select(cols) %>%
  describe(IQR = T) %>% round(2) %>%
  select(n,median,IQR)

st.comp.num=all.moca %>%
  select(cols) %>%
  describe(IQR = T) %>% round(2) %>%
  select(n,median,IQR)

table1_pooled=cbind("Singapore"=s3.comp.num[,1],
                    "Median (IQR)"=paste0(s3.comp.num[,2]," (",round(s3.comp.num[,3],2),")"),
                    "Australia"=st.comp.num[,1],
                    "Median (IQR)"=paste0(st.comp.num[,2]," (",round(st.comp.num[,3],2),")"))
table1_pooled=cbind(gsub("_|w1|.x"," ",rownames(st.comp.num)),table1_pooled)

### calculating differences between groups
table1_pooled=cbind(table1_pooled,
                    "Estimate (95% C.I.)"=NA,
                    "p-value"=NA)

for (i in seq_along(cols)) {
  table1_pooled[i,"Estimate (95% C.I.)"]=
    paste0(
      ### mean estimate
      round(wilcox.test(all.fab[,which(names(all.fab)==cols[i])],
                        all.moca[,which(names(all.moca)==cols[i])],
                        conf.int = T)$estimate, 3)," (",
      ### 95% CI lower bound
      round(wilcox.test(all.fab[,which(names(all.fab)==cols[i])],
                        all.moca[,which(names(all.moca)==cols[i])],
                        conf.int = T)$conf.int,3)[1],"-",
      ### 95% CI upper bound
      round(wilcox.test(all.fab[,which(names(all.fab)==cols[i])],
                        all.moca[,which(names(all.moca)==cols[i])],
                        conf.int = T)$conf.int,3)[2], ")")
  
  
  ### p-value
  table1_pooled[i,"p-value"]=round(wilcox.test(all.fab[,which(names(all.fab)==cols[i])],
                                               all.moca[,which(names(all.moca)==cols[i])],
                                               conf.int = T)$p.value,3)
  
}

table1_pooled[,"p-value"]=ifelse(table1_pooled[,"p-value"]==0,"<.001",table1_pooled[,"p-value"])
table1_pooled



#### frequencies for both cohorts

sing.cat=all.fab %>%
  select(cols.cat) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
sing.cat=trimws(sing.cat[-grep("NA|NA's",trimws(sing.cat[,1])),])

aus.cat=all.moca %>%
  select(cols.cat) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
aus.cat=trimws(aus.cat[-grep("NA|NA's",trimws(aus.cat[,1])),])

### matrix with values and percentages
freqs=matrix(c(as.numeric(sing.cat[,2]),round(as.numeric(sing.cat[,2])/nrow(all.fab)*100,2),
               as.numeric(aus.cat[,2]),round(as.numeric(aus.cat[,2])/nrow(all.moca)*100,2)),
             ncol=4,
             dimnames = list(tolower(aus.cat[,1]),
                             c("Singapore","%","Australia","%%")))


#### assigning categories
# fab.base[,cols.cat]=data.frame(apply(fab.base[,cols.cat],2,factor)) # making sure all variables are factorized
freqs=cbind(
  "vars"=rep(cols.cat,lengths(lapply(all.fab[,cols.cat],levels))),
  freqs)

##### fisher tests 

### applying fisher test to all the variables (in alphabetical order) and getting p-values
p.values=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Singapore"),
                                            which(colnames(freqs)=="Australia"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$p.value)),3)


est=round(unlist(
  lapply(split(freqs[,c(which(colnames(freqs)=="Singapore"),
                        which(colnames(freqs)=="Australia"))],
               freqs[,"vars"]),
         function(x)
           fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$estimate)
),2) 


c.int.lb=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Singapore"),
                                            which(colnames(freqs)=="Australia"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[1])),2)

c.int.ub=round(unlist(lapply(split(freqs[,c(which(colnames(freqs)=="Singapore"),
                                            which(colnames(freqs)=="Australia"))],
                                   freqs[,"vars"]),
                             function(x)
                               fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[2])),2)




### copying p-value into the table
freqs=cbind(freqs,
            "stats"= paste0(est[match(freqs[,"vars"],gsub(".odds ratio","",names(est)))], " (",
                            c.int.lb[match(freqs[,"vars"],names(c.int.lb))],"-",
                            c.int.ub[match(freqs[,"vars"],names(c.int.ub))], ")"),
            "p-value"=p.values[match(freqs[,"vars"],names(p.values))])

freqs[,"p-value"]=ifelse(freqs[,"p-value"]==0,"<.001",freqs[,"p-value"])

### getting rid of duplicated values and adding names to table
freqs[duplicated(freqs[,"stats"]),c("p-value","stats")]=""

vars=as.character(freqs[,"vars"])
freqs[,'vars'] = rownames(freqs)

#### making cunt and percentage into the same column
freqs = matrix(freqs,ncol = ncol(freqs),
               dimnames = list(Variables = vars,
                               "Table 1: Baseline Characteristics"=
                                 c("Categories","Singapore","%","Australia","%%","stats","p-value")))

freqs[,"Singapore"]=paste0(freqs[,"Singapore"]," (", freqs[,"%"], "%)")
freqs[,"Australia"]=paste0(freqs[,"Australia"]," (", freqs[,"%%"], "%)")

### putting numerical values and frequencies together
freqs=freqs[,-grep("%",colnames(freqs))]
colnames(freqs)[grep("stats",colnames(freqs))]="Estimate (95% C.I.)"
freqs=cbind("Variables"=rownames(freqs),freqs)
rownames(freqs)=rep("",nrow(freqs))


table1_pooled=tibble("Variables"=table1_pooled[,1],
                     "Categories"="",
                     "Singapore"= paste0(table1_pooled[,2],", ",table1_pooled[,3]),
                     "Australia"= paste0(table1_pooled[,4],", ",table1_pooled[,5]),
                     "Estimate (95% C.I.)"=table1_pooled[,6],
                     "p-value"=table1_pooled[,7])

table1_pooled=rbind(freqs,table1_pooled)
rownames(table1_pooled)=1:nrow(table1_pooled)


#### table 1 comparison of baseline characteristics
table1_pooled[,"Variables"] <- as.character(table1_pooled[,"Variables"])
table1_pooled[duplicated(table1_pooled[,"Variables"]),'Variables']=""

table1_pooled

write.csv(table1_pooled,"table1_pooled.csv")


rm(aus.cat,freqs,s3.comp.num,sing.cat,st.comp.num,c.int.lb,c.int.ub, est,i,p.values,vars)



 

#### table 2 for paper - FAB over time ####
### calculating descriptive statistics for fab domains
fab.w1=all.fab %>% select(grep("fab_p0",names(all.fab))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

fab.mo3=all.fab %>% select(grep("fab_p3",names(all.fab))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

fab.mo12=all.fab %>% select(grep("fab_p12",names(all.fab))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)


table2=cbind("Baseline Median (IQR)"=paste0(fab.w1[,2]," (",fab.w1[,3],")"),
             "3 Months Median (IQR)"=paste0(fab.mo3[,2]," (",fab.mo3[,3],")"),
             "z-val.1 (95% CI)"=NA,
             "p-value 1"=NA,
             "12 Months Median (IQR)"=paste0(fab.mo12[,2]," (",fab.mo12[,3],")"),
             "z-val.2 (95% CI)"=NA,
             "p-value 2"=NA,
             "z-val.3 (95% CI)"=NA,
             "p-value 3"=NA)

### calculating differences between time-points
cols1=names(s3)[grep("fab_p0",names(s3))]
cols2=names(s3)[grep("fab_p3",names(s3))]
cols3=names(s3)[grep("fab_p12",names(s3))]

pratt1=list()
pratt2=list()
pratt3=list()

#### This loop runs very slow when running the pratt tests!!
for (i in seq_along(cols1)) {
  ##making sure there are no NAs
  .f1=all.fab[,which(names(all.fab)==cols1[i])]
  .f1=ifelse(is.na(.f1),0,.f1)
  .f2=all.fab[,which(names(all.fab)==cols2[i])]
  .f2=ifelse(is.na(.f2),0,.f2)
  .f3=all.fab[,which(names(all.fab)==cols3[i])]
  .f3=ifelse(is.na(.f3),0,.f3)
  #
  # ### running the actual tests
  # pratt1[[i]]=wsrTest(.f1,.f2)
  #
  # pratt2[[i]]=wsrTest(.f2,.f3)
  #
  # pratt3[[i]]=wsrTest(.f1,.f3)
  # 
  #   table2[i,"p-value 1"]=round(as.numeric(pratt1[[i]]["p.value"]),3)
  #   table2[i,"z-val.1 (95% CI)"]=paste0(round(unlist(pratt1[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt1[[i]]['conf.int']),3),collapse=", "),")")
  # 
  #   table2[i,"p-value 2"]=round(as.numeric(pratt2[[i]]["p.value"]),3)
  #   table2[i,"z-val.2 (95% CI)"]=paste0(round(unlist(pratt2[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt2[[i]]['conf.int']),3),collapse=", "),")")
  # 
  #   table2[i,"p-value 3"]=round(as.numeric(pratt3[[i]]["p.value"]),3)
  #   table2[i,"z-val.3 (95% CI)"]=paste0(round(unlist(pratt3[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt3[[i]]['conf.int']),3),collapse=", "),")")


}


### Fixing p-values that are close to 0

table2[,"p-value 1"]=ifelse(table2[,"p-value 1"]==0,"<.001",table2[,"p-value 1"])
table2[,"p-value 2"]=ifelse(table2[,"p-value 2"]==0,"<.001",table2[,"p-value 2"])
table2[,"p-value 3"]=ifelse(table2[,"p-value 3"]==0,"<.001",table2[,"p-value 3"])



### naming the columns
table2=cbind(`Item (range)`=
               c("Abstraction (similarities)", "Language (naming)","Motor series",
                 "Interference","Inhibition","Involuntary behavior", "Overall score"),
             table2)


write_csv(as_tibble(table2),"Table2-FAB.csv")

rm(pratt1,pratt2,pratt3,fab.w1,fab.mo12,fab.mo3)












#### table 2 for paper - MMSE over time ####
### calculating descriptive statistics for mmse domains
mmse.w1=all.fab %>%
  select(grep("_total|_orientation|immediate_|_attn|_delayed_rec|_language|_construction",
                                names(all.fab))) %>%
  select(grep("mmse_p0", names(.))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

mmse.mo3=all.fab %>%
  select(grep("_total|_orientation|immediate_|_attn|_delayed_rec|_language|_construction",
                                 names(all.fab))) %>%
  select(grep("mmse_p3", names(.))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

mmse.mo12=all.fab %>%
  select(grep("_total|_orientation|immediate_|_attn|_delayed_rec|_language|_construction",
              names(all.fab))) %>%
  select(grep("mmse_p12", names(.))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)


table2.mmse=cbind("Baseline Median (IQR)"=paste0(mmse.w1[,2]," (",mmse.w1[,3],")"),
             "3 Months Median (IQR)"=paste0(mmse.mo3[,2]," (",mmse.mo3[,3],")"),
             "z-val.1 (95% CI)"=NA,
             "p-value 1"=NA,
             "12 Months Median (IQR)"=paste0(mmse.mo12[,2]," (",mmse.mo12[,3],")"),
             "z-val.2 (95% CI)"=NA,
             "p-value 2"=NA,
             "z-val.3 (95% CI)"=NA,
             "p-value 3"=NA)



cols1=rownames(mmse.w1)
cols2=rownames(mmse.mo3)
cols3=rownames(mmse.mo12)
all.fab[cols1] %>% describe
pratt1=list()
pratt2=list()
pratt3=list()

##### again - pratt tests really slow
for (i in seq_along(cols1)) {
  #making sure there are no NAs
  .f1=all.fab[,which(names(all.fab)==cols1[i])]
  .f1=ifelse(is.na(.f1),0,.f1)
  .f2=all.fab[,which(names(all.fab)==cols2[i])]
  .f2=ifelse(is.na(.f2),0,.f2)
  .f3=all.fab[,which(names(all.fab)==cols3[i])]
  .f3=ifelse(is.na(.f3),0,.f3)

  ### running the actual tests
  # pratt1[[i]]=wsrTest(.f1,.f2)
  # 
  # pratt2[[i]]=wsrTest(.f2,.f3)
  # 
  # pratt3[[i]]=wsrTest(.f1,.f3)
  # 
  #   table2.mmse[i,"p-value 1"]=round(as.numeric(pratt1[[i]]["p.value"]),3)
  #   table2.mmse[i,"z-val.1 (95% CI)"]=paste0(round(unlist(pratt1[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt1[[i]]['conf.int']),3),collapse=", "),")")
  # 
  #   table2.mmse[i,"p-value 2"]=round(as.numeric(pratt2[[i]]["p.value"]),3)
  #   table2.mmse[i,"z-val.2 (95% CI)"]=paste0(round(unlist(pratt2[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt2[[i]]['conf.int']),3),collapse=", "),")")
  # 
  #   table2.mmse[i,"p-value 3"]=round(as.numeric(pratt3[[i]]["p.value"]),3)
  #   table2.mmse[i,"z-val.3 (95% CI)"]=paste0(round(unlist(pratt3[[i]]['estimate']),3)," (",
  #                                       paste0(round(unlist(pratt3[[i]]['conf.int']),3),collapse=", "),")")


}


### Fixing p-values that are close to 0

table2.mmse[,"p-value 1"]=ifelse(table2.mmse[,"p-value 1"]==0,"<.001",table2.mmse[,"p-value 1"])
table2.mmse[,"p-value 2"]=ifelse(table2.mmse[,"p-value 2"]==0,"<.001",table2.mmse[,"p-value 2"])
table2.mmse[,"p-value 3"]=ifelse(table2.mmse[,"p-value 3"]==0,"<.001",table2.mmse[,"p-value 3"])



### naming the columns
table2.mmse=cbind(`Item (range)`=
               gsub("attn","attention",trimws(gsub("mmse_p0_|_"," ",rownames(mmse.w1)))),
             table2.mmse)


# write_csv(as_tibble(table2.mmse),"table2.mmse.csv")
rm(pratt1,pratt2,pratt3,mmse.w1,mmse.mo12,mmse.mo3)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
#### table 2 for paper - MoCA FS over time ####
### calculating descriptive statistics for MoCA FS domains

all.moca$moca_fs_w1_trails_cube = all.moca %>%
  select(grep("_exe_trail|_clock",names(.))) %>%
  select(grep("_w1",names(.))) %>%
  mutate(moca_fs_w1_trails_cube =
           rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>%
  select(moca_fs_w1_trails_cube) %>% unlist

all.moca$moca_fs_mo3_trails_cube = all.moca %>%
  select(grep("_exe_trail|_clock",names(.))) %>%
  select(grep("_mo3",names(.))) %>%
  mutate(moca_fs_mo3_trails_cube =
           rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>%
  select(moca_fs_mo3_trails_cube) %>% unlist

all.moca$moca_fs_mo12_trails_cube = all.moca %>%
  select(grep("_exe_trail|_clock",names(.))) %>%
  select(grep("_mo12",names(.))) %>%
  mutate(moca_fs_mo12_trails_cube =
           rowSums(.,na.rm=TRUE) *
           ifelse(rowSums(is.na(.)) == ncol(.), NA, 1)) %>%
  select(moca_fs_mo12_trails_cube) %>% unlist



moca.fs.w1<-data.frame(
  total = all.moca %>% select(moca_fs_w1),
  moca_fs_w1_trails_cube = all.moca %>% select(moca_fs_w1_trails_cube),
  attn = all.moca %>%
    select(grep("_att_total",names(.))) %>%
    select(grep("_w1",names(.))) ,
  fluency = all.moca %>%
    select(grep("_lang3",names(.))) %>%
    select(grep("_w1",names(.))),
  abstraction = all.moca %>%
    select(grep("_abs_total",names(.))) %>% 
    select(grep("_w1",names(.)))
) %>% describe(IQR = T) %>%
  select(n,median,IQR)


moca.fs.mo3<-data.frame(
  total = all.moca %>% select(moca_fs_mo3),
  moca_fs_mo3_trails_cube = all.moca %>% select(moca_fs_mo3_trails_cube),
  attn = all.moca %>%
    select(grep("_att_total",names(.))) %>%
    select(grep("_mo3",names(.))) ,
  fluency = all.moca %>%
    select(grep("_lang3",names(.))) %>%
    select(grep("_mo3",names(.))),
  abstraction = all.moca %>%
    select(grep("_abs_total",names(.))) %>% 
    select(grep("_mo3",names(.)))
) %>% describe(IQR = T) %>%
  select(n,median,IQR)

moca.fs.mo12<-data.frame(
  total = all.moca %>% select(moca_fs_mo12),
  moca_fs_mo12_trails_cube = all.moca %>% select(moca_fs_mo12_trails_cube),
  attn = all.moca %>%
    select(grep("_att_total",names(.))) %>%
    select(grep("_mo12",names(.))) ,
  fluency = all.moca %>%
    select(grep("_lang3",names(.))) %>%
    select(grep("_mo12",names(.))),
  abstraction = all.moca %>%
    select(grep("_abs_total",names(.))) %>% 
    select(grep("_mo12",names(.)))
) %>% describe(IQR = T) %>%
  select(n,median,IQR)

table2.moca.fs=cbind("Baseline Median (IQR)"=paste0(moca.fs.w1[,2]," (",moca.fs.w1[,3],")"),
                     "3 Months Median (IQR)"=paste0(moca.fs.mo3[,2]," (",moca.fs.mo3[,3],")"),
                     "z-val.1 (95% CI)"=NA,
                     "p-value 1"=NA,
                     "12 Months Median (IQR)"=paste0(moca.fs.mo12[,2]," (",moca.fs.mo12[,3],")"),
                     "z-val.2 (95% CI)"=NA,
                     "p-value 2"=NA,
                     "z-val.3 (95% CI)"=NA,
                     "p-value 3"=NA)



cols1=rownames(moca.fs.w1)
cols2=rownames(moca.fs.mo3)
cols3=rownames(moca.fs.mo12)

all.moca[cols1] %>% describe
pratt1=list()
pratt2=list()
pratt3=list()

for (i in seq_along(cols1)) {
  #making sure there are no NAs
  .f1=all.moca[,which(names(all.moca)==cols1[i])]
  .f1=ifelse(is.na(.f1),0,.f1)
  .f2=all.moca[,which(names(all.moca)==cols2[i])]
  .f2=ifelse(is.na(.f2),0,.f2)
  .f3=all.moca[,which(names(all.moca)==cols3[i])]
  .f3=ifelse(is.na(.f3),0,.f3)
  
  ### running the actual tests
  # pratt1[[i]]=wsrTest(.f1,.f2)
  # 
  # pratt2[[i]]=wsrTest(.f2,.f3)
  # 
  # pratt3[[i]]=wsrTest(.f1,.f3)
  # 
  # table2.moca.fs[i,"p-value 1"]=round(as.numeric(pratt1[[i]]["p.value"]),3)
  # table2.moca.fs[i,"z-val.1 (95% CI)"]=paste0(round(unlist(pratt1[[i]]['estimate']),3)," (",
  #                                             paste0(round(unlist(pratt1[[i]]['conf.int']),3),collapse=", "),")")
  # 
  # table2.moca.fs[i,"p-value 2"]=round(as.numeric(pratt2[[i]]["p.value"]),3)
  # table2.moca.fs[i,"z-val.2 (95% CI)"]=paste0(round(unlist(pratt2[[i]]['estimate']),3)," (",
  #                                             paste0(round(unlist(pratt2[[i]]['conf.int']),3),collapse=", "),")")
  # 
  # table2.moca.fs[i,"p-value 3"]=round(as.numeric(pratt3[[i]]["p.value"]),3)
  # table2.moca.fs[i,"z-val.3 (95% CI)"]=paste0(round(unlist(pratt3[[i]]['estimate']),3)," (",
  #                                             paste0(round(unlist(pratt3[[i]]['conf.int']),3),collapse=", "),")")
  
  
}


### Fixing p-values that are close to 0

table2.moca.fs[,"p-value 1"]=ifelse(table2.moca.fs[,"p-value 1"]==0,"<.001",table2.moca.fs[,"p-value 1"])
table2.moca.fs[,"p-value 2"]=ifelse(table2.moca.fs[,"p-value 2"]==0,"<.001",table2.moca.fs[,"p-value 2"])
table2.moca.fs[,"p-value 3"]=ifelse(table2.moca.fs[,"p-value 3"]==0,"<.001",table2.moca.fs[,"p-value 3"])



### naming the columns
table2.moca.fs=cbind(`Item (range)`=
                       trimws(gsub("_"," ",rownames(moca.fs.w1))),
                     table2.moca.fs)


# write_csv(as_tibble(table2.moca.fs),"table2.moca.fs.csv")
rm(pratt1,pratt2,pratt3,cols1,cols2,cols3,moca.fs.w1,moca.fs.mo3,moca.fs.mo12)




















#### Figure 1 for paper - FAB trajectory plot - change in scores ####

all.fab <- all.fab %>%
  mutate(trends =
           ifelse(
             fab_p12_total > fab_p3_total &
               fab_p3_total > fab_p0_total,
             '1.overall improver',
             ifelse(fab_p3_total > fab_p0_total &
                      fab_p3_total == fab_p12_total,
                    '2.improved-stable',
                    ifelse(fab_p3_total > fab_p0_total &
                             fab_p12_total < fab_p3_total,
                           '3.improved-declined',
                           ifelse(fab_p3_total == fab_p0_total &
                                    fab_p12_total > fab_p3_total ,
                                  '4.stable-improved',
                                  ifelse(fab_p12_total == fab_p3_total &
                                           fab_p3_total == fab_p0_total,
                                         '5.overall stable',
                                         ifelse(fab_p3_total == fab_p0_total &
                                                  fab_p12_total < fab_p3_total,
                                                '6.stable-declined',
                                                ifelse(fab_p3_total < fab_p0_total &
                                                         fab_p12_total > fab_p3_total,
                                                       '7.declined-improved',
                                                       ifelse(fab_p3_total < fab_p0_total &
                                                                fab_p3_total == fab_p12_total,
                                                              '8.declined-stable',
                                                              ifelse(fab_p12_total < fab_p3_total &
                                                                       fab_p3_total < fab_p0_total,
                                                                     '9.overall decliner',NA))))))))))

fab<-all.fab %>% select(id,fab_p0_total,fab_p3_total,fab_p12_total,trends)
table(fab$trends)


fab.scores=fab[,-which(names(fab)=="trends")]

fab.scores<-melt(fab.scores,id.vars = 'id')

names(fab.scores)=c("id", 'time', 'score')
fab=cbind(fab.scores,fab$trends)
names(fab)[4]="trends"


#removing letters from columns
fab$time=as.numeric(as.character(factor(fab$time,
                                        labels = c(7,30,365))))



# testing approach with data sequences

fab12=data.frame(round((table(fab$trends)/3)*100/nrow(all.fab),2))
sum(fab12$Freq)

fab3<- all.fab %>%
  mutate (fab3 = ifelse(fab_p3_total < fab_p0_total,
                        '3.decliner',
                        ifelse(fab_p3_total > fab_p0_total,
                               '1.improver',
                               ifelse(fab_p3_total == fab_p0_total,
                                      '2.stable',NA)))) %>% select(fab3) %>%  table() %>% data.frame()


#### Asssigning actual values 
d1=data.frame(from="onset", to=paste0(c('Improved','Stable','Declined'),' at 3mo'))
d2=data.frame(from=rep(d1$to, each=3), to=paste0(fab12$Var1,' (',fab12$Freq,')'))
edges=rbind(d1, d2)
# adding a second data frame with information for each node!
name=unique(c(as.character(edges$from), as.character(edges$to)))
vertices=data.frame(
  name=name,
  group=c(NA, rep(c("Improved","Stable","Declined"),4)),
  Three.Months=c(NA, rep(c("Improved","Stable","Declined"),4)),
  value=as.numeric(c(nrow(fab)/nlevels(factor(fab$time)),
                     fab3$Freq,fab12$Freq))
)

####Final figure FAB trajectory
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram') +
  geom_edge_diagonal2(aes(colour=node.group, width=node.value, alpha=0.8))+
  geom_node_text(aes( label=name, filter=leaf, color=Three.Months) , 
                 angle=0 , hjust=0, nudge_y=0.03, size = 3) +
  geom_node_point(aes(filter=leaf, size=value, color=Three.Months) , 
                  alpha=0.5) +
  
  # ggtitle(paste0('        Figure 2 - Changes in Frontal Assessment Battery from week 1 to 3 and 12 months post-stroke\n',
  #                '        n = ',nrow(fab)/3))+
  theme_ipsum(axis_text_size = 9) +
  theme(legend.position="bottom",
        legend.text = element_text(size = 8),
        plot.title = element_text(family = "Tahoma",
                                  face = "bold",hjust = 0.5, size = 12),
        text=element_text(family="Tahoma", angle = 0, vjust = 0),
        legend.title = element_text(colour="gray10",size=8),
        axis.line = element_line(size = 1, colour = "grey100"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
        
  ) +
  scale_x_continuous(name ="Trajectory group",
                     breaks=NULL) +
  
  scale_color_manual(
    name="Outcome at 12 months \n (number of participants)",
    guide="none",
    values = c("indianred1","lightseagreen","cornflowerblue")
    
    
  )+
  
  scale_size_continuous(name="Percentage of participants at 12 months")+
  scale_edge_width(guide='none',
                   range=c(0.2,6))+
  scale_edge_color_manual(name='Overall trajectory',
                          labels=c('Declined','Improved','Stable','0-3-12 month trajectory'),
                          values = c("indianred1","lightseagreen","cornflowerblue"),
                          na.value = "grey80")+
  scale_edge_alpha(guide='none') +
  coord_flip(expand = T, ylim = c(2,-.3), xlim = c(9,-1)) +
  scale_x_reverse(name = "", breaks=NULL) +
  scale_y_reverse(name ="",
                  breaks=c(2,1,0),
                  labels = c("Baseline","3-months","12-months")) +
  guides(edge_color=guide_legend(ncol=4, nrow = 1, title.position = "top"),
         
         size=guide_legend(ncol=4, nrow = 2, title.position = "top")) 


ggsave("Fig-1-tree-FAB.png",width = 12, height = 7, type = "cairo", dpi = 320)





















#### Figure 1 for paper - MoCA FS trajectory plot - change in scores ####

all.moca <- all.moca %>%
  mutate(fs_trends =
           ifelse(
             moca_fs_mo12 > moca_fs_mo3 &
               moca_fs_mo3 > moca_fs_w1,
             '1.overall improver',
             ifelse(moca_fs_mo3 > moca_fs_w1 &
                      moca_fs_mo3 == moca_fs_mo12,
                    '2.improved-stable',
                    ifelse(moca_fs_mo3 > moca_fs_w1 &
                             moca_fs_mo12 < moca_fs_mo3,
                           '3.improved-declined',
                           ifelse(moca_fs_mo3 == moca_fs_w1 &
                                    moca_fs_mo12 > moca_fs_mo3 ,
                                  '4.stable-improved',
                                  ifelse(moca_fs_mo12 == moca_fs_mo3 &
                                           moca_fs_mo3 == moca_fs_w1,
                                         '5.overall stable',
                                         ifelse(moca_fs_mo3 == moca_fs_w1 &
                                                  moca_fs_mo12 < moca_fs_mo3,
                                                '6.stable-declined',
                                                ifelse(moca_fs_mo3 < moca_fs_w1 &
                                                         moca_fs_mo12 > moca_fs_mo3,
                                                       '7.declined-improved',
                                                       ifelse(moca_fs_mo3 < moca_fs_w1 &
                                                                moca_fs_mo3 == moca_fs_mo12,
                                                              '8.declined-stable',
                                                              ifelse(moca_fs_mo12 < moca_fs_mo3 &
                                                                       moca_fs_mo3 < moca_fs_w1,
                                                                     '9.overall decliner',NA))))))))))

moca.fs<-all.moca %>% mutate (id=1:nrow(all.moca)) %>%  select(id,moca_fs_w1,moca_fs_mo3,moca_fs_mo12,fs_trends)
table(moca.fs$fs_trends)


moca.fs.scores=moca.fs[,-which(names(moca.fs)=="fs_trends")]

moca.fs.scores<-melt(moca.fs.scores,id.vars = 'id')

names(moca.fs.scores)=c("id", 'time', 'score')
moca.fs=cbind(moca.fs.scores,moca.fs$fs_trends)
names(moca.fs)[4]="fs_trends"


#removing letters from columns
moca.fs$time=as.numeric(as.character(factor(moca.fs$time,
                                            labels = c(7,30,365))))
moca.fs=na.omit(moca.fs)




moca.fs12=data.frame(round((table(moca.fs$fs_trends)/3)*100/nrow(all.moca),2))
sum(moca.fs12$Freq)

moca.fs3<- all.moca %>%
  mutate (moca.fs3 = ifelse(moca_fs_mo3 < moca_fs_w1,
                            '3.decliner',
                            ifelse(moca_fs_mo3 > moca_fs_w1,
                                   '1.improver',
                                   ifelse(moca_fs_mo3 == moca_fs_w1,
                                          '2.stable',NA)))) %>% select(moca.fs3) %>%  table() %>% data.frame()


#### Asssigning actual values 
d1=data.frame(from="onset", to=paste0(c('Improved','Stable','Declined'),' at 3mo'))
d2=data.frame(from=rep(d1$to, each=3), to=paste0(moca.fs12$Var1,' (',moca.fs12$Freq,')'))
edges=rbind(d1, d2)
# adding a second data frame with information for each node!
name=unique(c(as.character(edges$from), as.character(edges$to)))
vertices=data.frame(
  name=name,
  group=c(NA, rep(c("Improved","Stable","Declined"),4)),
  Three.Months=c(NA, rep(c("Improved","Stable","Declined"),4)),
  value=as.numeric(c(219,moca.fs3$Freq,moca.fs12$Freq))
)

####Final figure MoCA FS trajectory
mygraph <- graph_from_data_frame(edges, vertices=vertices)
ggraph(mygraph, layout = 'dendrogram') +

  geom_edge_diagonal2(aes(colour=node.group, width=node.value, alpha=0.8))+
  geom_node_text(aes( label=name, filter=leaf, color=Three.Months) , 
                 angle=0 , hjust=0, nudge_y=0.03, size = 3) +
  geom_node_point(aes(filter=leaf, size=value, color=Three.Months) , 
                  alpha=0.5) +
  
  # ggtitle(paste0('        Figure 2 - Changes in MoCA Frontal scores from week 1 to 3 and 12 months post-stroke\n',
  #                '        n = ',nrow(moca.fs)/3))+
  theme_ipsum(axis_text_size = 9) +
  theme(legend.position="bottom",
        legend.text = element_text(size = 8),
        plot.title = element_text(family = "Tahoma",
                                  face = "bold",hjust = 0.5, size = 12),
        text=element_text(family="Tahoma", angle = 0, vjust = 0),
        legend.title = element_text(colour="gray10",size=8),
        axis.line = element_line(size = 1, colour = "grey100"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank()
        
  ) +
  scale_x_continuous(name ="Trajectory group",
                     breaks=NULL) +
  
  scale_color_manual(
    name="Outcome at 12 months \n (number of participants)",
    guide="none",
    values = c("indianred1","lightseagreen","cornflowerblue")
    
    
  )+
  
  scale_size_continuous(name="Percentage of participants at 12 months")+
  scale_edge_width(guide='none',
                   range=c(0.2,6))+
  scale_edge_color_manual(name='Overall trajectory',
                          labels=c('Declined','Improved','Stable','0-3-12 month trajectory'),
                          values = c("indianred1","lightseagreen","cornflowerblue"),
                          na.value = "grey80")+
  scale_edge_alpha(guide='none') +
  coord_flip(expand = T, ylim = c(2,-.3), xlim = c(9,-1)) +
  scale_x_reverse(name = "", breaks=NULL) +
  scale_y_reverse(name ="",
                  breaks=c(2,1,0),
                  labels = c("Baseline","3-months","12-months")) +
  guides(edge_color=guide_legend(ncol=4, nrow = 1, title.position = "top"),
         size=guide_legend(ncol=4, nrow = 1, title.position = "top")) 


ggsave("Fig-1-tree-moca-fs.png",width = 12, height = 7,type = "cairo", dpi = 320)

rm(vertices,moca.fs.scores,moca.fs3,moca.fs12,fab3,fab12,d1,d2,edges,mygraph)


###### differences in trajectory groups #####

##### groups match??? ####
.pos.match <- names(sort(table(fab$trends), decreasing = T ))==
  names(sort(table(moca.fs$fs_trends)/3, decreasing = T ))


## similar trajectory (yes / no)
chisq.test(
  matrix(
  c(
    sum(sort(table(fab$trends), decreasing = T )[.pos.match]), ## yes
       sum(sort(table(fab$trends), decreasing = T )) - 
        sum(sort(table(fab$trends), decreasing = T )[.pos.match]), ## no
       sum(sort(table(moca.fs$fs_trends), decreasing = T )[.pos.match]), ## yes
       sum(sort(table(moca.fs$fs_trends), decreasing = T )) -
       sum(sort(table(moca.fs$fs_trends), decreasing = T )[.pos.match])), ## no
  ncol = 2)
)$p.value

ks.test(table(fab$trends),table(moca.fs$fs_trends))



 
###### FAB modeling - prepping data for longitudinal analysis ####
.num_vars=cols[-grep("onset_to|mmse|fab",cols)]
.fact_vars=cols.cat

formulas <- expand.grid(list(
  paste(t(combn(.fact_vars, 1, simplify = T))[,1]),
  
  paste(t(combn(.num_vars, 2, simplify = T))[,1], "+",
        t(combn(.num_vars, 2, simplify = T))[,2]
        
  )))


#creating combinations of formulas to evaluate
formulas <- paste("time","+",formulas$Var1,"+",formulas$Var2)


fab.long.adj=all.fab[,c("id","fab_p3_total","fab_p12_total")]
fab.long.adj=melt(fab.long.adj, id.vars = "id")
names(fab.long.adj)=c("id","time","fab.score")
levels(fab.long.adj$time)=c("90 days","365 days")
fab.long.adj <- data.frame(fab.long.adj,
                           all.fab %>%
                             select(fab_p0_total,.num_vars,.fact_vars))

fab.long.adj <- fab.long.adj[order(fab.long.adj$id),]

# plot(density(fab.long.adj$moca.score))
describe(all.fab$fab_p12_total - all.fab$fab_p3_total)
describe(all.fab$fab_p3_total - all.fab$fab_p0_total)


### any value below 61 causes one or more models to not converge 
fab.long.adj$fab.score.gamma <- (fab.long.adj$fab.score - 61)*-1 
fab.long.adj$fab_baseline_gamma <- (fab.long.adj$fab_p0_total - 61)*-1

# plot(density(fab.long.adj$fab.score.gamma))
# plot(density(fab.long.adj$fab_baseline_gamma))

####No NAs
fab.long.adj <- fab.long.adj[!is.na(fab.long.adj$id),]

### ascending ID numbers
fab.long.adj$id <- as.character(factor(fab.long.adj$id,
                                       labels = c(1:length(unique(fab.long.adj$id)))))

#### rescaling continuous variables
.num_vars
.scales <- build_scales(dataSet = fab.long.adj, cols = c(.num_vars), verbose = TRUE)
fab.long.adj <- fastScale(dataSet = fab.long.adj, scales = .scales, verbose = TRUE)

#####https://stats.stackexchange.com/questions/111467/is-it-necessary-to-scale-the-target-value-in-addition-to-scaling-features-for-re
.fab_sc <- build_scales(dataSet = fab.long.adj, cols = "fab_baseline_gamma", verbose = TRUE)
fab.long.adj <- fastScale(dataSet = fab.long.adj, scales = .fab_sc, verbose = TRUE)

# plot(density(fab.long.adj$fab_baseline_gamma))

#### FAB model without additional variables ####
fit_model <- function (pid, formula, data) {
  
  test_data <- data[data$id == pid,]
  train_data <- data[data$id != pid,]
  
  
  optimx_options <- c("nlminb", "bobyqa","L-BFGS-B", "nmkb", "hjkb", "uobyqa","newuoa", "nlm")
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    mod.glmer <- glmer(
      formula(formula),
      data=train_data,
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if(is.null(mod.glmer@optinfo$conv$lme4$messages)){
      print(paste0("Model converged with the, ", optimx_options[i],", optimization!, pid = ",
                   pid," formula = ",formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
    
  }
  
  
  #
  # http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
  #
  pred.glmer <- predict(mod.glmer, newdata=test_data, allow.new.levels=T)
  
  RMSE <- function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  
  rmse <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"fab.score.gamma"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse = rmse,
                    observed = as.matrix(data[data$id==pid,"fab.score.gamma"]) %>% paste(collapse = ", "),
                    predicted = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    patient = pid)
  return(out)
  
}

## testing function
lapply(30:35, fit_model, "fab.score.gamma ~ time + offset(fab_baseline_gamma) + (1 | id) -1", fab.long.adj)

cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
  
}


### results adjusted by baseline score (only for models with smoking) --> I am providing the results as an RDS file separately 
models.adj.fab.base.offset.time.only <- lapply("fab.score.gamma ~ time + offset(fab_baseline_gamma) + (1 | id) -1", cross_validate, fab.long.adj)
models.adj.fab.base.offset.time.only <- data.frame(purrr::map_df(models.adj.fab.base.offset.time.only, bind_rows))
saveRDS(models.adj.fab.base.offset.time.only,"models-FAB-adjusted-by-baseline-offset-time-only.rds")




###### FAB model validation  ####

fit_model <- function (pid, formula, data) {
  
  test_data <- data[data$id == pid,]
  train_data <- data[data$id != pid,]
  
  
  optimx_options <- c("nlminb", "bobyqa","L-BFGS-B", "nmkb", "hjkb", "uobyqa","newuoa", "nlm")
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    mod.glmer <- glmer(
      formula(trimws(paste("fab.score.gamma ~ ",formula, "+ offset(fab_baseline_gamma) + (1 | id) -1"))),
      data=train_data,
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if(is.null(mod.glmer@optinfo$conv$lme4$messages)){
      print(paste0("Model converged with the, ", optimx_options[i],", optimization!, pid = ",
                   pid," formula = ",formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
    
  }
  
  
  #
  # http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
  #
  pred.glmer <- predict(mod.glmer, newdata=test_data, allow.new.levels=T)
  
  RMSE <- function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  
  rmse <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"fab.score.gamma"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse = rmse,
                    observed = as.matrix(data[data$id==pid,"fab.score.gamma"]) %>% paste(collapse = ", "),
                    predicted = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    patient = pid)
  return(out)
  
}

## testing function
lapply(30, fit_model, formulas[10], fab.long.adj)

cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
  
}


### results adjusted by baseline score (only for models with smoking) --> I am providing the results as an RDS file separately 
models.adj.fab.base.offset.currrent.smoke <- lapply(formulas[grep("current_smoker",formulas)], cross_validate, fab.long.adj)
models.adj.fab.offset.currrent.smoke <- data.frame(purrr::map_df(models.adj.fab.base.offset.currrent.smoke, bind_rows))
saveRDS(models.adj.fab.offset.currrent.smoke,"models-FAB-adjusted-by-baseline-offset-current-smokers.rds")



### results adjusted by baseline score (the line blocked line below takes several hours to run) --> I am providing the results as an RDS file separately 
# models.adj.fab.base.offset <- lapply(formulas, cross_validate, fab.long.adj)
# models.fab.adj.baseline.offset <- data.frame(purrr::map_df(models.adj.fab.base.offset, bind_rows))
# saveRDS(models.fab.adj.baseline.offset,"models-FAB-adjusted-by-baseline-offset.rds")










###### MoCA FS modeling - prepping data for longitudinal analysis ####
all.moca$id <- 1:nrow(all.moca)
moca.fs.long.adj=all.moca[,c("id","moca_fs_mo3","moca_fs_mo12")]
moca.fs.long.adj=melt(moca.fs.long.adj, id.vars = "id")
names(moca.fs.long.adj)=c("id","time","moca.fs.score")
levels(moca.fs.long.adj$time)=c("90 days","365 days")
moca.fs.long.adj <- data.frame(moca.fs.long.adj,
                               all.moca %>%
                                 select(moca_fs_w1,.num_vars,.fact_vars))

moca.fs.long.adj <- moca.fs.long.adj[order(moca.fs.long.adj$id),]

# plot(density(moca.fs.long.adj$moca.score))
describe(all.moca$moca_fs_mo12 - all.moca$moca_fs_mo3)
describe(all.moca$moca_fs_mo3 - all.moca$moca_fs_w1)


moca.fs.long.adj$moca.fs.score.gamma <- (moca.fs.long.adj$moca.fs.score - 24)*-1
moca.fs.long.adj$moca_fs_baseline_gamma <- (moca.fs.long.adj$moca_fs_w1 - 24)*-1

# plot(density(moca.fs.long.adj$moca.fs.score.gamma))
# plot(density(moca.fs.long.adj$moca_fs_baseline_gamma))

# making sure all patients have ID numbers
moca.fs.long.adj <- moca.fs.long.adj[!is.na(moca.fs.long.adj$id),]

moca.fs.long.adj$id <- as.character(factor(moca.fs.long.adj$id,
                                           labels = c(1:length(unique(moca.fs.long.adj$id)))))


##### scaling continuous variables (only for gamma models)
.num_vars
.scales <- build_scales(dataSet = moca.fs.long.adj, cols = c(.num_vars), verbose = TRUE)
moca.fs.long.adj <- fastScale(dataSet = moca.fs.long.adj, scales = .scales, verbose = TRUE)

#####https://stats.stackexchange.com/questions/111467/is-it-necessary-to-scale-the-target-value-in-addition-to-scaling-features-for-re
.moca_fs_sc <- build_scales(dataSet = moca.fs.long.adj, cols = "moca_fs_baseline_gamma", verbose = TRUE)
moca.fs.long.adj <- fastScale(dataSet = moca.fs.long.adj, scales = .moca_fs_sc, verbose = TRUE)

# plot(density(moca.fs.long.adj$moca_fs_baseline_gamma))


#### MoCA FS model without additional variables ####
fit_model <- function (pid, formula, data) {
  
  test_data <- data[data$id == pid,]
  train_data <- data[data$id != pid,]
  
  
  optimx_options <- c("nlminb", "bobyqa","L-BFGS-B", "nmkb", "hjkb", "uobyqa","newuoa", "nlm")
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    mod.glmer <- glmer(
      formula(formula),
      data=train_data,
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if(is.null(mod.glmer@optinfo$conv$lme4$messages)){
      print(paste0("Model converged with the, ", optimx_options[i],", optimization!, pid = ",
                   pid," formula = ",formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
    
  }
  
  
  #
  # http://svmiller.com/blog/2018/06/mixed-effects-models-optimizer-checks/
  #
  pred.glmer <- predict(mod.glmer, newdata=test_data, allow.new.levels=T)
  
  RMSE <- function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  
  rmse <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"moca.fs.score.gamma"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse = rmse,
                    observed = as.matrix(data[data$id==pid,"moca.fs.score.gamma"]) %>% paste(collapse = ", "),
                    predicted = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    patient = pid)
  return(out)
  
}

## testing function
lapply(30:35, fit_model, "moca.fs.score.gamma ~ time + offset(moca_fs_baseline_gamma) + (1 | id) -1", moca.fs.long.adj)

cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
  
}


### results adjusted by baseline score (only for models with smoking) --> I am providing the results as an RDS file separately 
models.adj.moca.fs.base.offset.time.only <- lapply("moca.fs.score.gamma ~ time + offset(moca_fs_baseline_gamma) + (1 | id) -1", cross_validate, moca.fs.long.adj)
models.adj.moca.fs.base.offset.time.only <- data.frame(purrr::map_df(models.adj.moca.fs.base.offset.time.only, bind_rows))
saveRDS(models.adj.moca.fs.base.offset.time.only,"models-MoCA-FS-adjusted-by-baseline-offset-time-only.rds")



###### MoCA FS model validation ####

fit_model <- function (pid, formula, data) {
  
  test_data <- data[data$id == pid,]
  train_data <- data[data$id != pid,]
  
  optimx_options <- c("nlminb", "bobyqa","L-BFGS-B", "nmkb", "hjkb", "uobyqa","newuoa", "nlm")
  
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    mod.glmer <- glmer(
      formula(trimws(paste("moca.fs.score.gamma ~ ",formula, "+ offset(moca_fs_baseline_gamma) + (1 | id) -1"))),
      data=train_data,
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if(is.null(mod.glmer@optinfo$conv$lme4$messages)){
      print(paste0("Model converged with the, ", optimx_options[i],", optimization!, pid = ",
                   pid," formula = ",formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
    
  }
  
  
  
  pred.glmer <- predict(mod.glmer, newdata=test_data, allow.new.levels=T)
  
  RMSE <- function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  
  rmse <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"moca.fs.score.gamma"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse = rmse,
                    observed = as.matrix(data[data$id==pid,"moca.fs.score.gamma"]) %>% paste(collapse = ", "),
                    predicted = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    patient = pid)
  return(out)
  
}

### testing the models
lapply(30:40, fit_model, formulas[10], moca.fs.long.adj)

cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
  
}

### results adjusted by baseline score (only formulas containing current smoking) --> I am providing the results in the RDS file separately 
models.adj.moca.fs.base.offset.current.smoking <- lapply(formulas[grep("current_smok", formulas)], cross_validate, moca.fs.long.adj)
models.adj.moca.fs.offset.current.smoking <- data.frame(purrr::map_df(models.adj.moca.fs.base.offset.current.smoking, bind_rows))
saveRDS(models.adj.moca.fs.offset.current.smoking,"models-moca-fs-abstraction-adjusted-by-baseline-offset-current-smoking.rds")



### results adjusted by baseline score (the line blocked line below takes several hours to run) --> I am providing the results in the RDS file separately 
# models.adj.moca.fs.base.offset <- lapply(formulas, cross_validate, moca.fs.long.adj)
# models.moca.fs.abstraction.adj.baseline.offset. <- data.frame(purrr::map_df(models.adj.moca.fs.base.offset, bind_rows))
# saveRDS(models.moca.fs.abstraction.adj.baseline.offset.,"models-moca-fs-abstraction-adjusted-by-baseline-offset.rds")


###### checking gamma assumptions 
#### https://stats.stackexchange.com/questions/390063/what-are-the-assumptions-of-a-gamma-glm-or-glmm-for-hypothesis-testing









###### Figure 2 - plotting the models (boxplots) ######

setwd( "/Users/Womps/Documents/U/SkyDrive/Australia Life/PhD year 3/S3 and START/Plots and scripts")
n <- readRDS("models-moca-fs-abstraction-adjusted-by-baseline-offset.rds")
.n <- readRDS("models-moca-fs-abstraction-adjusted-by-baseline-offset-current-smoking.rds")
.nbase <- readRDS("models-MoCA-FS-adjusted-by-baseline-offset-time-only.rds")
o <- readRDS("models-FAB-adjusted-by-baseline-offset.rds")
.o <- readRDS("models-FAB-adjusted-by-baseline-offset-current-smokers.rds")
.obase <- readRDS("models-FAB-adjusted-by-baseline-offset-time-only.rds")
n <- rbind(n[-grep("smoke",n$formula),],
           .n,.nbase)
o <- rbind(o[-grep("smoke",o$formula),],
           .o, .obase)


names(n)
n = cbind(n[-grep("observed|predict", names(n))],
          "observed" = do.call(rbind,strsplit(n$observed, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname,
          "predicted" = do.call(rbind,strsplit(n$predicted, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname)

o = cbind(o[-grep("observed|predict", names(o))],
          "observed" = do.call(rbind,strsplit(o$observed, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname,
          "predicted" = do.call(rbind,strsplit(o$predicted, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname)

### making formulas a categorical (factor) variable

n$formula <- factor(n$formula)
o$formula <- factor(o$formula)


describe(n$rmse)
describe(o$rmse)

## data in long format
op <- data.frame(
  melt(n %>% select(patient, grep("obser", names(.))), id.vars = "patient"),
  melt(n %>% select(patient, grep("predict", names(.))), id.vars = "patient"),
  n %>% select(formula),
  n %>% select(rmse))
levels(op$variable)=c("Predicted MoCA frontal scores at 3-months", "Predicted MoCA frontal scores at 12-months")
op <- rbind(op,
            data.frame(
              melt(o %>% select(patient, grep("obser", names(.))), id.vars = "patient"),
              melt(o %>% select(patient, grep("predict", names(.))), id.vars = "patient"),
              o %>% select(formula),
              o %>% select(rmse))
)

names(op)[grep("val", names(op))]=c("observed","predicted") 

levels(op$variable)[3:4] = c("Predicted Frontal Assessment Battery scores at 3 months", "Predicted Frontal Assessment Battery scores at 12 months") 

op$patient.1 <- c(rep("Australia",nrow(n)*2),
                  rep("Singapore", nrow(o)*2))

head(op)


# Define the number of colors
unique(op$formula)

op$formula <- as.character(op$formula)
unique(op$formula)
op$formula[grep("offset", op$formula)]= " base model (time adjusted by baseline HLCF)"
op$formula <- factor(op$formula)

nb.cols <- nlevels(op$formula)


mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)
# mycolors.edges <- paste0(rep(c("grey","coral","azure","darkgoldenrod","indianred", 
#                                "goldenrod", "lightsalmon","lightsteelblue"),4),seq(1,4))
# 
# mycolors.edges <- mycolors.edges[1:nb.cols]



# #### alternative 2
# mycolors.edges <- paste0(rep(c("grey","brown", 
#                                "goldenrod","lightsteelblue"),4),seq(1,4))
# mycolors.edges <- c(mycolors.edges,mycolors.edges,mycolors.edges)[1:nb.cols]


#### alternative 3 (greyscale)
# mycolors.edges <- rep(c("grey40","grey60","grey75","ivory4","lightblue4","black"
#                         ),8)[1:nb.cols]


#### ordering by median
op$formula.ordered <- paste(substr(op$formula,1,100), 
                      as.character(op$patient.1))

fac <- with(op, reorder(formula.ordered, rmse, median, na.rm = T, order = TRUE))
op$formula.ordered <- factor(op$formula.ordered, levels = levels(fac))

lapply(split(op$rmse,op$formula.ordered), describe)[levels(fac)]



##### calculating correlations to add on to the plot
cors <- plyr::ddply(op, c("formula","patient.1"), 
                    summarise, cor = round(cor(observed, predicted, 
                                               method = "pearson",
                                               use = "complete.obs"), 3))

mycolors[grep(first(mycolors),mycolors)]="black"


#### labels
plotlabs <- unlist(
  sapply(factor(
  paste0(as.character(sort(unique(op$formula))),
         " (",
         lapply(split(cors[,"cor"],cors[,"formula"]),
                paste, collapse = " | "),
         ")")
), 
function(x) gsub(" by HLCF"," by baseline HLCF",
                 gsub("  +"," ",
                      gsub("diab","diabetes",
                           gsub("ihd","isch heart dis",
                                gsub("baseline|p0|binary|total|bin|years","",
                                     gsub("gender","sex",
                                          gsub("ht","hypertension",
                                               gsub("cci","charlson cmb. index",
                                                    gsub("time \\+ |_"," ", 
                                                         as.character(x))))))))))
)
)


####alternative
mycolors.edges <- rep(c("grey50","gray25","indianred3","lightgoldenrod3",
                        "goldenrod", "darkcyan"),8)[1:nb.cols]

##### making the graph
ggplot(op, aes(x = as.numeric(formula.ordered), y = rmse)) +
  geom_boxplot(aes(fill=formula, color=formula), 
               size=.4, width=0.6 , notch = T, outlier.shape = NA) +
  facet_wrap(~ patient.1, scales = "free") + 
  theme_ipsum() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size=8),
    legend.key.size = unit(4,"mm")
  ) + 
  scale_fill_manual(name = "Variable combinations used in models (Pearson's r score START | S3)*",
                    values = mycolors,
                    labels =  plotlabs,
                    guide = guide_legend( 
                      ncol=3, title.hjust = .5,
                      title.position = "top") ) +
  scale_color_manual(name = "Variable combinations used in models (Pearson's r score START | S3)*",
                    values = mycolors.edges,
                    labels =  plotlabs,
                    guide = guide_legend( 
                      ncol=3, title.hjust = .5,
                      title.position = "top") ) +
  scale_y_continuous(limits = c(0,6)) +
  labs( x = "Model used", 
        y = "RMSE value (error from predicted vs. observed scores)", 
        # title = "Gamma-distribution mixed-effects models predicting frontal function performance",
        subtitle = "Models organized by ascending median Root Mean Square Error (RMSE)", 
        caption = "Note: Prediction estimates were obtained with the 'k-fold' cross-validation method\n*All models included time (3 months | 12 months) as a fixed effect; and baseline MoCA-FS | FAB scores as offset" )
  




ggsave("models-rmse-MoCA-Abstraction-FAB-adjusted-gamma-offset-pearson-score-david.pdf", width = 9.5, height = 8.5, device = cairo_pdf )

ggsave("models-rmse-MoCA-Abstraction-FAB-adjusted-gamma-offset-pearson-score-david.png", width = 9.5, height = 8.5, type = "cairo", dpi = 200 )



##### prediction accuracy ####

op <- op %>% mutate(error = sqrt((predicted - observed)^2))

percentage.good<- function(x, tolerance = 2) unlist(
  lapply( 
    lapply(split(x,factor(x$formula)),
           filter,{error} <= tolerance), nrow)) / (nrow(x)/nlevels(factor(x$formula)))

##### how many people predictions are there within 2 points of the observed value??
percentage.good(op %>% filter(patient.1=="Australia"), 2) %>% sort


###### average RMSE per model #####

op %>% group_by(formula.ordered) %>% summarise(mean(rmse, na.rm = T))
tapply(op$rmse, op$formula.ordered, mean, na.rm = T) %>% sort
describeBy(op$rmse, op$patient.1)


##### correlations by model ####
cors[order(cors$cor),]





# ##################Decision trees###############################

#sources
# https://www.joyofdata.de/blog/titanic-challenge-kaggle-svms-kernlab-decision-trees-party/
# https://www.analyticsvidhya.com/blog/2016/04/complete-tutorial-tree-based-modeling-scratch-in-python/
# http://www.exegetic.biz/blog/2013/05/package-party-conditional-inference-trees/
# https://ademos.people.uic.edu/Chapter24.html#4_how_to_avoid_overfitting_the_decision_tree


##### FAB regression trees ####

### creating function to clean variable names
clean_names <- function (x) trimws(
  gsub("score w1","baseline",
       gsub("fab  ","fab baseline",
            gsub("depre","depression",
                 gsub("nihss","nihss baseline",
                      gsub("mmse","mmse baseline",
                           gsub("diab","diabetes",
                                gsub("ihd","isch heart dis",
                                     gsub("baseline|p0|binary|total|bin|years","",
                                          gsub("ht","hypertension",
                                               gsub("cci","charlson index",
                                                    gsub("time \\+ |_"," ", 
                                                         as.character(names(x))))))))))))))

### data for prediction of FAB at 3 months
tree.fab.2 <- all.fab %>% select(.num_vars, .fact_vars, fab_p0_total, 
                                 fab_p3_total 
) %>% as_tibble()
names(tree.fab.2) <- clean_names(tree.fab.2)


### data for prediction of FAB at 12 months
tree.fab.3 <- all.fab %>% select(.num_vars, .fact_vars, fab_p0_total, 
                                 fab_p12_total 
                 
) %>% as_tibble()
names(tree.fab.3) <- clean_names(tree.fab.3)

#creating the tree models
output.tree.2 <- ctree(
  `fab p3` ~ ., 
  data = tree.fab.2,
  controls = ctree_control(
    maxdepth = 3,
    minsplit = 25,
    teststat = "quad",
    testtype = "Teststatistic"
  )
)

### function to modify appeareance of trees
innerWeights <- function(node){
  grid.circle(r=0.5,gp = gpar(fill = "White",col="White"))
  mainlab <- paste(node$psplit$variableName, "\n(n = ")
  mainlab <- paste(mainlab, sum(node$weights),")\n" , sep = "")
  
  grid.text(mainlab,gp = gpar(col='black'))
}


###plotting the trees
cairo_pdf("tree-3-months.pdf", width = 12, height = 8)
plot(output.tree.2, 
     inner_panel=innerWeights
     
)
dev.off()

output.tree.3 <- ctree(
  `fab p12` ~ ., 
  data = tree.fab.3,
  controls = ctree_control(
    maxdepth = 3,
    minsplit = 32,
    teststat = "quad",
    testtype = "Teststatistic"
  )
)
cairo_pdf("tree-12-months.pdf", width = 12, height = 8)
plot(output.tree.3,
     inner_panel=innerWeights

)
dev.off()









###### MoCA FS regression trees #####

### data for prediction of moca at 3 months
tree.moca.2 <- all.moca %>% select(.num_vars, .fact_vars, moca_fs_w1, 
                                   moca_fs_mo3) %>% as_tibble()
names(tree.moca.2) <- gsub("moca fs w1","moca fs baseline",clean_names(tree.moca.2))

### data for prediction of moca at 12 months
tree.moca.3 <- all.moca %>% select(.num_vars, .fact_vars, moca_fs_w1, 
                                   moca_fs_mo12) %>% as_tibble()
names(tree.moca.3) <- gsub("moca fs w1","moca fs baseline",clean_names(tree.moca.3))

####creating the tree models
output.tree.2 <- ctree(
  `moca fs mo3` ~ ., 
  data = tree.moca.2,
  controls = ctree_control(
    maxdepth = 3,
    teststat = "quad",
    testtype = "Teststatistic"
  )
)

### plotting model
cairo_pdf("tree-3-months-moca.pdf", width = 12, height = 8)
plot(output.tree.2,
     inner_panel=innerWeights
     
)

dev.off()

### create model
output.tree.3 <- ctree(
  `moca fs mo12` ~ .,
  data = tree.moca.3, 
  controls = ctree_control(
    maxdepth = 3,
    teststat = "quad",
    testtype = "Teststatistic"
  )
)

### plot model
cairo_pdf("tree-12-months-moca.pdf", width = 12, height = 8)
plot(output.tree.3,
     inner_panel=innerWeights
)

dev.off()











### random effects decision trees####

### FAB longitudinal ####
fab.long=all.fab[,c("id","fab_p3_total","fab_p12_total")]
fab.long=melt(fab.long, id.vars = "id")
names(fab.long)=c("id","time","fab.score")
levels(fab.long$time)=c("3 months","12 months")
fab.long <- data.frame(fab.long,
                       all.fab %>% 
                         select(fab_p0_total, .num_vars, .fact_vars))

fab.long <- fab.long[order(fab.long$id),]

fab.long <- fab.long %>% na.omit() 
fab.long <- as_tibble(fab.long)
fab.long <- subset(fab.long, !is.na(fab.score))
names(fab.long) <- clean_names(fab.long)


tree.glmer <- lmertree(fab.score ~ time | (1 | id  ) |
                         `fab baseline` +
                         `charlson index` +
                         `age onset` + 
                         `nihss baseline` + 
                         hypertension + 
                         `isch heart dis` + 
                         diabetes + 
                         `prev stroke` + 
                         `disab prestroke` + 
                         gender + 
                         depression + 
                         `smoke prestroke` + 
                         ethnicity + 
                         `marital status` - 1, 
                       data = fab.long
)

cairo_pdf("tree-fab-long.pdf", width = 20, height = 12)
plot(tree.glmer, which = "tree", type = "extended"
     
     # gp = gpar(fontsize = 8)
     
)
dev.off()



resids <- residuals(tree.glmer)
preds <- predict(tree.glmer)

plot(factor(fab.long$id), resids)
scatter.smooth(preds, resids)

# sctest(tree.glmer, node = 2)

## good resource
##https://stats.stackexchange.com/questions/330750/glmertree-confidence-intervals-for-regression-coefficients-at-terminal-nodes-an
# tree.glmer$lmer









#####MoCA longitudinal #####
names(all.moca)
moca.long=all.moca[,c("id","moca_fs_mo3","moca_fs_mo12")]
moca.long=melt(moca.long, id.vars = "id")
names(moca.long)=c("id","time","moca.score")
levels(moca.long$time)=c("3 months","12 months")
moca.long <- data.frame(moca.long,
                        all.moca %>% 
                          select(moca_fs_w1,.num_vars, .fact_vars))

moca.long <- moca.long[order(moca.long$id),]

names(moca.long)


moca.long <- moca.long %>% na.omit() 

moca.long <- as_tibble(moca.long)

names(moca.long) <- clean_names(moca.long)

tree.glmer <- lmertree(moca.score ~ time | (1 | id) | 

                         `moca fs w1` +
                         `charlson index` +
                         `age onset` +
                         `nihss baseline` +
                         hypertension +
                         `isch heart dis` +
                         diabetes +
                         `prev stroke` +
                         `disab prestroke` +
                         gender +
                         depression +
                         `smoke prestroke` +
                         ethnicity +
                         `marital status` - 1, 
                       
                       data = moca.long )



cairo_pdf("tree-moca-long.pdf", width = 14, height = 10)
plot(tree.glmer, which = "tree", type = "extended")
# print(output.tree.3)
dev.off()
























