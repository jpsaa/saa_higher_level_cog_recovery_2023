


# write.csv(all, "data-start-for-docR.csv", row.names = F)
rm(list=setdiff(ls(), ls()[grep('pratt|all',ls())])) #clear the workspace (but pratt test results)
# 
# # install.packages(c(
#   # 'psych',
#   # 'ggplot2',
#   # "ggsci",
#   # "ggstatsplot"
#   # 'foreign',
#   # 'nnet',
#   # 'reshape',
#   # 'fastmatch',
#   # 'dplyr',
#   # "openxlsx",
#   # "xtable",
#   # "kableExtra",
#   # "coin",
#   # "asht",
#   # "ggraph",
#   # "igraph",
#   # ))
# 

library(optimx) ## optimizer for models
library(psych)
library(ggplot2) 
library(ggstatsplot) ###statistics on plots
library(reshape2) ### long format data
library(dplyr)
library(coin) ## wilcoxon signed-pratt test
library(asht) ### confidence intervals for wilcoxon signed-pratt test
library(ggraph) ### trees
library(igraph) ### weighted trees
library(tibble) ### tables with "flexible" column characters
library(lme4) ###generalized linear models
library(dataPreparation)
library(lqmm) ## quantile mixed regression
library(extrafont) ##embed fonts in figures
library(hrbrthemes) ## graph colors
library(quantreg) ### quantile regression
library(optimx) ##optimizer for mixed models with lme4



## setting up work directory
setwd(
  "/some folder that makes sense"
)

all <- read.csv("data/data-start-for-docR.csv")
#### Datasets for baseline, complete, incomplete, and no moca ####
all.moca = all %>% dplyr::filter (!is.na(moca_score_w1) & !is.na(moca_score_mo3) & !is.na(moca_score_mo12))
moca.base = all %>% dplyr::filter (!is.na(moca_score_w1))
moca.incomp = moca.base %>% dplyr::filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))
# no.moca = all %>% dplyr::filter (is.na(moca_score_w1) | is.na(moca_score_mo3) | is.na(moca_score_mo12))


#### table 1 (continuous variables)####
### describing variables
.cont.vars = all %>% select(age_t0,nihss_score_w1,madrs_w1,moca_score_w1,height_t0, weight_t0, bmi_t0,
                           systolic_bp_t0, diastolic_bp_t0,
                           aerobic_score_w1, strength_score_w1, CCMI_score_t0) %>% names
all.base=moca.base %>%
  select(.cont.vars) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

comp.num=all.moca %>%
  select(.cont.vars) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

incomp.num=moca.incomp %>%
  select(.cont.vars) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)

table1=cbind("All"=all.base[,1],
             "Median (IQR)"=paste0(all.base[,2]," (",round(all.base[,3],2),")"),
             "Included"=comp.num[,1],
             "Median (IQR)"=paste0(comp.num[,2]," (",round(comp.num[,3],2),")"),
             "Excluded"=incomp.num[,1],
             "Median (IQR)"=paste0(incomp.num[,2]," (",round(incomp.num[,3],2),")"))
table1=cbind(trimws(gsub("t0|_|w1|.x"," ",rownames(comp.num))),table1)

rm(all.base,comp.num,incomp.num)
### calculating differences between groups
table1=cbind(table1,
             "Estimate (95% C.I.)"=NA,
             "p-value"=NA)

for (i in seq_along(.cont.vars)) {
  table1[i,"Estimate (95% C.I.)"]=paste0(
    wilcox.test(moca.base[,which(names(moca.base)==.cont.vars[i])],
                moca.incomp[,which(names(moca.incomp)==.cont.vars[i])],
                conf.int = T)$estimate %>% round (3),
    " (",
    wilcox.test(moca.base[,which(names(moca.base)==.cont.vars[i])],
                moca.incomp[,which(names(moca.incomp)==.cont.vars[i])],
                conf.int = T)$conf.int[1] %>% round(3),
    "-",
    wilcox.test(moca.base[,which(names(moca.base)==.cont.vars[i])],
                moca.incomp[,which(names(moca.incomp)==.cont.vars[i])],
                conf.int = T)$conf.int[2] %>% round(3), 
    ")")

  table1[i,"p-value"]=wilcox.test(moca.base[,which(names(moca.base)==.cont.vars[i])],
                                        moca.incomp[,which(names(moca.incomp)==.cont.vars[i])],
                                        conf.int = T)$p.value %>% round (3)

}

table1[,"p-value"]=ifelse(table1[,"p-value"]==0,"<.001",table1[,"p-value"])
table1



#### table 1 (count variables)####

.freqs = moca.base %>%
  select(gender_t0,ethnicity_binary_w1,educ_binary,marital_status_binary_w1,mrs_prestroke_binary_t0,
         prev_stroke_t0,tia_t0,ht_t0,dm_t0,ihd_t0,af_t0,smoke_ever_w1) %>% names
  
#### table comparing those with and without complete moca scores

all.base=moca.base %>%
  select(.freqs) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
all.base=all.base[-which(trimws(all.base[,1])=="NA's"),]

comp=all.moca %>%
  select(.freqs) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit
comp=comp[-which(trimws(comp[,1])=="NA's"),]

incomp=moca.incomp %>%
  select(.freqs) %>%
  summary() %>% strsplit(":") %>% data.frame() %>% unname() %>% t() %>% na.omit

### matrix with values and percentages
freqs=matrix(c(as.numeric(all.base[,2]),round(as.numeric(all.base[,2])/nrow(moca.base)*100,2),
               as.numeric(comp[,2]),round(as.numeric(comp[,2])/nrow(moca.base)*100,2),
               as.numeric(incomp[,2]),round(as.numeric(incomp[,2])/nrow(moca.base)*100,2)),
             ncol=6,
             dimnames = list(comp[,1],
                             c("All","%","Included","%","Excluded", "%%")))
#### assigning categories
freqs=cbind(vars=
              rep(trimws(gsub("t0|_|w1|.x"," ",.freqs)),each=nlevels(all$educ_binary)),
            freqs)


### Fisher test for the first variable (diabetes)
fisher.test(
  matrix(
    as.numeric(
      unlist(
        split(freqs[,c(which(colnames(freqs)=="Included"),
                       which(colnames(freqs)=="Excluded"))],
              freqs[,"vars"])["dm"]
        )
      ),ncol = 2))$p.value






### confindence interval (lower)
fisher.test(matrix(as.numeric(unlist(split(freqs[,c(which(colnames(freqs)=="Included"),
                                                  which(colnames(freqs)=="Excluded"))],
                                         freqs[,"vars"])[2])),ncol = 2))$conf.int[1]




### applying fisher test to all the variables (in alphabetical order) and getting p-values
est=lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                        which(colnames(freqs)=="Excluded"))],
               freqs[,"vars"]),
         function(x)
           fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$estimate
         ) %>% unlist %>% round(2)


p.values=lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                               which(colnames(freqs)=="Excluded"))],
                      freqs[,"vars"]),
                function(x)
                  fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$p.value
                ) %>% unlist %>% round(2)


c.int.lb=lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                               which(colnames(freqs)=="Excluded"))],
                      freqs[,"vars"]),
                function(x)
                  fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[1]
                ) %>% unlist %>% round(2)

c.int.ub=lapply(split(freqs[,c(which(colnames(freqs)=="Included"),
                               which(colnames(freqs)=="Excluded"))],
                      freqs[,"vars"]),
                function(x)
                  fisher.test(matrix(as.numeric(unlist(x)),ncol = 2))$conf.int[2]
                ) %>% unlist %>%round(2)




### copying stats into the table
freqs=cbind(freqs,
            "Estimate (95% C.I.)"= paste0(
              est[match(freqs[,"vars"],
                        gsub(".odds ratio","",names(est))
                        )
                  ],
              " (",
              c.int.lb[match(freqs[,"vars"],names(c.int.lb))],
              "-",
              c.int.ub[match(freqs[,"vars"],names(c.int.ub))],
              ")"),
            "p-value"=p.values[match(freqs[,"vars"],names(p.values))])

### getting rid of duplicate values and adding names to table
freqs[duplicated(freqs[,"p-value"]),c("p-value","Estimate (95% C.I.)")]=""

vars=as.character(freqs[,"vars"])
freqs[,'vars'] = rownames(freqs)

#### making count and percentage into the same column
freqs = matrix(freqs,
               ncol = ncol(freqs),
               dimnames = list(Variables = vars,
                               "Table 1: Baseline Characteristics"=
                                 c("Categories","All","%","Included","%%", "Excluded","%%%","c.int","p-value")))

freqs[,"All"]=paste0(freqs[,"All"]," (", freqs[,"%%"], "%)")
freqs[,"Included"]=paste0(freqs[,"Included"]," (", freqs[,"%%"], "%)")
freqs[,"Excluded"]=paste0(freqs[,"Excluded"]," (", freqs[,"%%%"], "%)")

### putting numerical values and frequencies together
freqs=freqs[,-grep("%",colnames(freqs))]
freqs=cbind("Variables"=rownames(freqs),freqs)
rownames(freqs)=1:nrow(freqs)

table1=table1[,-grep("Inc|Exc",colnames(table1))]

colnames(table1)[grep("Med",colnames(table1))]=c("Categories","Included", "Excluded")

table1=rbind(freqs,table1)


#### table 1 (final) ####
table1[duplicated(table1[,"Variables"]),'Variables']=""
table1

write.csv(as_tibble(table1),"Table1.csv", row.names = F)
#### remove unnecesary items
rm(all.base,comp,incomp,freqs,c.int.lb,c.int.ub,est,i,p.values,vars)








#### table 2 - moca over time ####
### calculating descriptive statistics for moca domains
moca.w1=all.moca %>% 
  select(grep("moca_score|_total",names(.))) %>%
  select(grep("_w1",names(.))) %>% 
  describe(IQR = T) %>%
  select(n,median,IQR)

moca.mo3=all.moca %>% 
  select(grep("moca_score|_total",names(.))) %>%
  select(grep("_mo3",names(.))) %>% 
  select(-grep("total_",names(.))) %>% 
  describe(IQR = T) %>%
  select(n,median,IQR)

moca.mo12=all.moca %>% 
  select(grep("moca_score|_total",names(.))) %>%
  select(grep("_mo12",names(.))) %>% 
  select(-grep("total_",names(.))) %>%
  describe(IQR = T) %>%
  select(n,median,IQR)


table2=cbind("Baseline Median (IQR)"=paste0(moca.w1[,2]," (",moca.w1[,3],")"),
             "3 Months Median (IQR)"=paste0(moca.mo3[,2]," (",moca.mo3[,3],")"),
             "z-val.1 (95% CI)"=NA,
             "p-value 1"=NA,
             "12 Months Median (IQR)"=paste0(moca.mo12[,2]," (",moca.mo12[,3],")"),
             "z-val.2 (95% CI)"=NA,
             "p-value 2"=NA,
             "z-val.3 (95% CI)"=NA,
             "p-value 3"=NA)

### calculating differences between groups
pratt1=list()
pratt2=list()
pratt3=list()

### loop below takes really really long
for (i in seq_along(rownames(moca.w1))) {
  pratt1[[i]]=wsrTest(all.moca[,which(names(all.moca)==rownames(moca.w1)[i])],
                 all.moca[,which(names(all.moca)==rownames(moca.mo3)[i])])

  pratt2[[i]]=wsrTest(all.moca[,which(names(all.moca)==rownames(moca.mo3)[i])],
                      all.moca[,which(names(all.moca)==rownames(moca.mo12)[i])])

  pratt3[[i]]=wsrTest(all.moca[,which(names(all.moca)==rownames(moca.w1)[i])],
                      all.moca[,which(names(all.moca)==rownames(moca.mo12)[i])])

  table2[i,"p-value 1"]=round(as.numeric(pratt1[[i]]["p.value"]),3)
  table2[i,"z-val.1 (95% CI)"]=paste0(round(unlist(pratt1[[i]]['estimate']),2)," (",
                                      paste0(round(unlist(pratt1[[i]]['conf.int']),2),collapse=", "),")")

  table2[i,"p-value 2"]=round(as.numeric(pratt2[[i]]["p.value"]),3)
  table2[i,"z-val.2 (95% CI)"]=paste0(round(unlist(pratt2[[i]]['estimate']),2)," (",
                                      paste0(round(unlist(pratt2[[i]]['conf.int']),2),collapse=", "),")")

  table2[i,"p-value 3"]=round(as.numeric(pratt3[[i]]["p.value"]),3)
  table2[i,"z-val.3 (95% CI)"]=paste0(round(unlist(pratt3[[i]]['estimate']),2)," (",
                                      paste0(round(unlist(pratt3[[i]]['conf.int']),2),collapse=", "),")")


}


### Fixing p-values that are close to 0

table2[,"p-value 1"]=ifelse(table2[,"p-value 1"]==0,"<.001",table2[,"p-value 1"])
table2[,"p-value 2"]=ifelse(table2[,"p-value 2"]==0,"<.001",table2[,"p-value 2"])
table2[,"p-value 3"]=ifelse(table2[,"p-value 3"]==0,"<.001",table2[,"p-value 3"])



### naming the columns
table2=cbind(`Item (range)`=
                     c("Overall score", "Ex. Fun./Visuosp","Naming",
                       "Attention","Language","Abstraction","Recall","Orientation"),
             table2)


# write_csv(as_tibble(table2),"Table2.csv")
### remove unnecesary items 

rm(moca.w1,moca.mo3,moca.mo12,pratt1,pratt2,pratt3)








### Supplementary Figure - sup fig 1 bubble plot - change in scores ####
all.moca$diff1 = with(all.moca, moca_score_mo3 - moca_score_w1)
all.moca$diff2 = with(all.moca, moca_score_mo12 - moca_score_mo3)
all.moca$moca_impaired_w1 = ifelse(all.moca$moca_score_w1 < 24, "yes", "no")

ggplot(all.moca, aes(x = diff1, y = diff2))+
  geom_count(aes(col=..n..))+
  # scale_size_area(max_size = 7)+
  guides(col="legend")+
  ggtitle("Figure 3 - Changes in MoCA score in the START cohort (n=122)") +
  labs(x = "Change from week 1 to 3 months post stroke\n(points)\n\n Note: Dotted lines represent the median",
       y = "Change from 3 months to 1 year post stroke\n(points)",
       col = "Count", size = "Count") +
  scale_color_distiller(type="div", palette = "Set3") +
  theme_bw()+
  theme(axis.line = element_line(size=0.5, colour = "gray40"),
        panel.grid.major = element_line(colour = "gray60", size = 0.1),
        panel.grid.minor = element_blank(),
        plot.title = element_text(family = "Tahoma", face = "bold",
                                  hjust = .5, size = 12),
        text=element_text(family="Tahoma"),
        axis.text.x=element_text(colour="gray10", size = 8),
        axis.text.y=element_text(colour="gray10", size = 8),
        axis.title.x = element_text(colour="black", size = 8),
        axis.title.y = element_text(colour="black", size = 8)) +
  geom_vline(xintercept=mean(all.moca$diff1),
                 color="red", linetype="dashed",
             alpha=.5) +
  geom_hline(yintercept=median(all.moca$diff2),
                 color="red", linetype="dashed",
             alpha=.5)





#### MoCA change (for figure 2 (trajectory map) and tables 3 and 4) ####

all.moca <- all.moca %>%
  mutate(trends =
           ifelse(
             moca_score_mo12 > moca_score_mo3 &
               moca_score_mo3 > moca_score_w1,
             '1.overall improver',
             ifelse(moca_score_mo3 > moca_score_w1 &
                      moca_score_mo3 == moca_score_mo12,
                    '2.improved-stable',
                    ifelse(moca_score_mo3 > moca_score_w1 &
                             moca_score_mo12 < moca_score_mo3,
                           '3.improved-declined',
                           ifelse(moca_score_mo3 == moca_score_w1 &
                                    moca_score_mo12 > moca_score_mo3 ,
                                  '4.stable-improved',
                                  ifelse(moca_score_mo12 == moca_score_mo3 &
                                           moca_score_mo3 == moca_score_w1,
                                         '5.overall stable',
                                         ifelse(moca_score_mo3 == moca_score_w1 &
                                                  moca_score_mo12 < moca_score_mo3,
                                                '6.stable-declined',
                                                ifelse(moca_score_mo3 < moca_score_w1 &
                                                         moca_score_mo12 > moca_score_mo3,
                                                       '7.declined-improved',
                                                       ifelse(moca_score_mo3 < moca_score_w1 &
                                                                moca_score_mo3 == moca_score_mo12,
                                                              '8.declined-stable',
                                                              ifelse(moca_score_mo12 < moca_score_mo3 &
                                                                       moca_score_mo3 < moca_score_w1,
                                                                     '9.overall decliner',NA))))))))))

###Table 3####

moca<-all.moca %>% select(id,moca_score_w1,moca_score_mo3,moca_score_mo12,trends)

.t3_cols=all.moca %>% select(trends,age_t0,CCMI_score_t0,
                     systolic_bp_t0,diastolic_bp_t0,
                     systolic_bp_mo3,diastolic_bp_mo3,
                     systolic_bp_mo12, diastolic_bp_mo12,
                     moca_score_w1,moca_score_mo3,moca_score_mo12,
                     mmse_score_mo3,mmse_score_mo12,
                     stroop_color_word_ratio_mo3,stroop_ratio_mo12,
                     ravens_score_mo3, ravens_total_score_mo12,
                     time_taken_mo3,tmt_time_taken_mo12,
                     ldsf_score_mo3,ldsf_score_mo12,
                     ldsb_score_mo3,ldsb_score_mo12,
                     madrs_score_w1,madrs_score_mo3,madrs_score_mo12,
                     nihss_score_w1,nihss_score_mo3, nihss_score_mo12,
                     barthel_score_mo3, barthel_score_mo12,
                     mrs_score_mo3,mrs_score_mo12,
                     aerobic_score_w1,
                     strength_score_w1,rapa_aerobic_score_mo3,
                     rapa_strengthflexibility_score_mo3,
                     rapa_aerobic_score_mo12,
                     rapa_strengthflexibility_score_mo12,
                     acs_1_82_raln_mo3,
                     acs_1_82_raln_mo12,
                     wsas_score_mo3,
                     wsas_score_mo12,
                     sis_total_mo3,
                     sis_total_mo12) %>% names


t3 = all.moca %>%  select(.t3_cols) %>% group_by(trends) %>%
  summarise_if(is.numeric, funs(sum(!is.na(.)),median,IQR),na.rm = TRUE)


###getting descriptive stats for all variables
counts=names(t3)[grep("sum",names(t3))]
medians=names(t3)[grep("median",names(t3))]
iqrs=names(t3)[grep("IQR",names(t3))]
table3=matrix(nrow=nrow(t3))

for (i in seq_along(medians)) {
  #### looking for NA values and turning them into spaces
  .counts=which(names(t3)==counts[i])
  .med=which(names(t3)==medians[i])
  .iqr=which(names(t3)==iqrs[i])
    table3=cbind(table3,
                 cbind(
                   paste0(
                     round(as.numeric(unlist(t3[,.counts])),2),", ",
                     round(as.numeric(unlist(t3[,.med])),2),
                          " (",
                        paste0(round(as.numeric(unlist(t3[,.iqr])),2),
                               ")"))
                 )
               )
    table3[grep("NA",table3[,i+1]),i+1]=""

}


table3=table3[,-1]
table3=t(table3)

##### binding count variable stats
get_stats <- function(x, var , categ=NULL){
   if (!is.null(categ)){
     ### mapping zeroes
     miss = cbind(x %>% select(trends), (x %>% select({{var}}))=={{categ}}) %>% table %>%
       data.frame() %>% filter({{var}}==T, Freq==0) %>% select(trends,Freq)
     names(miss) <- c("trends","categ")
     
     #### getting values
     .nums = x %>% filter({{var}}=={{categ}}) %>%
       group_by(trends) %>% 
       summarize(categ =n()) %>% 
       rbind(miss) %>%
       arrange(trends) %>% select(categ) %>% unlist

     paste0(.nums,
            ' (', 
            round(.nums/nrow(all.moca)*100, 2),
            "%)")
     
   } else {
     
     paste0(
       x %>% group_by(trends) %>% select(trends) %>%
         summarize(var=n()) %>% select(var) %>% unlist(),
       " (",
       round(
         ((x %>% group_by(trends) %>% select(trends) %>%
             summarize(var=n()) %>% select(var) %>% unlist())/nrow(all.moca))*100,2),
       "%)")
      
  }
     
  }

table3=rbind(
  get_stats(all.moca,id),
  get_stats(all.moca, gender_w1, "male"),
  get_stats(all.moca, educ_binary, "secondary or more"),
  get_stats(all.moca, marital_status_binary_w1, "married"),
  get_stats(all.moca, disab_prestroke, "some disab"),
  table3)
colnames(table3)=unlist(t3[,1])
####rownames
table3=cbind(Variable=c(
  "n (%)","Males (%)","Education","Marital status","mrs prestroke",
  trimws(gsub("median|t0|.y|_"," ",medians))
  ),table3)
### creating space between assessments
cols=c("stolic bp",'moca','mmse','stroop',"ravens",'time taken',
       'lds','madrs','nihss','barthel','mrs score','aerobic','acs','wsas','sis')
.t3=matrix(ncol=ncol(table3))
grab=1
for (i in seq_along(cols)) {
  space=first(grep(cols[i],table3[,1]))-1
  .t3=rbind(.t3,table3[grab:space,],"")
  grab=first(grep(cols[i],table3[,1]))

}
#### adding the last category
.t3=rbind(.t3[-1,],table3[grep("sis",table3[,1]),])
table3=.t3

write.csv(as_tibble(table3), "Table3.csv",row.names =  F)
rm(t3,cols,counts,grab,i,iqrs,medians,space)






### how many sites? ####
length(table(all.moca$site_id_w1))









#### Table 4 - overall improvers and improved declined #####

sga=all.moca %>%  filter(trends=="1.overall improver" | trends=="3.improved-declined") %>%
  select(trends,gender_w1, educ_binary, marital_status_binary_w1, disab_prestroke,.t3_cols)

sga.desc=sga %>% group_by(trends) %>%
  summarise_if(is.numeric, funs(sum(!is.na(.)),median,IQR),na.rm = TRUE)

counts=names(sga.desc)[grep("_sum",names(sga.desc))]
medians=names(sga.desc)[grep("median",names(sga.desc))]
iqrs=names(sga.desc)[grep("IQR",names(sga.desc))]
table4=matrix(nrow=nrow(sga.desc))
for (i in seq_along(medians)) {
  #### looking for NA values and turning them into spaces
  .counts=which(names(sga.desc)==counts[i])
  .med=which(names(sga.desc)==medians[i])
  .iqr=which(names(sga.desc)==iqrs[i])
  table4=cbind(table4,
               cbind(
                 paste0(
                   round(as.numeric(unlist(sga.desc[,.counts])),2),", ",
                   round(as.numeric(unlist(sga.desc[,.med])),2),
                   " (",
                   paste0(round(as.numeric(unlist(sga.desc[,.iqr])),2),
                          ")"))
               )
  )
  table4[grep("NA",table4[,i+1]),i+1]=""

}


table4=table4[,-1]
table4=t(table4)
colnames(table4)=levels(factor(sga$trends))


#### applying wilcoxon test accross all columns and groupping by trend (overall improver and improver declined)
cols=sga %>% select(-grep("gender|trends|educ_|marital_|disab_pres",names(.))) %>% names

sigs <- list()
c1=c("Variable","z-value 1","95% CI 1","p-value 1")
.tests=matrix(ncol = length(c1),
             nrow = length(cols))
colnames(.tests)=c1
for(i in seq_along(cols)) {
    sigs[[i]] <- wilcox_test(
      sga[,which(names(sga)==cols[i])] ~ factor(trends),
      data=sga,
      distribution = "exact", zero.method="Pratt", conf.int=T
    )
    .tests[i,"Variable"]=cols[i]
    .tests[i,"z-value 1"]=round(statistic(sigs[[i]]),3)
    .tests[i,"95% CI 1"]=paste0(round(confint(sigs[[i]])$conf.int[1],3),", ",
                             round(confint(sigs[[i]])$conf.int[2],3))
    .tests[i,"p-value 1"]=round(pvalue(sigs[[i]]),3)

  }

  .tests=cbind(.tests[,"Variable"],table4,.tests[,-which(colnames(.tests)=="Variable")])
  table4=.tests
  table4

  #### count data values
  
  
  fisher.stats <- function(x, var){
    calc <- x %>% select(trends,{{var}}) %>% group_by(trends) %>%
      table() %>% t() %>% fisher.test()
    est <- unname(round(calc$estimate,3))
    lb <- round(calc$conf.int[1],3)
    ub <- round(calc$conf.int[2],3)
    p <- round(calc$p.value,3)
    tibble(`z-value`=est, 
               `95% CI`= paste0(lb,", ", ub),
           `p-value`=p)
  }
  

  #### adding count variables
  table4=rbind(
    c("n",get_stats(sga,gender_w1),
      "","",""),
    t(c("males",get_stats(sga,gender_w1, "male"),
      fisher.stats(sga, gender_w1) %>% unname)),
    t(c("education",get_stats(sga,educ_binary,"secondary or more"),
          fisher.stats(sga, educ_binary))),
    t(c("marital status",get_stats(sga,marital_status_binary_w1,"married"),
          fisher.stats(sga,marital_status_binary_w1) %>% unname)),
    t(c("prestroke disab",get_stats(sga,disab_prestroke,"some disab"),
          fisher.stats(sga, disab_prestroke) %>%unname)),
    table4)

  
  table4[,1]<- trimws(gsub("median|t0|.y|_"," ",table4[,1]))



  
  
  
  
  
  
  
  ##### comparison overall improver vs overall decliner
  sga2=all.moca %>%  filter(trends=="1.overall improver" | trends=="9.overall decliner") %>%
    select(trends,gender_w1, educ_binary, marital_status_binary_w1, disab_prestroke,.t3_cols)
  
  sga.desc=sga2 %>% group_by(trends) %>%
    summarise_if(is.numeric, funs(sum(!is.na(.)),median,IQR),na.rm = TRUE)

  counts=names(sga.desc)[grep("_sum",names(sga.desc))]
  medians=names(sga.desc)[grep("median",names(sga.desc))]
  iqrs=names(sga.desc)[grep("IQR",names(sga.desc))]
  .table4=matrix(nrow=nrow(sga.desc))
  for (i in seq_along(medians)) {
    #### looking for NA values and turning them into spaces
    .counts=which(names(sga.desc)==counts[i])
    .med=which(names(sga.desc)==medians[i])
    .iqr=which(names(sga.desc)==iqrs[i])
    .table4=cbind(.table4,
                  cbind(
                    paste0(
                      round(as.numeric(unlist(sga.desc[,.counts])),2),", ",
                      round(as.numeric(unlist(sga.desc[,.med])),2),
                      " (",
                      paste0(round(as.numeric(unlist(sga.desc[,.iqr])),2),
                             ")"))
                  )
    )
    .table4[grep("NA",.table4[,i+1]),i+1]=""

  }


  .table4=.table4[,-1]
  .table4=t(.table4)
  colnames(.table4)=levels(factor(sga2$trends))


  #### applying wilcoxon test accross all columns and groupping by trend (overall improver and improver declined)
  sigs <- list()
  c1=c("Variable","z-value 2","95% CI 2","p-value 2")
  .tests=matrix(ncol = length(c1),
               nrow = length(cols))
  colnames(.tests)=c1
  for(i in seq_along(cols)) {
    sigs[[i]] <- wilcox_test(
      sga2[,which(names(sga2)==cols[i])] ~ factor(trends),
      data=sga2,
      distribution = "exact", zero.method="Pratt", conf.int=T
    )
    .tests[i,"Variable"]=cols[i]
    .tests[i,"z-value 2"]=round(statistic(sigs[[i]]),3)
    .tests[i,"95% CI 2"]=paste0(round(confint(sigs[[i]])$conf.int[1],3),", ",
                             round(confint(sigs[[i]])$conf.int[2],3))
    .tests[i,"p-value 2"]=round(pvalue(sigs[[i]]),3)

  }

  
  .tests=cbind(.tests[,"Variable"],.table4,.tests[,-which(colnames(.tests)=="Variable")])

  #### adding count variables
  .tests=rbind(
    c("n",get_stats(sga2,gender_w1),
      "","",""),
    t(c("males",get_stats(sga2,gender_w1, "male"),
        fisher.stats(sga2, gender_w1) %>% unname)),
    t(c("education",get_stats(sga2,educ_binary,"secondary or more"),
        fisher.stats(sga2, educ_binary))),
    t(c("marital status",get_stats(sga2,marital_status_binary_w1,"married"),
        fisher.stats(sga2,marital_status_binary_w1) %>% unname)),
    t(c("prestroke disab",get_stats(sga2,disab_prestroke,"some disab"),
        fisher.stats(sga2, disab_prestroke) %>%unname)),
    .tests)
  
table4=cbind(table4,.tests[,3:6])
colnames(table4)[which(colnames(table4)=="")]=c("variables","1.overall improver", "3.improved-declined", "9.overall decliner")

table4 <- apply(table4,2,as.character)

write.csv(as_tibble(table4, .name_repair = "minimal"), "table4.csv", row.names =  F)

#### remove unnecesary elements
rm(sga,sga.desc,sga2,sigs,c1,cols, counts, i, iqrs, medians)




##### fig 2 ####
#### Figure 2 - moca tree####
moca.scores=moca[,-which(names(moca)=="trends")]

moca.scores<-melt(moca.scores,id.vars = 'id')

names(moca.scores)=c("id", 'time', 'score')
moca=cbind(moca.scores,moca$trends)
names(moca)[4]="trends"


#removing letters from columns
moca$time=as.numeric(as.character(factor(moca$time,
                                         labels = c(7,30,365))))
moca=na.omit(moca)


###simple tree with groups and no values####
# testing approach with data sequences

moca12=data.frame(table(moca$trends))
sum(moca12$Freq)

moca3<- all.moca %>%
  mutate (moca3 = ifelse(moca_score_mo3 < moca_score_w1,
                         '3.decliner',
                         ifelse(moca_score_mo3 > moca_score_w1,
                                '1.improver',
                                ifelse(moca_score_mo3 == moca_score_w1,
                                       '2.stable',NA)))) %>% select(moca3) %>%  table() %>% data.frame()


#### Asssigning actual values ####
d1=data.frame(from="onset", to=paste0(c('Improved','Stable','Declined'),' at 3mo'))
d2=data.frame(from=rep(d1$to, each=3), to=paste0(moca12$Var1,' (',moca12$Freq/3,')'))
edges=rbind(d1, d2)
# adding a second data frame with information for each node!
name=unique(c(as.character(edges$from), as.character(edges$to)))
vertices=data.frame(
  name=name,
  group=c(NA, rep(c("Improved","Stable","Declined"),4)),
  Three.Months=c(NA, rep(c("Improved","Stable","Declined"),4)),
  value=as.numeric(c(158,moca3$Freq,moca12$Freq/3))
)

####Figure 2 trajectory####
mygraph <- graph_from_data_frame(edges, vertices=vertices)

ggraph(mygraph, layout = 'dendrogram') +
  # geom_edge_diagonal() +
  geom_edge_diagonal2(aes(colour=node.group, width=node.value, alpha=0.8))+
  geom_node_text(aes( label=name, filter=leaf, color=Three.Months) , 
                 angle=0 , hjust=0, nudge_y=0.03, size = 3) +
  geom_node_point(aes(filter=leaf, size=value, color=Three.Months) , 
                  alpha=0.5) +
  
  # ggtitle(paste0('        Figure 2 - Changes in MoCA scores from week 1 to 3 and 12 months post-stroke\n',
  #                '        n = ',nrow(moca)/3))+
  
  theme_ipsum(axis_text_size = 9) +
  theme(legend.position="bottom",
        legend.text = element_text(size = 8),
        plot.title = element_text(family = "Tahoma",
                                  face = "bold",hjust = 0.5, size = 12),
        text=element_text(family="Tahoma", angle = 0, vjust = 0),
        legend.title = element_text(colour="gray10",size=8),
        axis.line = element_line(size = 1, colour = "grey100"),
        # axis.ticks.x = element_line(size = 1)
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
  
  scale_size_continuous(name="Number of participants at 12 months")+
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
         # color=guide_legend(ncol=1, nrow = 3, title.position = "top"),
         size=guide_legend(ncol=4, nrow = 1, title.position = "top")) 


ggsave("Fig-3-tree-cairo.pdf",width = 12, height = 7,units = 'in',device = cairo_pdf)
rm(d1,d2,edges,moca,moca.scores,moca12,moca3,mygraph,vertices,name)


##### end fig 2 


##### Mcnemars test for count data ####
all.moca$imp.base=all.moca %>% mutate(imp.base=ifelse(moca_score_w1 < 24, "yes","no")) %>% select(imp.base)
all.moca$imp.mo3=all.moca %>% mutate(imp.mo3=ifelse(moca_score_mo3 < 24, "yes","no")) %>% select(imp.mo3)
all.moca$imp.mo12=all.moca %>% mutate(imp.mo12=ifelse(moca_score_mo12 < 24, "yes","no")) %>% select(imp.mo12)


####McNemars tests #####
####change 1
matrix(
  c(
    all.moca %>% filter(imp.base=="yes",imp.mo3=="yes") %>% nrow(),
    all.moca %>% filter(imp.base=="no",imp.mo3=="yes") %>% nrow(),
    all.moca %>% filter(imp.base=="yes",imp.mo3=="no") %>% nrow(),
    all.moca %>% filter(imp.base=="no",imp.mo3=="no") %>% nrow()),
  dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                   `month 3` = c("Cognitive impairment", "No cognitive impairment")),
  nrow=2
) %>% mcnemar.test()

##### change 2
matrix(
  c(
    all.moca %>% filter(imp.mo3=="yes",imp.mo12=="yes") %>% nrow(),
    all.moca %>% filter(imp.mo3=="no",imp.mo12=="yes") %>% nrow(),
    all.moca %>% filter(imp.mo3=="yes",imp.mo12=="no") %>% nrow(),
    all.moca %>% filter(imp.mo3=="no",imp.mo12=="no") %>% nrow()),
  dimnames = list (`month 3` = c("Cognitive impairment", "No cognitive impairment"),
                   `month 12` = c("Cognitive impairment", "No cognitive impairment")),
  nrow=2
) %>% mcnemar.test()

#### overall change
matrix(
  c(
    all.moca %>% filter(imp.base=="yes",imp.mo12=="yes") %>% nrow(),
    all.moca %>% filter(imp.base=="no",imp.mo12=="yes") %>% nrow(),
    all.moca %>% filter(imp.base=="yes",imp.mo12=="no") %>% nrow(),
    all.moca %>% filter(imp.base=="no",imp.mo12=="no") %>% nrow()),
  dimnames = list (`week 1` = c("Cognitive impairment", "No cognitive impairment"),
                   `month 12` = c("Cognitive impairment", "No cognitive impairment")),
  nrow=2
)  %>% mcnemar.test()


#### categorize data (impaired/not impaired) --> 1 = yes, 0 = no

all.moca = all.moca %>% mutate(impaired_w1 = ifelse(moca_score_w1 < 24, 1, 0))
all.moca = all.moca %>% mutate(impaired_mo3 = ifelse(moca_score_mo3 < 24, 1, 0))
all.moca = all.moca %>% mutate(impaired_mo12 = ifelse(moca_score_mo12 < 24, 1, 0))



###selecting all variables for analyses
.all.vars=
  all.moca %>%select(
    gender_w1, educ_binary, prev_stroke_t0,
    tia_t0, ht_t0, af_t0, dm_t0, ihd_t0,
    disab_prestroke, smoke_ever_w1,
    ethnicity_binary_w1, marital_status_binary_w1,
    age_t0, nihss_score_w1, CCMI_score_t0,
    madrs_w1, bmi_t0, aerobic_score_w1,
    strength_score_w1) %>% names

all.moca %>% select(.all.vars) %>% summary


# ##### data for regression analyses ####

#### binary logistic regressions
blr1 = all.moca %>% select(impaired_w1,
                                  .all.vars) %>%na.omit()
blr2 = all.moca %>% select(impaired_mo3,
                                  .all.vars, moca_score_w1) %>% na.omit()
blr3 = all.moca %>% select(impaired_mo12,
                                  .all.vars, moca_score_w1) %>% na.omit()

#### quantile regression
qr1 = all.moca %>% select(moca_score_w1,
                                 .all.vars) %>% na.omit()
qr2 = all.moca %>% select(moca_score_mo3,
                                 .all.vars, moca_score_w1) %>% na.omit()
qr3 = all.moca %>% select(moca_score_mo12,
                                 .all.vars, moca_score_w1) %>% na.omit()

#### Fitting univariable models ####

#### Baseline BLR ####
.ORs=lapply(names(blr1)[-1],
            function(var) {
              formula    <- as.formula(paste("impaired_w1 ~", var))
              res.logist <- glm(formula, data = blr1, family = binomial)
              # summary(res.logist, digits = 3)
              # confint(res.logist)
              ORs=matrix(c(
                exp(summary(res.logist)$coefficients[-1,1] +
                      qnorm(c(0.5,0.025,0.975)) * summary(res.logist)$coefficients[-1,2]),
                summary(res.logist)$coefficients[-1,4]),ncol=4,
                dimnames = list(c(var),
                                c("OR","lb","ub","p-value"))) %>%
                round(3)
              ORs=cbind(var,
                        "odds.ratio"=paste0(ORs[,'OR']," (",ORs[,'lb'],
                                            " to ",ORs[,'ub'], ")"),
                        "p.value"=ORs[,"p-value"])
              
              ORs = data.frame(ORs)
              
              
            })

.ORS.w1 = data.frame(purrr::map_df(.ORs, bind_rows))


#### 3-month BLR ####
.ORs=lapply(names(blr2)[-1],
            function(var) {
              formula    <- as.formula(paste("impaired_mo3 ~", var))
              res.logist <- glm(formula, data = blr2, family = binomial)
              # summary(res.logist, digits = 3)
              # confint(res.logist)
              ORs=matrix(c(
                exp(summary(res.logist)$coefficients[-1,1] +
                      qnorm(c(0.5,0.025,0.975)) * summary(res.logist)$coefficients[-1,2]),
                summary(res.logist)$coefficients[-1,4]),ncol=4,
                dimnames = list(c(var),
                                c("OR","lb","ub","p-value"))) %>%
                round(3)
              ORs=cbind(var,
                        "odds.ratio"=paste0(ORs[,'OR']," (",ORs[,'lb'],
                                            " to ",ORs[,'ub'], ")"),
                        "p.value"=ORs[,"p-value"])
              
              ORs = data.frame(ORs)
              
            })

.ORS.mo3 = data.frame(purrr::map_df(.ORs, bind_rows))

#### 12-month BLR ####
.ORs=lapply(names(blr3)[-1],
            function(var) {
              formula    <- as.formula(paste("impaired_mo12 ~", var))
              res.logist <- glm(formula, data = blr3, family = binomial)
              # summary(res.logist, digits = 3)
              # confint(res.logist)
              ORs=matrix(c(
                exp(summary(res.logist)$coefficients[-1,1] +
                      qnorm(c(0.5,0.025,0.975)) * summary(res.logist)$coefficients[-1,2]),
                summary(res.logist)$coefficients[-1,4]),ncol=4,
                dimnames = list(c(var),
                                c("OR","lb","ub","p-value"))) %>%
                round(3)
              ORs=cbind(var,
                        "odds.ratio"=paste0(ORs[,'OR']," (",ORs[,'lb'],
                                            " to ",ORs[,'ub'], ")"),
                        "p.value"=ORs[,"p-value"])
              
              ORs = data.frame(ORs)
              
            })

.ORS.mo12 = data.frame(purrr::map_df(.ORs, bind_rows))

write.csv(rbind(.ORS.w1,"",.ORS.mo3,"",.ORS.mo12) %>% 
            filter(p.value < .05) %>% mutate(p.value = ifelse(p.value!="" & 
                                                                p.value < 0.001, "<.001", p.value)),
          "odds-ratio.csv", row.names = F)

#### Baseline Quant Reg ####
.stats=lapply(names(qr1)[-1],
              function(var) {
                formula    <- as.formula(paste("moca_score_w1 ~", var))
                res.qr <- summary(rq(formula, data = qr1, method = "fn"), se="boot")
                stats=matrix(c(
                  res.qr$coefficients[-1,1] +
                    qnorm(c(0.5,0.025,0.975)) * res.qr$coefficients[-1,2],
                  res.qr$coefficients[-1,4]),ncol=4,
                  dimnames = list(c(var),
                                  c("beta","lb","ub","p-value"))) %>%
                  round(3)
                stats=
                  data.frame(cbind("var"=var,
                                   "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                 stats[,'ub'], ")"),
                                   "p.value"=stats[,"p-value"]))
                
                
              })

.stats.w1 = data.frame(purrr::map_df(.stats, bind_rows))


#### 3-month Quant Reg ####
.stats=lapply(names(qr2)[-1],
              function(var) {
                formula    <- as.formula(paste("moca_score_mo3 ~", var))
                res.qr <- summary(rq(formula, data = qr2, method = "fn"), se="boot")
                stats=matrix(c(
                  res.qr$coefficients[-1,1] +
                    qnorm(c(0.5,0.025,0.975)) * res.qr$coefficients[-1,2],
                  res.qr$coefficients[-1,4]),ncol=4,
                  dimnames = list(c(var),
                                  c("beta","lb","ub","p-value"))) %>%
                  round(3)
                stats=
                  data.frame(cbind("var"=var,
                                   "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                 stats[,'ub'], ")"),
                                   "p.value"=stats[,"p-value"]))
              })

.stats.mo3 = data.frame(purrr::map_df(.stats, bind_rows))


#### 12-month Quant Reg ####
.stats=lapply(names(qr3)[-1],
       function(var) {
         formula    <- as.formula(paste("moca_score_mo12 ~", var))
         res.qr <- summary(rq(formula, data = qr3, method = "fn"), se="boot")
         stats=matrix(c(
           res.qr$coefficients[-1,1] +
             qnorm(c(0.5,0.025,0.975)) * res.qr$coefficients[-1,2],
           res.qr$coefficients[-1,4]),ncol=4,
           dimnames = list(c(var),
                           c("beta","lb","ub","p-value"))) %>%
           round(3)
         stats=
           data.frame(cbind("var"=var,
                            "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                          stats[,'ub'], ")"),
                            "p.value"=stats[,"p-value"]))
       })

.stats.mo12 = data.frame(purrr::map_df(.stats, bind_rows))

write.csv(
  rbind(.stats.w1,"",.stats.mo3,"",.stats.mo12) %>% 
    filter(p.value < .05) %>% mutate(p.value = ifelse(p.value!="" & 
                                                        p.value < 0.001, "<.001", p.value)),
  "quantile-regression.csv", row.names = F)

rm(blr1,blr2,blr3,qr1,qr2,qr3)



### mixed linear model predicting scores####

moca.long=all.moca[,c("id","moca_score_w1","moca_score_mo3","moca_score_mo12")]
moca.long=melt(moca.long, id.vars = "id")
names(moca.long)=c("id","time","moca.score")
levels(moca.long$time)=c("3-7 days","3 months","12 months")
moca.long <- data.frame(moca.long,
                        all.moca %>%
                          select(.all.vars))


moca.long <- moca.long[order(moca.long$id),]


# #### testing first quantile mixed model
# 
# #### plotting model
# ggplot(moca.long, aes(x = age_t0, y = moca.score)) +
#   geom_point() + geom_smooth(method = "lm")
# 
# boxplot(moca.score ~ time, data = moca.long)
# 
# ggplot(moca.long, aes(x = age_t0, y = moca.score)) + 
#   geom_point(size = 2) +
#   facet_wrap(~ time) +
#   theme_classic() +
#   theme(legend.position = "none")
# 
# 
# 
# 

#### lqmm all variables separately, no adjustment #####

mixed.vars=names(moca.long)[-grep("id|moca.|time",names(moca.long))]
.stats=lapply(mixed.vars,
             function(var) {
               formula    <- as.formula(paste("moca.score ~", var))
               mixed <- lqmm(fixed = eval(formula), random = ~ 1, group = id,
                             data = moca.long, tau = 0.5, nK = 11, type = "normal",
                             na.action = na.omit
                             # control = lqmmControl(LP_max_iter = 1000,
                             #                       LP_tol_ll = 1e-04)
               )
               mixed$call$fixed <- formula
               stats=summary(mixed)$tTable
               
               stats=matrix(c(
                 stats[-1,"Value"],stats[-1,"lower bound"],stats[-1,"upper bound"],
                 stats[-1,"Pr(>|t|)"]),ncol=4,
                 dimnames = list(c(var),
                                 c("beta","lb","ub","p-value"))) %>%
                 round(3)
               stats=
                 data.frame(cbind("var"=var,
                                  "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                stats[,'ub'], ")"),
                                  "p.value"=stats[,"p-value"]))
             })

.stats = data.frame(purrr::map_df(.stats, bind_rows))

write.csv(
  .stats %>% 
    filter(p.value < .05) %>% mutate(p.value = ifelse(p.value!="" & 
                                                        p.value < 0.001, "<.001", p.value)),
  "lqmm.csv", row.names = F)



###### lqmm adjusted by moca baseline ####
moca.long.adj=all.moca[,c("id","moca_score_mo3","moca_score_mo12")]
moca.long.adj=melt(moca.long.adj, id.vars = "id")
names(moca.long.adj)=c("id","time","moca.score")
levels(moca.long.adj$time)=c("90 days","365 days")
moca.long.adj <- data.frame(moca.long.adj,
                            all.moca %>%
                              select(.all.vars, moca_score_w1))

names(moca.long.adj)[grep("moca_score_w1", names(moca.long.adj))]="moca_baseline"

moca.long.adj <- moca.long.adj[order(moca.long.adj$id),]

mixed.vars=names(moca.long.adj)[-grep("id|moca.|time",names(moca.long.adj))]
.stats=lapply(mixed.vars,
             function(var) {
               formula    <- as.formula(paste("moca.score ~", var, "+ moca_baseline"))
               mixed <- lqmm(fixed = eval(formula), random = ~ 1, group = id,
                             data = moca.long.adj, tau = 0.5, nK = 11, type = "normal",
                             na.action = na.omit
                             # control = lqmmControl(LP_max_iter = 1000,
                             #                       LP_tol_ll = 1e-04)
               )
               mixed$call$fixed <- formula
               stats=summary(mixed)$tTable
               pos = grep(var, rownames(stats))
               
               stats=matrix(c(
                 stats[pos,"Value"],stats[pos,"lower bound"],stats[pos,"upper bound"],
                 stats[pos,"Pr(>|t|)"]),ncol=4,
                 dimnames = list(c(rownames(stats)[pos]),
                                 c("beta","lb","ub","p-value"))) %>%
                 round(3)
               stats=
                 data.frame(cbind("var"=rownames(stats),
                                  "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                stats[,'ub'], ")"),
                                  "p.value"=stats[,"p-value"]))
             })

.stats = data.frame(purrr::map_df(.stats, bind_rows))

write.csv(
  .stats %>% 
    filter(p.value < .05) %>% mutate(p.value = ifelse(p.value!="" & 
                                                        p.value < 0.001, "<.001", p.value)),
  "lqmm-adjusted-moca-baseline.csv", row.names = F)



# #### selecting only significant variables from the univariable models
moca.long.model=moca.long %>% select(id, moca.score, time, 
                                     educ_binary, 
                                     ethnicity_binary_w1, 
                                     smoke_ever_w1,
                                     tia_t0,
                                     ht_t0, 
                                     ihd_t0, 
                                     disab_prestroke, 
                                     age_t0, 
                                     strength_score_w1, 
                                     nihss_score_w1)


### final mixed model ####
.mixed <- lqmm(fixed = as.formula(paste("moca.score ~",
                                       paste(names(moca.long.model)[-(1:3)], collapse= "+"),
                                       -1)),
              random = ~ 1, group = id,
              data = moca.long.model, tau = 0.5, nK = 11, type = "normal",
              na.action = na.omit)

lqmm.mixed.adjusted.summary=summary(.mixed)$tTable

####adjusting p-values
.p.adjusted=p.adjust(lqmm.mixed.adjusted.summary[,5], method = "bonferroni" )

##### simplified summary table
lqmm.mixed.adjusted.summary=
  matrix(c(
  lqmm.mixed.adjusted.summary[,"Value"],lqmm.mixed.adjusted.summary[,"lower bound"],lqmm.mixed.adjusted.summary[,"upper bound"],
  lqmm.mixed.adjusted.summary[,"Pr(>|t|)"]),ncol=4,
  dimnames = list(c(rownames(lqmm.mixed.adjusted.summary)),
                  c("beta","lb","ub","p-value"))) %>%
  round(3)

#### simplify further
lqmm.mixed.adjusted.summary=
  cbind(
    "estimate (95% CI)"=paste0(lqmm.mixed.adjusted.summary[,'beta'],
                               " (",
                               lqmm.mixed.adjusted.summary[,'lb'],
                               " to ",
                               lqmm.mixed.adjusted.summary[,'ub'],
                               ")"),
    "p-val"=lqmm.mixed.adjusted.summary[,"p-value"],
    "p-val adjusted"=round(.p.adjusted,3))
lqmm.mixed.adjusted.summary
####clear the workspace
# rm(list=setdiff(ls(), ls()[grep('pratt|all.moca|moca.long',ls())])) 
# 
# 
#### STEPS FOR MODEL SELECTION ####

##### Creating Gamma distributions ####
# plot(density(moca.long.adj$moca.score))
moca.long.adj$moca.score.gamma <- (moca.long.adj$moca.score - 42)*-1
moca.long.adj$moca_baseline_gamma <- (moca.long.adj$moca_baseline - 42)*-1

# plot(density(moca.long.adj$moca.score.gamma))
# plot(density(moca.long.adj$moca_baseline_gamma))







##### scaling variables

.num_vars <- c("age_t0", "nihss_score_w1", "aerobic_score_w1",
               "strength_score_w1", "CCMI_score_t0", "madrs_w1")
.scales <- build_scales(dataSet = moca.long.adj, cols = c(.num_vars), verbose = TRUE)
moca.long.adj <- fastScale(dataSet = moca.long.adj, scales = .scales, verbose = TRUE)
.moca_sc <- build_scales(dataSet = moca.long.adj, cols = "moca_baseline_gamma", verbose = TRUE)
moca.long.adj <- fastScale(dataSet = moca.long.adj, scales = .moca_sc, verbose = TRUE)








##### Gamma models adjusted ####

.stats=lapply(mixed.vars,
             function(var) {
               mixed <- glmer(
                 formula(trimws(paste("moca.score.gamma ~ ",
                                      var, "+ offset(moca_baseline_gamma) + (1 | id)"))),
                 data=moca.long.adj,
                 family = Gamma(link = "identity"),
                 control=glmerControl(optimizer = "optimx",
                                      optCtrl = list(method = "nlminb",
                                                     maxit = 1e9)))
               stats=summary(mixed)$coefficients
               pos = grep(var, rownames(stats))
               
               
               # table of estimates with 95% CI
               se <- sqrt(diag(vcov(mixed)))
               stats <- cbind("Value" = fixef(mixed), "lower bound" = fixef(mixed) - 1.96 * se, 
                              "upper bound" = fixef(mixed) + 1.96 * se, "p.value"=stats[pos,"Pr(>|z|)"])
               
               stats=matrix(c(
                 stats[pos,"Value"],stats[pos,"lower bound"],stats[pos,"upper bound"],
                 stats[pos,"p.value"]),ncol=4,
                 dimnames = list(c(rownames(stats)[pos]),
                                 c("beta","lb","ub","p-value"))) %>%
                 round(3)
               stats=
                 data.frame(cbind("var"=rownames(stats),
                                  "beta"=paste0(stats[,'beta']," (",stats[,'lb']," to ",
                                                stats[,'ub'], ")"),
                                  "p.value"=stats[,"p-value"]))
             })

.stats = data.frame(purrr::map_df(.stats, bind_rows))
write.csv(
  .stats %>% 
    filter(p.value < .05) %>% mutate(p.value = ifelse(p.value!="" & 
                                                        p.value < 0.001, "<.001", p.value)),
  "gamma-adjusted-moca-baseline.csv", row.names = F)



### creating formulas with time
## separating numerical and categorical variables to be analyzed (from previous model selection steps)
names(moca.long.adj) <- gsub("\\.| |  ","_",names(moca.long.adj))
.num_vars=names(moca.long.adj)[grep("age_|nihss_|strength",names(moca.long.adj))]
.fact_vars=names(moca.long.adj)[grep("gender|educ|tia_|ht_|disab_|ethn|smok",names(moca.long.adj))]

# making all possible permutations of variables extracted above
formulas <- expand.grid(list(
  paste(t(combn(.fact_vars, 1, simplify = T))[,1]),
  
  paste(t(combn(.num_vars, 2, simplify = T))[,1], "+",
        t(combn(.num_vars, 2, simplify = T))[,2]
  
  )))


# adding time to the formulas
formulas <- paste("time","+",formulas$Var1,"+",formulas$Var2)
# formulas.no.time<- gsub("time \\+ ", "", formulas)

### making patient IDs consecutive
moca.long.adj$id <- as.character(factor(moca.long.adj$id,
                                        labels = c(1:length(unique(moca.long.adj$id)))))

fit_model <- function (
  pid,
  formula ,data) {
  
  test_data <- data[data$id == pid,]
  train_data <- data[data$id != pid,]
  
  
  ##### Gamma models
  #### creating a list of optimizers to test if the first fails
  optimx_options <- c("nlminb", "bobyqa","L-BFGS-B", "nmkb", "hjkb", "uobyqa","newuoa", "nlm")
  
  for (i in 1:length(optimx_options)) {
    set.seed(1)
    mod.glmer <- glmer(
      formula(trimws(paste("moca_score_gamma ~ ",formula, "+ moca_baseline_gamma + (1 | id) -1"))),
      data=train_data, 
      family = Gamma(link = "identity"),
      control=glmerControl(optimizer = "optimx",
                           optCtrl = list(method = optimx_options[i],
                                          maxit = 1e9)))
    if(is.null(mod.glmer@optinfo$conv$lme4$messages)){
      print(paste0("Model converged with the, ", optimx_options[i],", optimization!, pid = ",pid," formula = ",formula ))
      # print(summary(mod.glmer))
      mod.glmer
      break
    }
    
    
  }
  
  #### getting predictions

  pred.glmer <- predict(mod.glmer, newdata=test_data, allow.new.levels=T)
  
  
  mixed <- lqmm(fixed = as.formula(paste("moca_score ~",
                                         formula,"+ moca_baseline",-1)),
                random = ~ 1, group = id,
                data = train_data, tau = 0.5, nK = 11, type = "normal",
                na.action = na.omit)
  
  
  ##### modifying the source function for predicting in lqmm
  
  predict.lqmm.modified <- function (object, newdata ,level = 0, ...) {
    tau <- object$tau
    nq <- length(tau)
    group <- object$group
    M <- object$ngroups
    q <- object$dim_theta[2]
    
    group.new <- as.numeric(newdata$id)
    M.new <- length(unique(newdata$id))
    
    ### encoding the new data
    newdata$id <- as.numeric(newdata$id)
    enc <- dataPreparation::build_encoding(dataSet = newdata, cols = names(newdata))
    newdata.enc <- dataPreparation::one_hot_encoder(dataSet = newdata, encoding = enc, drop = TRUE)
    
    # enc <- dataPreparation::build_encoding(dataSet = train.data, cols = names(train.data))
    # train.data.enc <- dataPreparation::one_hot_encoder(dataSet = train.data, encoding = enc, drop = TRUE)
    
    ### matching the column names
    names(newdata.enc) <- gsub("\\.| |-|_","",names(newdata.enc))
    colnames(object$mmf) <- gsub("\\.| |-|_","",colnames(object$mmf))
    # names(train.data.enc) <- gsub("\\.| |-","",names(train.data.enc))
    colnames(object$mmr) <- gsub("\\.| |-|_","",colnames(object$mmr))
    
    
    ### gettting only columns used in model
    matched <- match(colnames(object$mmf),names(newdata.enc))
    library(dplyr)
    newdata.mod <- newdata.enc %>% select(matched) %>% as.matrix
    # train.data.model <- train.data.enc %>% select(matched) %>% as.matrix
    
    
    if (nq == 1) {
      FXD <- object$mmf %*% matrix(object$theta_x)
      # prediction of new data with fixed effects only, when predicting only one quartile (length of tau ==1)
      FXD.new <-  newdata.mod %*% matrix(object$theta_x)
      
    }
    else {
      # predictions as above for more than one quartile (tau > 1)
      rownames(object$theta_x) <- gsub("\\.| |-|_","",rownames(object$theta_x))
      FXD <- object$mmf %*% object$theta_x
      FXD.new <-  newdata.mod %*% object$theta_x
    }
    if (level == 1) {
      # predicting the random effects
      RE <- ranef(object)
      mmr.l <- split(object$mmr, group)
      # creating a diag matrix containing the new groups
      mmr.l.new <- split(cbind(newdata.enc ,"(Intercept)"=1) %>% 
                           select(colnames(object$mmr)), group.new)
      
      if (nq == 1) {
        # Predcting random effects from the train data for tau ==1
        RE.l <- split(RE, unique(group))
        RND <- NULL
        # container for new data
        RND.new <- NULL
        
        for (i in 1:M) {
          RND <- rbind(RND, matrix(as.numeric(mmr.l[[i]]), 
                                   ncol = q) %*% matrix(as.numeric(RE.l[[i]]), 
                                                        nrow = q))
        }
        
        # this loop returns the random effects for new data based on train data (for tau == 1)
        for (k in 1:M.new) {
          
          RND.new <- rbind(RND.new, matrix(as.numeric(unlist(mmr.l.new[[k]])),
                                           ncol = q) %*% matrix(
                                             list(
                                               ## the function below extracts the random effects from the train data
                                               VarCorr.lqmm(object) ###function adapted from nlme package 
                                             )[[k]]
                                           )
          )
        }
      }
      else {
        # random effects from train data
        RND <- matrix(NA, length(object$y), nq)
        # container for new data
        RND.new <- matrix(NA, nrow(newdata), nq)
        
        for (j in 1:nq) {
          RE.l <- split(RE[[j]], unique(group))
          tmp <- NULL
          for (i in 1:M) {
            tmp <- rbind(tmp, matrix(as.numeric(mmr.l[[i]]), 
                                     ncol = q) %*% matrix(as.numeric(RE.l[[i]]), 
                                                          nrow = q))
          }
          RND[, j] <- tmp
        }
        # this loop creates the random effects for the new data based on the random effects from the train data for tau > 1
        for (m in 1:nq) {
          RE.l.new <- split(VarCorr.lqmm(object)[[m]], unique(group.new))
          tmp <- NULL
          for (p in 1:M.new) {
            tmp <- rbind(tmp, matrix(as.numeric(unlist(mmr.l.new[[p]])),
                                     ncol = q ) %*% matrix(
                                       VarCorr.lqmm(object)[[p]]
                                     )
            )
          }
          RND.new[, m] <- tmp
        }  
        
      }
    }
    if (level == 0) {
      colnames(FXD) <- format(tau, digits = 4)
      colnames(FXD.new) <- format(tau, digits = 4)
      
      ans <- FXD[object$revOrder, ]
      ans.newdata <- round(FXD.new, 3)
      ans.newdata[newdata$time,]
      
      
    }
    if (level == 1) {
      ans <- FXD + RND
      ### analyzing how predictions are adjusted by random effects
      # cbind(cbind(FXD[1:10]) , cbind(RND[1:10]), cbind(FXD[1:10]) + cbind(RND[1:10]) )
      
      # adding the fixed and random effects for new data
      ans.newdata <- FXD.new + RND.new
      
      colnames(ans) <- format(tau, digits = 4)
      ans <- ans[object$revOrder, ]
      
      ans.newdata[newdata$time,]
      
    }
    # return only the new data
    return(ans.newdata)
  }
  
  ###using the modified function
  pred.lqmm <- predict.lqmm.modified(mixed, newdata = test_data, level = 0)
   
  
  RMSE <- function(m, o){
    sqrt(mean((m - o)^2))
  }
  
  # rmse.fixed.ef <- RMSE(rbind(pred.lqmm.fixef), cbind(data[data$id==pid,"moca.score"])) %>% round(3)
  # rmse.raef <- RMSE(rbind(pred.lqmm.raef), cbind(data[data$id==pid,"moca.score"])) %>% round(3)
  rmse.glmer <- RMSE(pred.glmer, as.matrix(data[data$id==pid,"moca_score_gamma"])) %>% round(3)
  rmse.lqmm <- RMSE(pred.lqmm, as.matrix(data[data$id==pid,"moca_score"])) %>% round(3)
  
  out <- data.frame(formula = formula,
                    rmse.glmer = rmse.glmer,
                    rmse.lqmm = rmse.lqmm,
                    observed.gamma = as.matrix(data[data$id==pid,"moca_score_gamma"]) %>% paste(collapse = ", "),
                    predicted.gamma = pred.glmer %>% round (3) %>% paste(collapse = ", "),
                    observed.lqmm = as.matrix(data[data$id==pid,"moca_score"]) %>% paste(collapse = ", "),
                    predicted.lqmm = pred.lqmm %>% round (3) %>% paste(collapse = ", "),
                    patient = pid)
  return(out)
  
}



#### testing function (only on a limited number of patients and with one formula)
lapply(1:10, fit_model, formulas[3], moca.long.adj)

### cross validation function
cross_validate <- function (combination, data) {
  
  all_ids <- unique(data$id)
  lapply(all_ids, fit_model, combination, data)
  
  
}

#### comparison between gamma and lqmm models ####

### results adjusted by baseline score
model.comparisons.2 <- lapply(formulas, cross_validate, moca.long.adj)

model.comparisons.2 <- data.frame(purrr::map_df(model.comparisons.2, 
                                                bind_rows))
saveRDS(model.comparisons.2,"model-comparisons-lqmm-vs-gamma-adjusted-moca-baseline-NO-offset.rds")






##### Plotting the results
n <- readRDS("model-comparisons-lqmm-vs-gamma-adjusted-moca-baseline-NO-offset.rds")

head(n)
n = cbind(n[-grep("observed|predict", names(n))],
          "observed.gamma" = do.call(rbind,strsplit(n$observed.gamma, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname,
          "predicted.gamma" = do.call(rbind,strsplit(n$predicted.gamma, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname,
          "observed.lqmm" = do.call(rbind,strsplit(n$observed.lqmm, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname,
          "predicted.lqmm" = do.call(rbind,strsplit(n$predicted.lqmm, ", ")) %>% 
            data.frame(stringsAsFactors = F) %>% sapply(as.numeric) %>% unname) 


### making formulas a categorical (factor) variable

n$formula <- factor(n$formula)

library(RColorBrewer)
# display.brewer.all()

#### Graphing values ####
# Define the number of colors you want
nb.cols <- nlevels(n$formula)

mycolors <- colorRampPalette(brewer.pal(12, "Paired"))(nb.cols)


## data in long format
op <- data.frame(
  melt(n %>% select(patient, grep("obser", names(.))), id.vars = "patient"),
  melt(n %>% select(patient, grep("predict", names(.))), id.vars = "patient"),
  n %>% select(formula, rmse.glmer, rmse.lqmm)
)

op <- op[order(op$patient),]

names(op)[grep("value", names(op))]=c("observed", "predicted")

op <- op[,-grep("patient.1|variable.1", names(op))]
names(op)[grep("variable", names(op))]=c("model.time")
levels(op$model.time)=c("Gamma models at 3 months", "Gamma models at 12 months",
                        "Lqmm models at 3 months", "Lqmm models at 12 months")


head(op)
op <- data.frame(
  melt(op %>% select(patient, grep("rmse", names(.))), id.vars = "patient"),
  op %>% select(names(op)[-grep("rmse|patient", names(op))])
)

head(op)

names(op)[grep("variable|value", names(op))] = c( "rmse.type", "rmse")


op$model <- ifelse(grepl('Gamma',op$model.time), "Gamma regression", "Mixed-quantile regression" )
table(op$model)
##### correlations in facet_wrap 

cors <- plyr::ddply(op, c("formula", "model.time"), 
                    summarise, cor = round(cor(observed, predicted, 
                                               method = "pearson",
                                               use = "complete.obs"), 3))


###### boxplot #####
ggplot(op, aes(x = as.numeric(formula), y = rmse)) +
  geom_boxplot(aes( color=formula) , size=.4, width=0.6 , notch = T, outlier.shape = NA) +
  geom_jitter(aes( color=formula) , 
              size=.5,  width=0.3, alpha = .2) +
  facet_wrap(~ model) + 
  theme_ipsum() +
  theme(
    legend.position="bottom",
    legend.text = element_text(size=8),
    legend.key.size = unit(4,"mm")
  ) + 
  scale_color_manual(name = "Variable combinations used in models \n(Model correlations: Gamma 3mo | 12mo | Lqmm 3mo | 12mo)*",
                     values = mycolors,
                     labels =
                       # sapply(mtcars,function(x) gsub("\\.",",",as.character(x)))
                       unlist(
                         sapply(factor(
                           paste0(as.character(sort(unique(op$formula))),
                                  " (",
                                  lapply(split(cors[,"cor"],cors[,"formula"]),
                                         paste, collapse = " | "),
                                  ")")
                         ),
                         function(x) gsub("  +"," ",
                                          gsub("diab","diabetes",
                                               gsub("ihd","isch heart dis",
                                                    gsub("baseline|p0|w1|t0|binary|total|bin|years","",
                                                         gsub("ht","hypertension",
                                                              gsub("cci","charlson cmb. index",
                                                                   gsub("time \\+ |_"," ",
                                                                        as.character(x)))))))))
                       ),
                     guide = guide_legend(
                       ncol=2, title.hjust = .5,
                       title.position = "top")
  ) +
  labs( x = "Model used", 
        y = "RMSE value (error from predicted vs. observed scores)", 
        # title = "Gamma-distribution mixed-effects models predicting frontal function performance",
        subtitle = "Pearson's correlation calculated with predicted and observed scores for all models in each time-point.\nModel comparisons based on Root Mean Square Error (RMSE) across evaluations", 
        caption = "Note: each dot represents model predictions for each patient. Prediction estimates were obtained with the leave-one-out cross-validation method (LOOCV)\n*All models included time (3 months | 12 months) as a fixed effect as well as baseline MoCA scores (as model offset in Gamma models; and as fixed effect in quantile models)" ) 



ggsave("models-rmse-Gamma-vs-lqmm-both-adjusted-by-moca-baseline.png", width = 8.5, height = 8.5, type = "cairo", dpi = 200 )
ggsave("models-rmse-Gamma-vs-lqmm-both-adjusted-by-moca-baseline.pdf", width = 8.5, height = 8.5, device = cairo_pdf)



##### sup fig 2 correlation plot in facet_wrap ######

cors <- plyr::ddply(op, c("formula", "model"), 
                    summarise, cor = round(cor(observed, predicted, 
                                               method = "pearson",
                                               use = "complete.obs"), 3))

display.brewer.all()
mycolors <- colorRampPalette(brewer.pal(9, "Set1"))(nb.cols)

ggplot(op, aes(observed, predicted)) + 
  geom_point(color="#69b3a2", size = 1, alpha = .1, shape = 3)  +
  geom_jitter(aes(col = formula),size=.05,  width=0.4, alpha = .7) +
  facet_wrap(~formula + model, nrow = 7) +
  geom_text(data=cors, aes(label=paste("r=", cor, sep="")), size = 3,
            x=25, y=11) +
  scale_color_manual(name = 
                       "Variable combinations used in models: \nFormula* (Gamma | Lqmm)",
                     values = mycolors,
                     labels =
                       # sapply(mtcars,function(x) gsub("\\.",",",as.character(x)))
                       unlist(
                         sapply(factor(
                           paste0(as.character(sort(unique(op$formula))),
                                  " (",
                                  lapply(split(cors[,"cor"],cors[,"formula"]),
                                         paste, collapse = " | "),
                                  ")")
                         ),
                         function(x) gsub("  +"," ",
                                          gsub("diab","diabetes",
                                               gsub("ihd","isch heart dis",
                                                    gsub("baseline|p0|w1|t0|binary|total|bin|years","",
                                                         gsub("ht","hypertension",
                                                              gsub("cci","charlson cmb. index",
                                                                   gsub("time \\+ |_"," ",
                                                                        as.character(x)))))))))
                       )
                     # guide = guide_legend(
                     #   ncol=1, title.hjust = .5,
                     #   title.position = "top")
  ) +
  theme_ipsum() +
  theme(
    strip.text =  element_text(color = "transparent"),
    panel.spacing.x = unit(.5, "lines"),
    panel.spacing.y = unit(-1.9, "lines")
    # strip.text.x =element_text(size=.1)
  ) +
  labs(
    # title = "LQMM models - Predicted vs observed MoCA scores accross evaluation time-points \n(time as random effect)",
    caption = "*All formulas incorporated baseline MoCA scores as well as evaluation time (3mo | 12mo) as fixed effects.\nPearson's r scores obtained by comparing scores predicted and observed, for each subject and model tested") +
  guides(size = guide_legend(jitter=2),
         colour = guide_legend(ncol = 1,
                               override.aes = list(size=3, point = 3)))

ggsave("Gamma-vs-lqmm-correlations.png", width = 12, height = 11, type = "cairo", dpi=200)
ggsave("Gamma-vs-lqmm-correlations.pdf", width = 12, height = 11, device = cairo_pdf)





