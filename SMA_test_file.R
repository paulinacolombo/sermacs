#######################################################
# Code for cholera stuff
#######################################################

###################
#analyses
################### descriptive stats ####
#if MG_ICP1 empty, was no metagenomics data for that person
table(is.na(combo$MG_ICP1))
# FALSE  TRUE 
# 218  1860 
table(combo$ICE)
# ind5   ind6 No_ICE 
# 67     22     30 



################### Table 1 - fine 4/7/24 ####
library(table1)
table1_html <- table1(~Area_Code + Sex + age_sum_mo + as.factor(age_cat) + Dehydration_Status + as.factor(Culture_Results) +
         as.factor(qPCR_tcpA_Result_CT28) + as.factor(qPCR_ICP1_Result_CT28) +
         as.factor(qPCR_ICP2_Result_CT28) + as.factor(qPCR_ICP3_ResultCT28) + 
           as.factor(AntYN_clean) | RDT, data=combo, overall=F)
table1_html
table1_df <- as.data.frame(table1_html)
# write.csv(table1_df,"", row.names = FALSE)

table1_html2 <- table1(~Area_Code + Sex + age_sum_mo + as.factor(age_cat) + Dehydration_Status + as.factor(Culture_Results) +
                        as.factor(qPCR_tcpA_Result_CT28) + as.factor(qPCR_ICP1_Result_CT28) +
                        as.factor(qPCR_ICP2_Result_CT28) + as.factor(qPCR_ICP3_ResultCT28) | RDT, data=combo, overall=F,
                       render.continuous = c(.="Median [Min, Max]", .="Median (Q1-Q3)", .="N"))
table1_html2
table1_df2 <- as.data.frame(table1_html2)
# write.csv(table1_df2,"", row.names = FALSE)



