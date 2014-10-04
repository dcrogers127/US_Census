#############################################
# Program: GBM.R
#
# Notes:
#
#
#
#
# Date: Mon 09/03/2012 
#
#############################################

rm(list = ls(all = TRUE))
library(gbm)
set.seed(93920)

coor <- read.csv("C:\\Users\\Clark\\Documents\\R\\US_Census\\Data\\CenPop2010_Mean_BG.txt")
coor <- coor[,-5]
colnames(coor)[1:4] <- c("State", "County", "Tract", "Block_Group")

data <- read.csv("C:\\Users\\Clark\\Documents\\R\\US_Census\\Data\\training_filev1.csv")
final <- read.csv("C:\\Users\\Clark\\Documents\\R\\US_Census\\Data\\test_filev1.csv")

money <- c("Med_HHD_Inc_BG_ACS_06_10", "Med_HHD_Inc_BG_ACSMOE_06_10", 
           "Med_HHD_Inc_TR_ACS_06_10", "Med_HHD_Inc_TR_ACSMOE_06_10", 
           "Aggregate_HH_INC_ACS_06_10", "Aggregate_HH_INC_ACSMOE_06_10",
           "Med_House_Val_BG_ACS_06_10", "Med_House_Val_BG_ACSMOE_06_10",
           "Med_house_val_tr_ACS_06_10", "Med_house_val_tr_ACSMOE_06_10",
           "Aggr_House_Value_ACS_06_10", "Aggr_House_Value_ACSMOE_06_10")
for (m in money) {
    final[m] <- as.numeric(gsub("[[:punct:]]", "", final[,names(data)==m]))
    data[m] <- as.numeric(gsub("[[:punct:]]", "", data[,names(data)==m]))

    final[is.na(final[,names(data)==m]),names(final)==m] <- median(data[,names(data)==m], na.rm=TRUE)
    data[is.na(data[,names(data)==m]),names(data)==m] <- median(data[,names(data)==m], na.rm=TRUE)
}

o.numeric <- c("LAND_AREA", "Med_HHD_Inc_BG_ACS_06_10", "Med_HHD_Inc_BG_ACSMOE_06_10",
               "Med_HHD_Inc_TR_ACS_06_10", "Med_HHD_Inc_TR_ACSMOE_06_10",
               "Aggregate_HH_INC_ACS_06_10", "Aggregate_HH_INC_ACSMOE_06_10",
               "Med_House_Val_BG_ACS_06_10", "Med_House_Val_BG_ACSMOE_06_10",
               "Med_house_val_tr_ACS_06_10", "Med_house_val_tr_ACSMOE_06_10",
               "Aggr_House_Value_ACS_06_10", "Aggr_House_Value_ACSMOE_06_10",
               "TEA_Mail_Out_Mail_Back_CEN_2010", "TEA_Update_Leave_CEN_2010",
               "BILQ_Mailout_count_CEN_2010")
for (m in o.numeric) {
    final[is.na(final[,names(data)==m]),names(final)==m] <- median(data[,names(data)==m], na.rm=TRUE)
    data[is.na(data[,names(data)==m]),names(data)==m] <- median(data[,names(data)==m], na.rm=TRUE)
}

data$PCT_Males_CEN_2010 <- data$Males_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Males_ACS_06_10 <- data$Males_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Females_CEN_2010 <- data$Females_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Females_ACS_06_10 <- data$Females_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_under_5_CEN_2010 <- data$Pop_under_5_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_under_5_ACS_06_10 <- data$Pop_under_5_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_5_17_CEN_2010 <- data$Pop_5_17_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_5_17_ACS_06_10 <- data$Pop_5_17_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_18_24_CEN_2010 <- data$Pop_18_24_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_18_24_ACS_06_10 <- data$Pop_18_24_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_25_44_CEN_2010 <- data$Pop_25_44_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_25_44_ACS_06_10 <- data$Pop_25_44_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_45_64_CEN_2010 <- data$Pop_45_64_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_45_64_ACS_06_10 <- data$Pop_45_64_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_65plus_CEN_2010 <- data$Pop_65plus_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Pop_65plus_ACS_06_10 <- data$Pop_65plus_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Tot_GQ_CEN_2010 <- data$Tot_GQ_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Inst_GQ_CEN_2010 <- data$Inst_GQ_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Non_Inst_GQ_CEN_2010 <- data$Non_Inst_GQ_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Hispanic_CEN_2010 <- data$Hispanic_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_Hispanic_ACS_06_10 <- data$Hispanic_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_White_alone_CEN_2010 <- data$NH_White_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_White_alone_ACS_06_10 <- data$NH_White_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_Blk_alone_CEN_2010 <- data$NH_Blk_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_Blk_alone_ACS_06_10 <- data$NH_Blk_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_AIAN_alone_CEN_2010 <- data$NH_AIAN_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_AIAN_alone_ACS_06_10 <- data$NH_AIAN_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_Asian_alone_CEN_2010 <- data$NH_Asian_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_Asian_alone_ACS_06_10 <- data$NH_Asian_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_NHOPI_alone_CEN_2010 <- data$NH_NHOPI_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_NHOPI_alone_ACS_06_10 <- data$NH_NHOPI_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_NH_SOR_alone_CEN_2010 <- data$NH_SOR_alone_CEN_2010/data$Tot_Population_CEN_2010
data$PCT_NH_SOR_alone_ACS_06_10 <- data$NH_SOR_alone_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_5yrs_Over_ACS_06_10 <- data$Pop_5yrs_Over_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Othr_Lang_ACS_06_10 <- data$Othr_Lang_ACS_06_10/(data$Tot_Population_ACS_06_10-data$Pop_under_5_ACS_06_10)
data$PCT_Pop_25yrs_Over_ACS_06_10 <- data$Pop_25yrs_Over_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Not_HS_Grad_ACS_06_10 <- data$Not_HS_Grad_ACS_06_10/(data$Pop_25_44_ACS_06_10+data$Pop_45_64_ACS_06_10+data$Pop_65plus_ACS_06_10)
data$PCT_College_ACS_06_10 <- data$College_ACS_06_10/(data$Pop_25_44_ACS_06_10+data$Pop_45_64_ACS_06_10+data$Pop_65plus_ACS_06_10)
data$PCT_Prs_Blw_Pov_Lev_ACS_06_10 <- data$Prs_Blw_Pov_Lev_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pov_Univ_ACS_06_10 <- data$Pov_Univ_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Pop_1yr_Over_ACS_06_10 <- data$Pop_1yr_Over_ACS_06_10/data$Tot_Population_ACS_06_10
data$PCT_Diff_HU_1yr_Ago_ACS_06_10 <- data$Diff_HU_1yr_Ago_ACS_06_10/data$Pop_1yr_Over_ACS_06_10
data$PCT_ENG_VW_SPAN_ACS_06_10 <- data$ENG_VW_SPAN_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_ENG_VW_INDO_EURO_ACS_06_10 <- data$ENG_VW_INDO_EURO_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_ENG_VW_API_ACS_06_10 <- data$ENG_VW_API_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_ENG_VW_OTHER_ACS_06_10 <- data$ENG_VW_OTHER_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_ENG_VW_ACS_06_10 <- data$ENG_VW_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Rel_Family_HHDS_CEN_2010 <- data$Rel_Family_HHDS_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Rel_Family_HHD_ACS_06_10 <- data$Rel_Family_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_MrdCple_Fmly_HHD_CEN_2010 <- data$MrdCple_Fmly_HHD_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_MrdCple_Fmly_HHD_ACS_06_10 <- data$MrdCple_Fmly_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Not_MrdCple_HHD_CEN_2010 <- data$Not_MrdCple_HHD_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Not_MrdCple_HHD_ACS_06_10 <- data$Not_MrdCple_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Female_No_HB_CEN_2010 <- data$Female_No_HB_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Female_No_HB_ACS_06_10 <- data$Female_No_HB_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_NonFamily_HHD_CEN_2010 <- data$NonFamily_HHD_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_NonFamily_HHD_ACS_06_10 <- data$NonFamily_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Sngl_Prns_HHD_CEN_2010 <- data$Sngl_Prns_HHD_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Sngl_Prns_HHD_ACS_06_10 <- data$Sngl_Prns_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_HHD_PPL_Und_18_CEN_2010 <- data$HHD_PPL_Und_18_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_HHD_PPL_Und_18_ACS_06_10 <- data$HHD_PPL_Und_18_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Tot_Prns_in_HHD_CEN_2010 <- data$Tot_Prns_in_HHD_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Tot_Prns_in_HHD_ACS_06_10 <- data$Tot_Prns_in_HHD_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Rel_Child_Under_6_CEN_2010 <- data$Rel_Child_Under_6_CEN_2010/data$Rel_Family_HHDS_CEN_2010
data$PCT_Rel_Child_Under_6_ACS_06_10 <- data$Rel_Child_Under_6_ACS_06_10/data$Rel_Family_HHD_ACS_06_10
data$PCT_HHD_Moved_in_ACS_06_10 <- data$HHD_Moved_in_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_PUB_ASST_INC_ACS_06_10 <- data$PUB_ASST_INC_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Aggregate_HH_INC_ACS_06_10 <- data$Aggregate_HH_INC_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Tot_Occp_Units_CEN_2010 <- data$Tot_Occp_Units_CEN_2010/data$Tot_Housing_Units_CEN_2010
data$PCT_Tot_Occp_Units_ACS_06_10 <- data$Tot_Occp_Units_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Tot_Vacant_Units_CEN_2010 <- data$Tot_Vacant_Units_CEN_2010/data$Tot_Housing_Units_CEN_2010
data$PCT_Tot_Vacant_Units_ACS_06_10 <- data$Tot_Vacant_Units_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Renter_Occp_HU_CEN_2010 <- data$Renter_Occp_HU_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Renter_Occp_HU_ACS_06_10 <- data$Renter_Occp_HU_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Owner_Occp_HU_CEN_2010 <- data$Owner_Occp_HU_CEN_2010/data$Tot_Occp_Units_CEN_2010
data$PCT_Owner_Occp_HU_ACS_06_10 <- data$Owner_Occp_HU_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Single_Unit_ACS_06_10 <- data$Single_Unit_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_MLT_U2_9_STRC_ACS_06_10 <- data$MLT_U2_9_STRC_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_MLT_U10p_ACS_06_10 <- data$MLT_U10p_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Mobile_Homes_ACS_06_10 <- data$Mobile_Homes_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Crowd_Occp_U_ACS_06_10 <- data$Crowd_Occp_U_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_Occp_U_NO_PH_SRVC_ACS_06_10 <- data$Occp_U_NO_PH_SRVC_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_No_Plumb_ACS_06_10 <- data$No_Plumb_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Built_Last_5_yrs_ACS_06_10 <- data$Built_Last_5_yrs_ACS_06_10/data$Tot_Housing_Units_ACS_06_10
data$PCT_Aggr_House_Value_ACS_06_10 <- data$Aggr_House_Value_ACS_06_10/data$Tot_Occp_Units_ACS_06_10
data$PCT_MailBack_Area_Count_CEN_2010 <- data$MailBack_Area_Count_CEN_2010 /data$Tot_Housing_Units_CEN_2010
data$PCT_TEA_Mail_Out_Mail_Back_CEN_2010 <- data$TEA_Mail_Out_Mail_Back_CEN_2010 /data$MailBack_Area_Count_CEN_2010
data$PCT_TEA_Update_Leave_CEN_2010 <- data$TEA_Update_Leave_CEN_2010 /data$MailBack_Area_Count_CEN_2010
data$PCT_BILQ_Mailout_count_CEN_2010 <- data$BILQ_Mailout_count_CEN_2010 /data$MailBack_Area_Count_CEN_2010

final$PCT_Males_CEN_2010 <- final$Males_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Males_ACS_06_10 <- final$Males_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Females_CEN_2010 <- final$Females_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Females_ACS_06_10 <- final$Females_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_under_5_CEN_2010 <- final$Pop_under_5_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_under_5_ACS_06_10 <- final$Pop_under_5_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_5_17_CEN_2010 <- final$Pop_5_17_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_5_17_ACS_06_10 <- final$Pop_5_17_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_18_24_CEN_2010 <- final$Pop_18_24_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_18_24_ACS_06_10 <- final$Pop_18_24_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_25_44_CEN_2010 <- final$Pop_25_44_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_25_44_ACS_06_10 <- final$Pop_25_44_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_45_64_CEN_2010 <- final$Pop_45_64_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_45_64_ACS_06_10 <- final$Pop_45_64_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_65plus_CEN_2010 <- final$Pop_65plus_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Pop_65plus_ACS_06_10 <- final$Pop_65plus_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Tot_GQ_CEN_2010 <- final$Tot_GQ_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Inst_GQ_CEN_2010 <- final$Inst_GQ_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Non_Inst_GQ_CEN_2010 <- final$Non_Inst_GQ_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Hispanic_CEN_2010 <- final$Hispanic_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_Hispanic_ACS_06_10 <- final$Hispanic_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_White_alone_CEN_2010 <- final$NH_White_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_White_alone_ACS_06_10 <- final$NH_White_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_Blk_alone_CEN_2010 <- final$NH_Blk_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_Blk_alone_ACS_06_10 <- final$NH_Blk_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_AIAN_alone_CEN_2010 <- final$NH_AIAN_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_AIAN_alone_ACS_06_10 <- final$NH_AIAN_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_Asian_alone_CEN_2010 <- final$NH_Asian_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_Asian_alone_ACS_06_10 <- final$NH_Asian_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_NHOPI_alone_CEN_2010 <- final$NH_NHOPI_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_NHOPI_alone_ACS_06_10 <- final$NH_NHOPI_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_NH_SOR_alone_CEN_2010 <- final$NH_SOR_alone_CEN_2010/final$Tot_Population_CEN_2010
final$PCT_NH_SOR_alone_ACS_06_10 <- final$NH_SOR_alone_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_5yrs_Over_ACS_06_10 <- final$Pop_5yrs_Over_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Othr_Lang_ACS_06_10 <- final$Othr_Lang_ACS_06_10/(final$Tot_Population_ACS_06_10-final$Pop_under_5_ACS_06_10)
final$PCT_Pop_25yrs_Over_ACS_06_10 <- final$Pop_25yrs_Over_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Not_HS_Grad_ACS_06_10 <- final$Not_HS_Grad_ACS_06_10/(final$Pop_25_44_ACS_06_10+final$Pop_45_64_ACS_06_10+final$Pop_65plus_ACS_06_10)
final$PCT_College_ACS_06_10 <- final$College_ACS_06_10/(final$Pop_25_44_ACS_06_10+final$Pop_45_64_ACS_06_10+final$Pop_65plus_ACS_06_10)
final$PCT_Prs_Blw_Pov_Lev_ACS_06_10 <- final$Prs_Blw_Pov_Lev_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pov_Univ_ACS_06_10 <- final$Pov_Univ_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Pop_1yr_Over_ACS_06_10 <- final$Pop_1yr_Over_ACS_06_10/final$Tot_Population_ACS_06_10
final$PCT_Diff_HU_1yr_Ago_ACS_06_10 <- final$Diff_HU_1yr_Ago_ACS_06_10/final$Pop_1yr_Over_ACS_06_10
final$PCT_ENG_VW_SPAN_ACS_06_10 <- final$ENG_VW_SPAN_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_ENG_VW_INDO_EURO_ACS_06_10 <- final$ENG_VW_INDO_EURO_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_ENG_VW_API_ACS_06_10 <- final$ENG_VW_API_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_ENG_VW_OTHER_ACS_06_10 <- final$ENG_VW_OTHER_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_ENG_VW_ACS_06_10 <- final$ENG_VW_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Rel_Family_HHDS_CEN_2010 <- final$Rel_Family_HHDS_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Rel_Family_HHD_ACS_06_10 <- final$Rel_Family_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_MrdCple_Fmly_HHD_CEN_2010 <- final$MrdCple_Fmly_HHD_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_MrdCple_Fmly_HHD_ACS_06_10 <- final$MrdCple_Fmly_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Not_MrdCple_HHD_CEN_2010 <- final$Not_MrdCple_HHD_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Not_MrdCple_HHD_ACS_06_10 <- final$Not_MrdCple_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Female_No_HB_CEN_2010 <- final$Female_No_HB_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Female_No_HB_ACS_06_10 <- final$Female_No_HB_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_NonFamily_HHD_CEN_2010 <- final$NonFamily_HHD_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_NonFamily_HHD_ACS_06_10 <- final$NonFamily_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Sngl_Prns_HHD_CEN_2010 <- final$Sngl_Prns_HHD_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Sngl_Prns_HHD_ACS_06_10 <- final$Sngl_Prns_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_HHD_PPL_Und_18_CEN_2010 <- final$HHD_PPL_Und_18_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_HHD_PPL_Und_18_ACS_06_10 <- final$HHD_PPL_Und_18_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Tot_Prns_in_HHD_CEN_2010 <- final$Tot_Prns_in_HHD_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Tot_Prns_in_HHD_ACS_06_10 <- final$Tot_Prns_in_HHD_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Rel_Child_Under_6_CEN_2010 <- final$Rel_Child_Under_6_CEN_2010/final$Rel_Family_HHDS_CEN_2010
final$PCT_Rel_Child_Under_6_ACS_06_10 <- final$Rel_Child_Under_6_ACS_06_10/final$Rel_Family_HHD_ACS_06_10
final$PCT_HHD_Moved_in_ACS_06_10 <- final$HHD_Moved_in_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_PUB_ASST_INC_ACS_06_10 <- final$PUB_ASST_INC_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Aggregate_HH_INC_ACS_06_10 <- final$Aggregate_HH_INC_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Tot_Occp_Units_CEN_2010 <- final$Tot_Occp_Units_CEN_2010/final$Tot_Housing_Units_CEN_2010
final$PCT_Tot_Occp_Units_ACS_06_10 <- final$Tot_Occp_Units_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Tot_Vacant_Units_CEN_2010 <- final$Tot_Vacant_Units_CEN_2010/final$Tot_Housing_Units_CEN_2010
final$PCT_Tot_Vacant_Units_ACS_06_10 <- final$Tot_Vacant_Units_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Renter_Occp_HU_CEN_2010 <- final$Renter_Occp_HU_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Renter_Occp_HU_ACS_06_10 <- final$Renter_Occp_HU_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Owner_Occp_HU_CEN_2010 <- final$Owner_Occp_HU_CEN_2010/final$Tot_Occp_Units_CEN_2010
final$PCT_Owner_Occp_HU_ACS_06_10 <- final$Owner_Occp_HU_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Single_Unit_ACS_06_10 <- final$Single_Unit_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_MLT_U2_9_STRC_ACS_06_10 <- final$MLT_U2_9_STRC_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_MLT_U10p_ACS_06_10 <- final$MLT_U10p_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Mobile_Homes_ACS_06_10 <- final$Mobile_Homes_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Crowd_Occp_U_ACS_06_10 <- final$Crowd_Occp_U_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_Occp_U_NO_PH_SRVC_ACS_06_10 <- final$Occp_U_NO_PH_SRVC_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_No_Plumb_ACS_06_10 <- final$No_Plumb_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Built_Last_5_yrs_ACS_06_10 <- final$Built_Last_5_yrs_ACS_06_10/final$Tot_Housing_Units_ACS_06_10
final$PCT_Aggr_House_Value_ACS_06_10 <- final$Aggr_House_Value_ACS_06_10/final$Tot_Occp_Units_ACS_06_10
final$PCT_MailBack_Area_Count_CEN_2010 <- final$MailBack_Area_Count_CEN_2010 /final$Tot_Housing_Units_CEN_2010
final$PCT_TEA_Mail_Out_Mail_Back_CEN_2010 <- final$TEA_Mail_Out_Mail_Back_CEN_2010 /final$MailBack_Area_Count_CEN_2010
final$PCT_TEA_Update_Leave_CEN_2010 <- final$TEA_Update_Leave_CEN_2010/final$MailBack_Area_Count_CEN_2010
final$PCT_BILQ_Mailout_count_CEN_2010 <- final$BILQ_Mailout_count_CEN_2010/final$MailBack_Area_Count_CEN_2010

data <- merge(x=data, y=coor, by=c("State","County","Tract","Block_Group"), all.x=TRUE, sort=FALSE)
final <- merge(x=final, y=coor, by=c("State","County","Tract","Block_Group"), all.x=TRUE, sort=FALSE)

y.all <- data$Mail_Return_Rate_CEN_2010
w.all <- data$weight

y.final <- final$Mail_Return_Rate_CEN_2010
w.final <- final$weight

data <- data[,c(-171,-172)]
final <- final[,c(-171)]

indices_to_use <- apply(as.matrix(1:261), 1, function(i) sum(is.na(data[,i])) == 0 & sum(is.na(final[,i])) == 0 & is.numeric(data[,i])  == TRUE)

x.all <- as.matrix(data[,which(indices_to_use)])
x.final <- as.matrix(final[,which(indices_to_use)])

n.trees = 12000
shrinkage = 0.07
depth = 10
min.obs = 40
dist = "laplace"

GBM_model_all <- gbm.fit(
    x = x.all
    ,y = y.all
    ,distribution = dist
    ,n.trees = n.trees
    ,shrinkage = shrinkage
    ,interaction.depth = depth
    ,n.minobsinnode = min.obs
    ,verbose = TRUE)

yhat.final <- predict.gbm(object = GBM_model_all ,newdata = x.final ,n.trees)
write.csv(yhat.final, file = "C:\\Users\\Clark\\Documents\\R\\US_Census\\GBM\\pred_gbm19_20120922.csv", row.names = FALSE)

GBM_model_all <- gbm.more(GBM_model_all,
    n.new.trees = 4000,
    data = NULL,
    weights = NULL,
    offset = NULL,
    verbose = NULL)

yhat.final <- predict.gbm(object = GBM_model_all ,newdata = x.final ,16000)
write.csv(yhat.final, file = "C:\\Users\\Clark\\Documents\\R\\US_Census\\GBM\\pred_gbm20_20120922.csv", row.names = FALSE)

set.seed()
         
GBM_model_all <- gbm.more(GBM_model_all,
    n.new.trees = 4000,
    data = NULL,
    weights = NULL,
    offset = NULL,
    verbose = NULL)

yhat.final <- predict.gbm(object = GBM_model_all ,newdata = x.final ,20000)
write.csv(yhat.final, file = "C:\\Users\\Clark\\Documents\\R\\US_Census\\GBM\\pred_gbm21_20120922.csv", row.names = FALSE)

GBM_model_all <- gbm.more(GBM_model_all,
    n.new.trees = 5000,
    data = NULL,
    weights = NULL,
    offset = NULL,
    verbose = NULL)

yhat.final <- predict.gbm(object = GBM_model_all ,newdata = x.final ,25000)
write.csv(yhat.final, file = "C:\\Users\\Clark\\Documents\\R\\US_Census\\GBM\\pred_gbm22_20120922.csv", row.names = FALSE)

GBM_model_all <- gbm.more(GBM_model_all,
    n.new.trees = 5000,
    data = NULL,
    weights = NULL,
    offset = NULL,
    verbose = NULL)

yhat.final <- predict.gbm(object = GBM_model_all ,newdata = x.final ,30000)
write.csv(yhat.final, file = "C:\\Users\\Clark\\Documents\\R\\US_Census\\GBM\\pred_gbm23_20120922.csv", row.names = FALSE)

