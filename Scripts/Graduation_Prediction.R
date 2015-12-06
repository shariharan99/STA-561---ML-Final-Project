library(glmnet)
library(DAAG)

dir <- dir("data/Complete_Data_wdiver_Nov19", 
           full.names = TRUE)

stepwise_coef <- c("MAS_COHORT_1112","MHI_COHORT_1112","MTR_COHORT_1112","MWH_COHORT_1112","CWD_COHORT_1112","ECD_COHORT_1112","State","County","LAND_AREA","URBANIZED_AREA_POP_CEN_2010","URBAN_CLUSTER_POP_CEN_2010","RURAL_POP_CEN_2010","Tot_Population_ACS_08_12","Males_CEN_2010","Males_ACS_08_12","Pop_5_17_CEN_2010","Pop_18_24_CEN_2010","Pop_18_24_ACS_08_12","Pop_25_44_ACS_08_12","Pop_45_64_ACS_08_12","Hispanic_CEN_2010","NH_White_alone_CEN_2010","NH_Blk_alone_CEN_2010","NH_AIAN_alone_ACS_08_12","NH_Asian_alone_CEN_2010","NH_SOR_alone_ACS_08_12","Othr_Lang_ACS_08_12","Age5p_French_ACS_08_12","Age5p_German_ACS_08_12","Age5p_WGerman_ACS_08_12","Age5p_Scandinavian_ACS_08_12","Age5p_SRBCroatian_ACS_08_12","Age5p_OthPacIsl_ACS_08_12","Age5p_NativeNAm_ACS_08_12","Age5p_Arabic_ACS_08_12","Pov_Univ_ACS_08_12","Civ_labor_16_24_ACS_08_12","Civ_emp_16_24_ACS_08_12","Civ_emp_25_44_ACS_08_12","Civ_emp_45_64_ACS_08_12","Pop_1yr_Over_ACS_08_12","ENG_VW_INDO_EURO_ACS_08_12","Not_MrdCple_HHD_CEN_2010","Not_MrdCple_HHD_ACS_08_12","Female_No_HB_ACS_08_12","Sngl_Prns_HHD_CEN_2010","HHD_PPL_Und_18_CEN_2010","Tot_Prns_in_HHD_ACS_08_12","Rel_Child_Under_6_CEN_2010","Tot_Housing_Units_ACS_08_12","Single_Unit_ACS_08_12","MLT_U2_9_STRC_ACS_08_12","MLT_U10p_ACS_08_12","Mobile_Homes_ACS_08_12","Crowd_Occp_U_ACS_08_12","Occp_U_NO_PH_SRVC_ACS_08_12","No_Plumb_ACS_08_12","Med_House_value_ACS_08_12","Aggr_House_Value_ACS_08_12","MailBack_Area_Count_CEN_2010","Census_Mail_Returns_CEN_2010","Vacants_CEN_2010","Deletes_CEN_2010","Census_UAA_CEN_2010","FRST_FRMS_CEN_2010","Mail_Return_Rate_CEN_2010","Low_Response_Score","pct_URBANIZED_AREA_POP_CEN_2010","pct_RURAL_POP_CEN_2010","pct_Males_ACS_08_12","pct_Females_CEN_2010","pct_Pop_Under_5_CEN_2010","pct_Pop_5_17_ACS_08_12","pct_Pop_25_44_ACS_08_12","pct_Pop_45_64_CEN_2010","pct_Pop_65plus_CEN_2010","pct_Inst_GQ_CEN_2010","pct_Hispanic_CEN_2010","pct_Hispanic_ACS_08_12","pct_NH_White_alone_CEN_2010","pct_NH_Blk_alone_CEN_2010","pct_NH_AIAN_alone_ACS_08_12","pct_NH_SOR_alone_ACS_08_12","pct_Age5p_Spanish_ACS_08_12","pct_Age5p_French_ACS_08_12","pct_Age5p_German_ACS_08_12","pct_Age5p_WGerman_ACS_08_12","pct_Age5p_SRBCroati_ACS_08_12","pct_Age5p_NativeNAm_ACS_08_12","pct_Age5p_Arabic_ACS_08_12","pct_Age5p_African_ACS_08_12","pct_Age5p_OthUnSp_ACS_08_12","pct_Prs_Blw_Pov_Lev_ACS_08_12","pct_Civ_unemp_16p_ACS_08_12","pct_Civ_emp_16_24_ACS_08_12","pct_Civ_unemp_65p_ACS_08_12","pct_Pop_1yr_Over_ACS_08_12","pct_Diff_HU_1yr_Ago_ACS_08_12","pct_Born_US_ACS_08_12","pct_ENG_VW_INDOEURO_ACS_08_12","pct_ENG_VW_OTHER_ACS_08_12","pct_MrdCple_HHD_ACS_08_12","pct_Not_MrdCple_HHD_ACS_08_12","pct_Female_No_HB_ACS_08_12","pct_HHD_PPL_Und_18_ACS_08_12","avg_Tot_Prns_in_HHD_ACS_08_12","pct_HHD_Moved_in_ACS_08_12","pct_PUB_ASST_INC_ACS_08_12","pct_Tot_Occp_Units_CEN_2010","pct_Vacant_Units_CEN_2010","pct_Vacant_Units_ACS_08_12","pct_Renter_Occp_HU_CEN_2010","pct_Renter_Occp_HU_ACS_08_12","pct_Owner_Occp_HU_CEN_2010","pct_Single_Unit_ACS_08_12","pct_MLT_U2_9_STRC_ACS_08_12","pct_MLT_U10p_ACS_08_12","pct_Mobile_Homes_ACS_08_12","pct_Crowd_Occp_U_ACS_08_12","pct_NO_PH_SRVC_ACS_08_12","pct_No_Plumb_ACS_08_12","diver","Civ_labor_16plus_ACS_08_12","pct_Pop_5_17_CEN_2010","pct_Females_ACS_08_12","Non_Inst_GQ_CEN_2010","Pop_under_5_ACS_08_12","NH_AIAN_alone_CEN_2010")
mse_lasso <- rep(0, length(dir))
mse_fwdstep <- rep(0, length(dir))
mse_ridge <- rep(0, length(dir))
mse_enet <- rep(0, length(dir))


for (i in 1:length(dir)){
  file <- read.table(dir[i], header = TRUE)[,-1]

  #Lasso#
  
  lasso.object <- cv.glmnet(as.matrix(file[,-which(colnames(file) == "ALL_RATE_1112")]),
                            as.vector(file[,"ALL_RATE_1112"]),
                            family = "gaussian")
  
  min_lambda <- lasso.object$lambda.min
  
  mse_lasso[i] <- lasso.object$cvm[which(lasso.object$lambda == min_lambda)]
  
  #Linear Regression from Forward Stepwise#
  
  file2 <- cbind(ALL_RATE_1112 = file[,"ALL_RATE_1112"], file[,stepwise_coef])
  
  cv.predictions <- cv.lm(ALL_RATE_1112 ~ ., data = file2, 
                          printit = FALSE)[,"Predicted"]
  
  mse_fwdstep[i] <- mean((cv.predictions - file2[,"ALL_RATE_1112"])^2)
  
  # Ridge #
  
  ridge.object <- cv.glmnet(as.matrix(file[,-which(colnames(file) == "ALL_RATE_1112")]),
                            as.vector(file[,"ALL_RATE_1112"]),
                            family = "gaussian", alpha = 0)
  
  min_lambda <- ridge.object$lambda.min
  
  mse_ridge[i] <- ridge.object$cvm[which(ridge.object$lambda == min_lambda)]
  
  #Elastic Net#
  
  enet.object <- cv.glmnet(as.matrix(file[,-which(colnames(file) == "ALL_RATE_1112")]),
                            as.vector(file[,"ALL_RATE_1112"]),
                            family = "gaussian", alpha = 0.5)
  
  min_lambda <- enet.object$lambda.min
  
  mse_enet[i] <- enet.object$cvm[which(enet.object$lambda == min_lambda)]
  
  rm(file)
  rm(file2)
  
  print(paste(i, "Iteration"))
}

total_mse <- cbind(mse_lasso, mse_ridge, mse_enet, mse_fwdstep)
colnames(total_mse) <- c("Lasso", "Ridge", "Elastic Net", "Stepwise")

mean_mse <- apply(total_mse, 2, mean)


