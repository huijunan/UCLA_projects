# Survival Final Project
library(dplyr)
library(magrittr)
library(tidyr)
library(readr)
# library(caret)
# library(PAmeasures)
# library(survival)
# library(cmprsk)
# library(MASS)

# Read in data-----------------
# Dataset obtained from https://seer.cancer.gov/data-software/documentation/ascii-files.html
data <- read.table("BREAST.txt", sep="|", strip.white = F, fill = TRUE) %>%
  set_colnames("raw") %>%
  mutate(raw = as.character(raw)) %>%
  separate(raw,
           into = c('pt_id', 'rg_id', 'Marital_Status', 'Race', 'Sex', 'Dx_age',
                    'Birth_yr', 'Seq_num', 'Dx_month', 'Dx_year', 'Primary_site',
                    'Lateral', 'Hist_O2', 'Behavior_O2', 'Hist_O3', 'Behavior_O3',
                    'Grade', 'Dx_conf', 'Rept_src', 'EOD_sz', 'EOD_ex', 'EOD_pe',
                    'EOD_nd', 'EOD_pn', 'EOD_ne', 'EOD13', 'EOD2', 'EOD4', 'EOD_code',
                    'Tumor_1V', 'Tumor_2V', 'Tumor_3V', 'CSTUMORSZ', 'CSEXTEN',
                    'CSLYMPHN', 'CSMETSDX', 'CS_1site', 'CS_2site', 'CS_3site',
                    'CS_4site', 'CS_5site', 'CS_6site', 'CS_25site', 'DAJCCT',
                    'DAJCCN', 'DAJCCM', 'DAJCCSTG', 'DSS1977S', 'SCSSM2KO', 'CSVFIRST',
                    'CSVLATES', 'CSVCURRENT', 'SURGPRIF', 'SURGSCOF', 'SURGSITF',
                    'NUMNODES', 'NO_SURG', 'SS_SURG', 'SURGSCOP', 'SURGSITE', 'RECNOREC',
                    'TYPE_FU', 'AGE_1REC', 'SITERWHO', 'ICDOTO9V', 'ICDOT10V',
                    'ICCC3WHO', 'ICCC3XWHO', 'BEHTREND', 'HISTREC', 'HISTRECB',
                    'cs0204schema', 'RAC_RECA', 'RAC_RECY', 'ORIGRECB', 'HST_STGA',
                    'AJCC_STG', 'AJ_3SEER', 'SSS77VZ', 'SSSM2KPZ', 'FIRSTPRM', 'ST_CNTY',
                    'CODPUB', 'CODPUBKM', 'STAT_REC', 'IHSLINK', 'SUMM2K', 'AYASITERWHO',
                    'LYMSUBRWHO', 'VSRTSADX', 'ODTHCLASS', 'CSTSEVAL', 'CSRGEVAL',
                    'CSMTEVAL', 'INTPRIM', 'ERSTATUS', 'PRSTATUS', 'CSSCHEMA', 'CS8SITE',
                    'CS10SITE', 'CS11SITE', 'CS13SITE', 'CS15SITE', 'CS16SITE', 'VASINV',
                    'SRV_TIME_MON', 'SRV_TIME_MON_FLAG', 'INSREC_PUB', 'DAJCC7T', 'DAJCC7N',
                    'DAJCC7M', 'DAJCC7STG', 'ADJTM_6VALUE', 'ADJNM_6VALUE', 'ADJM_6VALUE',
                    'ADJAJCCSTG', 'CS7SITE', 'CS9SITE', 'CS12SITE', 'her2', 'brst_sub',
                    'ANNARBOR', 'SCMETSDXB_PUB', 'SCMETSDXBR_PUB', 'SCMETSDXLIV_PUB',
                    'SCMETSDXLUNG_PUB', 'T_VALUE', 'N_VALUE', 'M_VALUE', 'MALIGCOUNT',
                    'BENBORDCOUNT', 'TUMSIZS', 'DSRPSG', 'DASRCT', 'DASRCN', 'DASRCM',
                    'DASRCTS', 'DASRCNS', 'DASRCMS', 'TNMEDNUM', 'METSDXLN', 'METSDXO'),
           sep = c(8, 18, 19, 21, 24, 27, 31, 36, 38, 42, 46, 47, 51, 52, 56, 57,
                   58, 59, 60, 63, 65, 67, 68, 70, 72, 85, 87, 91, 92, 93, 94, 95,
                   98, 101, 104, 106, 109, 112, 115, 118, 121, 124, 127, 129, 131,
                   133, 135, 136, 137, 146, 152, 158, 160, 161, 162, 164, 166, 171,
                   174, 175, 177, 191, 193, 203, 207, 211, 220, 223, 224, 227, 229,
                   232, 233, 234, 235, 236, 238, 240, 241, 242, 245, 250, 259, 264,
                   265, 266, 267, 269, 271, 272, 273, 274, 275, 276, 277, 278, 279,
                   281, 284, 287, 290, 293, 296, 299, 300, 304, 305, 311, 314, 317,
                   320, 323, 325, 327, 329, 331, 334, 337, 340, 341, 342, 348, 349,
                   350, 351, 352, 353, 355, 357, 360, 362, 366, 371, 376, 381, 386,
                   387, 388, 389, 391, 392)) %>%
  filter(rg_id == "0000001501")

write.csv(data, "data1.csv")

data1 <- read.csv("data1.csv", na.strings = c("", "     ", "             ", "  "))

# zero-variance check
# nearZeroVar(data1, names = T, freqCut = 190, uniqueCut = 10)

# Data pre-processing------------------
data2 <- data1 %>%
  mutate(Dx_year = as.numeric(Dx_year),
         Tumor_marker = ifelse(Tumor_1V == 2 & Tumor_2V == 2, 0, 1)) %>%
  filter(Behavior_O3 == "3") %>%     # focus on malignant patients
  filter(Dx_year > 1988)            # only looking at recent cases (1988 has new EOD system)

patient_ids <- data2 %>% group_by(pt_id) %>% summarise(n = n_distinct(X)) %>% filter(n == 1)

data3 <- data2 %>%
  select(-X,             # not cancer related
         -rg_id,         # Registry ID already used for SF selection
         -Race,          # race recode available (RAC_RECA)
         -RAC_RECY,      # race recode available (RAC_RECA)
         -IHSLINK,       # race recode available (RAC_RECA)
         -ORIGRECB,      # more than 90% are not hispanic, deleting this for simplicity
         -AGE_1REC,      # Recoded age, since age dx present, no need
         -EOD_pe,        # Blank across all
         -EOD2,          # Either Blank or --(not stated) across all)
         -DAJCCM,        # Blank across all
         -SITERWHO,      # Everybody's 26000, which stands for breast
         -HISTRECB,      # Brain group, everybody's 98, which stands for "not brain"
         -cs0204schema,  # Everybody's 013, which stands for breast
         -CSSCHEMA,      # Everybody's 58, which stands for breast
         -CS8SITE,       # Blank across all
         -CS10SITE,      # Blank across all
         -CS11SITE,      # Blank across all
         -CS13SITE,      # Blank across all
         -CS16SITE,      # Blank across all
         -VASINV,        # Blank across all
         -CS9SITE,       # Blank across all
         -CS12SITE,      # Blank across all
         -CS_25site,     # same across all
         -ANNARBOR,      # Everybody's 8, which stands for Not Applicable
         -Primary_site,  # C500-509 in same category in ICD-O-3 code 
                         # (Plasmablastic lymphoma/ALK positive large B-cell lymphoma
                         # /Lrg B-cell lymphoma in HHV8-assoc. multicentric Castleman DZ)
         -Hist_O2,       # Delete old coding method
         -Hist_O3,       # Hist recode available (HISTREC)
         -Behavior_O2,   # Exactly the same as Behavior_o3
         -Behavior_O3,   # already filtered for 3, malignant cancer only/only using data after 1988
         -EOD_code,      # this can be told from previous information/year related
         -Tumor_1V,      # combined into tumor_marker
         -Tumor_2V,      # combined into tumor_marker
         -Tumor_3V,      # indicator for testis cases, not breast
         -EOD13,         # only using data after 1988
         -EOD4,          # only using data after 1988
         -ODTHCLASS,     # information covered in other variable
         -STAT_REC,      # information covered in other variable
         -NO_SURG,       # Not for every patient
         -CSVFIRST,      # Not cancer-related
         -CSVLATES,      # Not cancer-related
         -CSVCURRENT,    # Not cancer-related
         -ICDOTO9V,      # Do not need ICD9 code
         -ICDOT10V,      # Do not need ICD10 code
         -ICCC3WHO,      # more than 99% data have the same value, not meaningful for analysis
         -ICCC3XWHO,     # more than 99% data have the same value, not meaningful for analysis
         -BEHTREND,      # all the same after filtering for Behavior_O3
         -ST_CNTY,       # only looking at SF already
         -TNMEDNUM,      # Not cancer-related
         -AYASITERWHO,   # Histology info already exists
         -LYMSUBRWHO,    # Does not contain useful info
         -CSSCHEMA,      # same across all
         -SURGSCOF,      # unk/same across all
         -SURGSCOP,      # unk/same across all
         -Dx_conf,       # More than 98% #1
         -Rept_src,      # More than 98% #1
         -RECNOREC,      # information covered
         -CODPUBKM,      # same information as CODPUB
         -EOD_sz, -EOD_ex, -EOD_nd, -EOD_pn, -EOD_ne, -CSTUMORSZ, -CSEXTEN, -INTPRIM,
         -CSLYMPHN, -CSMETSDX, -CS_1site, -CS_2site, -CS_3site, -CS_4site,
         -CS_5site, -CS_6site, -DAJCCT, -DAJCCN, -DAJCCM, -DAJCCSTG, -DSS1977S,
         -SCSSM2KO, -HST_STGA, -AJCC_STG, -AJ_3SEER, -SSS77VZ, -SSSM2KPZ,
         -SUMM2K, -CSTSEVAL, -CSRGEVAL, -CSMTEVAL, -CS15SITE, -DAJCC7T,
         -DAJCC7N, -DAJCC7M, -DAJCC7STG, -CS7SITE, -CS9SITE, -CS12SITE, -T_VALUE,
         -N_VALUE, -M_VALUE, -MALIGCOUNT, -BENBORDCOUNT, -TUMSIZS,
         -DASRCTS, -DASRCNS, -DASRCMS # Stage related information - combined
  ) %>%
  filter(pt_id %in% patient_ids$pt_id) %>% # only looking at patients with one record for simplicity of the project
  mutate(ADJTM_6VALUE = ifelse(ADJTM_6VALUE==0, "T0", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==5, "Tis", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==11 | ADJTM_6VALUE==12 | ADJTM_6VALUE==15 | ADJTM_6VALUE==18, 
                               "T1", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==20, "T2", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==30, "T3", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==41 | ADJTM_6VALUE==42 | ADJTM_6VALUE==43 | ADJTM_6VALUE==44, 
                               "T4", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==60, "Any T, Mets", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==88, "NA", ADJTM_6VALUE),
         ADJTM_6VALUE = ifelse(ADJTM_6VALUE==99, "TX", ADJTM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==0, "N0", ADJNM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==10, "N1", ADJNM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==20, "N2", ADJNM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==30, "N3", ADJNM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==88, "NA", ADJNM_6VALUE),
         ADJNM_6VALUE = ifelse(ADJNM_6VALUE==99, "NX", ADJNM_6VALUE),
         ADJM_6VALUE = ifelse(ADJM_6VALUE==0, "M0", ADJM_6VALUE),
         ADJM_6VALUE = ifelse(ADJM_6VALUE==10, "M1", ADJM_6VALUE),
         ADJM_6VALUE = ifelse(ADJM_6VALUE==88, "NA", ADJM_6VALUE),
         ADJM_6VALUE = ifelse(ADJM_6VALUE==99, "MX", ADJM_6VALUE),
         DASRCT = as.character(DASRCT),
         DASRCN = as.character(DASRCN),
         DASRCM = as.character(DASRCM),
         DSRPSG = as.numeric(DSRPSG),
         DASRCT = ifelse(DASRCT==88, "NA", DASRCT),
         DASRCT = ifelse(DASRCT=="c0", "T0", DASRCT),
         DASRCT = ifelse(DASRCT=="c1" | DASRCT=="p1" | DASRCT=="c1A" | DASRCT=="p1A" | DASRCT=="c1B" | DASRCT=="p1B" |
                           DASRCT=="c1C" | DASRCT=="p1C" | DASRCT== "T1c" | DASRCT=="c1MI" | DASRCT=="p1MI", "T1", DASRCT),
         DASRCT = ifelse(DASRCT=="c2" | DASRCT=="p2", "T2", DASRCT),
         DASRCT = ifelse(DASRCT=="c3" | DASRCT=="p3", "T3", DASRCT),
         DASRCT = ifelse(DASRCT=="c4" | DASRCT=="p4" | DASRCT=="T4" | DASRCT=="c4A" | DASRCT=="p4A" | DASRCT=="T4a" | DASRCT=="c4B" | 
                           DASRCT=="p4B" | DASRCT=="c4C" | DASRCT=="p4C" | DASRCT=="c4D" | DASRCT=="p4D", "T4b", DASRCT),
         DASRCT = ifelse(DASRCT=="cx", "TX", DASRCT),
         DASRCT = ifelse(DASRCT=="pIS", "Tis", DASRCT),
         DASRCN = ifelse(DASRCN==88, "NA", DASRCN),
         DASRCN = ifelse(DASRCN=="c0" | DASRCN=="p0" | DASRCN=="p0I-" | DASRCN=="p0I+" | DASRCN=="p0M-" | 
                           DASRCN=="p0M+", "N0", DASRCN),
         DASRCN = ifelse(DASRCN=="c1" | DASRCN=="p1" | DASRCN=="p1A" | DASRCN=="p1B" | DASRCN=="p1C" | 
                           DASRCN=="p1MI", "N1", DASRCN),
         DASRCN = ifelse(DASRCN=="c2" | DASRCN=="p2" | DASRCN=="c2A" | DASRCN=="p2A" | DASRCN=="c2B", "N2", DASRCN),
         DASRCN = ifelse(DASRCN=="c3" | DASRCN=="p3" | DASRCN=="p3A" | DASRCN=="c3B" | DASRCN=="p3B" | 
                           DASRCN=="c3C", "N3", DASRCN),
         DASRCN = ifelse(DASRCN=="cx" | DASRCN=="px", "NX", DASRCN),
         DASRCM = ifelse(DASRCM==88, "NA", DASRCM),
         DASRCM = ifelse(DASRCM=="c0", "M0", DASRCM),
         DASRCM = ifelse(DASRCM=="c1" | DASRCM=="p1", "M1", DASRCM),
         ADJAJCCSTG = as.character(ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="10", "I", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="32", "IIA", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="33", "IIB", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="51", "III", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="52", "IIIA", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="53", "IIIB", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="54", "IIIC", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="70", "IV", ADJAJCCSTG),
         ADJAJCCSTG = ifelse(ADJAJCCSTG=="88" | ADJAJCCSTG=="99", "NA", ADJAJCCSTG),
         Stage = ifelse(DSRPSG==1, "0", DSRPSG),
         Stage = ifelse(DSRPSG==2, "IA", Stage),
         Stage = ifelse(DSRPSG==3, "IB", Stage),
         Stage = ifelse(DSRPSG==4, "IIA", Stage),
         Stage = ifelse(DSRPSG==5, "IIB", Stage),
         Stage = ifelse(DSRPSG==6, "III", Stage),
         Stage = ifelse(DSRPSG==7, "IIIA", Stage),
         Stage = ifelse(DSRPSG==8, "IIIB", Stage),
         Stage = ifelse(DSRPSG==9, "IIIC", Stage),
         Stage = ifelse(DSRPSG==10, "IV", Stage),
         Stage = ifelse(DSRPSG==11 | DSRPSG==12, "NA", Stage)) %>%
  mutate(T_Stage = coalesce(ADJTM_6VALUE, DASRCT),
         N_Stage = coalesce(ADJNM_6VALUE, DASRCN),
         M_Stage = coalesce(ADJM_6VALUE, DASRCM),
         Stage = coalesce(ADJAJCCSTG, Stage)) %>%
  select(-pt_id, -ADJTM_6VALUE, -ADJNM_6VALUE, -ADJM_6VALUE, -DASRCT, -DASRCN, -DASRCM, -ADJAJCCSTG, -DSRPSG) %>%
  filter(CODPUB != 41000)


colMeans(is.na(data3)) # used to find missing value percentages in each variable

breast <-  data3 %>% 
  filter(SRV_TIME_MON_FLAG == 1) %>%
  select(
    -Birth_yr, -Dx_month, -Dx_year, # no need information
    -TYPE_FU, # this column only has one value now
    -FIRSTPRM,# this column only has one value now
    -SRV_TIME_MON_FLAG, # information used
    -SURGSITF, -NUMNODES, -SURGSITE, -INSREC_PUB, -SCMETSDXB_PUB, -SCMETSDXBR_PUB, -SCMETSDXLIV_PUB, -SS_SURG,
    -SCMETSDXLUNG_PUB, -METSDXLN, -METSDXO, -her2, -brst_sub
 ) %>% # delete columns with more than 40% missing data (not deleting tumor_marker since it's important)
  mutate(Marital_Status = ifelse(Marital_Status==9, "NA", Marital_Status),
         RAC_RECA = ifelse(RAC_RECA==9, "NA", RAC_RECA),
         Grade = ifelse(Grade==9, "NA", Grade),
         ERSTATUS = ifelse(ERSTATUS==4 | ERSTATUS==3, "NA", ERSTATUS),
         PRSTATUS = ifelse(PRSTATUS==4 | PRSTATUS==3, "NA", PRSTATUS) )%>%
  filter(Lateral <=2, # unspecified or bilateral case too rare
         VSRTSADX != 9,
         SRV_TIME_MON !=0,
         Stage != "0"
         ) %>% 
  mutate_at(c("T_Stage", "N_Stage", "M_Stage", "Marital_Status", "RAC_RECA", "ERSTATUS", "PRSTATUS", "Grade", "Stage"), 
            ~na_if(., "NA")) %>% 
  na.omit()

# create dummy variables
df <- breast %>%
  mutate(Marital_Married = ifelse(Marital_Status==2, 1, 0),
         Marital_Sep = ifelse(Marital_Status==3 | Marital_Status==4, 1, 0),
         Marital_Widow = ifelse(Marital_Status==5, 1, 0), # Baseline = Single
         Sex = Sex - 1, # Baseline = Single
         One_Primary = ifelse(Seq_num==0, 1, 0), # if the patient only have one primary
         Lateral = Lateral - 1, # Right = 0
         Grade2 = ifelse(Grade==2, 1, 0),
         Grade3 = ifelse(Grade==3, 1, 0),
         Grade4 = ifelse(Grade==4, 1, 0), # Baseline = Grade1
         Surg = ifelse(SURGPRIF==0, 0, 1), # if the patient had surgery to remove primary site or not
         Hist2 = ifelse(HISTREC==2, 1, 0), # squamous cell neoplams
         Hist5 = ifelse(HISTREC==5, 1, 0), # adenomas and adenocarcinomas
         Hist6 = ifelse(HISTREC==6, 1, 0), # adnexal and skin appendage neoplams
         Hist8 = ifelse(HISTREC==8, 1, 0), # cystic, mucinous and serous neoplams 
         Hist9 = ifelse(HISTREC==9, 1, 0), # ductal and lobular neoplams 
         Hist11 = ifelse(HISTREC==11, 1, 0), # complex epithelial neoplams 
         Hist22 = ifelse(HISTREC==22, 1, 0), # fibroepithelial neoplasms, where baseline = epithelial neoplasms, NOS
         White = ifelse(RAC_RECA==1, 1, 0),
         Black = ifelse(RAC_RECA==2, 1, 0), # Baseline = Other
         ERSTATUS = ifelse(ERSTATUS==1, 1, 0), # Negative = 0
         PRSTATUS = ifelse(PRSTATUS==1, 1, 0), # Negative = 0
         StageIIA = ifelse(Stage=="IIA", 1, 0),
         StageIIB = ifelse(Stage=="IIB", 1, 0),
         StageIII = ifelse(Stage=="III", 1, 0),
         StageIIIA = ifelse(Stage=="IIIA", 1, 0),
         StageIIIB = ifelse(Stage=="IIIB", 1, 0),
         StageIIIC = ifelse(Stage=="IIIC", 1, 0),
         StageIV = ifelse(Stage=="IV", 1, 0) # Baseline == Alive
         ) %>%
  select(-Marital_Status, - Seq_num, -Grade, -SURGPRIF, -HISTREC, -RAC_RECA, -T_Stage, 
         -N_Stage, -M_Stage, -Stage) 

# KME----------------
fit1 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ 1, 
               data = df)
plot(fit1, main = 'Kaplan Meier Estimator for \nSan Francisco Breast Cancer Patients from 1988-2016',
     ylab = 'Survival Probability', xlab = 'Time in Months')

# KM by group & log rank test----------------------
# marital status
fit2 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Marital_Status), data = breast)

plot(fit2, col = c('black', 'red', 'blue', 'green'), lty = 1:4, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Marital Status')

legend("bottomleft", c("Single", "Married", "Separate/Divorced", "Widowed"), lty = 1:4,
       col = c("black", "red", "blue",'green'))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Marital_Status), data = breast, rho = 0)

# sex
fit3 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Sex), data = breast)

plot(fit3, col = c('black', 'red'), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Sex')

legend("bottomleft", c("Male", "Female"), lty = 1:2, col = c("black", "red"))
# log rank test
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Sex), data = breast, rho = 0)

# number of primary tumor
fit4 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(One_Primary), data = df)

plot(fit4, col = c('black', 'red'), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Number of primary tumor')

legend("bottomleft", c("One in lifetime", "More than one"), lty = 1:2, col = c("black", "red"))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(One_Primary), data = df, rho = 0)

# Laterality
fit5 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Lateral), data = breast)

plot(fit5, col = c('black', 'red'), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Laterality')

legend("bottomleft", c("Left", "Right"), lty = 1:2, col = c("black", "red"))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Lateral), data = breast, rho = 0)

# Grade
fit6 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Grade), data = breast)

plot(fit6, col = c('black', 'red', 'blue', 'green'), lty = 1:4, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Grade')

legend("bottomleft", c("1", "2", "3", "4"), lty = 1:4,
       col = c("black", "red", "blue",'green'))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Grade), data = breast, rho = 0)

# Surgery on primary site
fit6 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Surg), data = df)

plot(fit6, col = c('black', 'red'), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Surgery history on primary site or not')

legend("bottomleft", c("Yes", "No"), lty = 1:2, col = c("black", "red"))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Surg), data = df, rho = 0)

# Histology
fit7 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(HISTREC), data = breast)

plot(fit7, col = c(1:6, 8:9), lty = 1:8, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Histology')

legend("bottomleft", c("1", "2",
                       "5",  "6",
                       "8",  "9",
                       "11",  "22"),
       bty = "n", lty = 1:8, col = c(1:6, 8:9), cex=0.6)
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(HISTREC), data = breast, rho = 0)

# race
fit8 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(RAC_RECA), data = breast)

plot(fit8, col = c(1:3), lty = 1:3, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Race')

legend("bottomleft", c("White", "Black", "Other"),
       bty = "n", lty = 1:3, col = c(1:3))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(RAC_RECA), data = breast, rho = 0)

# ER status
fit9 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(ERSTATUS), data = breast)

plot(fit9, col = c(1:2), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby ER Status')

legend("bottomleft", c("Positive", "Negative"),
       bty = "n", lty = 1:2, col = c(1:2))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(ERSTATUS), data = breast, rho = 0)

# PR status
fit10 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(PRSTATUS), data = breast)

plot(fit10, col = c(1:2), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby PR Status')

legend("bottomleft", c("Positive", "Negative"),
       bty = "n", lty = 1:2, col = c(1:2))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(PRSTATUS), data = breast, rho = 0)

# Stage
fit11 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Stage), data = breast)

plot(fit11, col = c(1:6, 8:9), lty = 1:8, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Stage')

legend("bottomleft", c("I", "IIA",
                       "IIB",  "III",
                       "IIIA",  "IIIB",
                       "IIIC",  "IV"),
       bty = "n", lty = 1:8, col = c(1:6, 8:9), cex=0.6)
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Stage), data = breast, rho = 0)

# tumor marker
fit12 <- survfit(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Tumor_marker), data = breast)

plot(fit12, col = c(1:2), lty = 1:2, 
     ylab = 'Estimated Survival', xlab = 'Time', main = 'Kaplan-Meier Estimates for Breast cancer patients \nby Tumor marker')

legend("bottomleft", c("Positive", "Negative"),
       bty = "n", lty = 1:2, col = c(1:2))
survdiff(Surv(SRV_TIME_MON, VSRTSADX) ~ as.factor(Tumor_marker), data = breast, rho = 0)


#### fit cox model-----------------------------------------------------
fit <- coxph(Surv(SRV_TIME_MON, VSRTSADX) ~ Dx_age + Sex + Lateral + ERSTATUS + PRSTATUS + Tumor_marker + 
               Marital_Married + Marital_Sep + Marital_Widow + One_Primary + Grade2 + Grade3 + Grade4 + Surg + Hist2 +
               Hist5 + Hist6 + Hist8 + Hist9 + Hist11 + Hist22 + White + Black + 
               StageIIA + StageIIB + StageIII + StageIIIA + StageIIIB + StageIIIC + StageIV, 
             data = df, x=TRUE, y=TRUE)
summary(fit)
fit_aic <- stepAIC(fit, direction = "both")
summary(fit_aic)
fitB <- coxph(Surv(SRV_TIME_MON, VSRTSADX) ~ Dx_age + ERSTATUS + PRSTATUS + 
                Marital_Married + Marital_Sep + Marital_Widow + One_Primary + Grade2 + Grade3 + Grade4 + Surg + Hist2 +
                Hist5 + Hist6 + Hist8 + Hist9 + Hist11 + Hist22 + White + Black + 
                StageIIA + StageIIB + StageIII + StageIIIA + StageIIIB + StageIIIC + StageIV, 
              data = df, x=TRUE, y=TRUE)
summary(fitB)
# R.squared and L.squared of Cox PH model
pam.coxph(fit)
pam.coxph(fitB)
# Cox-snell
mg.residual <- resid(fit, type = "martingale")
cs.residual <- df$VSRTSADX - mg.residual
fit.cs <- survfit(Surv(cs.residual, df$VSRTSADX) ~ 1) #Get Kaplan-Meier estiamtes
H.cs   <- cumsum(fit.cs$n.event/fit.cs$n.risk)
plot(fit.cs$time, H.cs, type='s', col='blue', 
     main = 'Cox-Snell Residual Plot \n for original cox model', 
     xlab = 'Residual', ylab = 'Nelson-Aalen Cum. Hazard') 
abline(0, 1, col='red',  lty = 2)

# AFT------------------------------
fit.lognormal <- survreg(Surv(SRV_TIME_MON, VSRTSADX) ~ Dx_age + Sex + Lateral + ERSTATUS + PRSTATUS + Tumor_marker + 
                         Marital_Married + Marital_Sep + Marital_Widow + One_Primary + Grade2 + Grade3 + Grade4 + Surg + Hist2 +
                         Hist5 + Hist6 + Hist8 + Hist9 + Hist11 + Hist22 + White + Black + 
                         StageIIA + StageIIB + StageIII + StageIIIA + StageIIIB + StageIIIC + StageIV, 
                       data = df, dist="lognormal",x=TRUE,y=TRUE)
summary(fit.lognormal)
# Cox-snell 3
eta    <- -fit.lognormal$linear.predictors / fit.lognormal$scale
r.ln   <- -log(1 - pnorm((log(df$SRV_TIME_MON) - fit.lognormal$linear.predictors) / fit.lognormal$scale))

fit3   <- survfit(Surv(r.ln, df$VSRTSADX) ~ 1)
H.ln  <- cumsum(fit3$n.event/fit3$n.risk)

plot(H.ln ~ fit3$time, type = 'l',
     main = 'Cox-Snell Residual Plot for \n Log-Normal Regression',
     ylab = 'Estimated Cumulative Hazard', xlab = 'Cox-Snell Residual')
abline(0, 1, col='red',  lty = 2)


fit.weibull <- survreg(Surv(SRV_TIME_MON, VSRTSADX) ~ Dx_age + Sex + Lateral + ERSTATUS + PRSTATUS + Tumor_marker + 
                           Marital_Married + Marital_Sep + Marital_Widow + One_Primary + Grade2 + Grade3 + Grade4 + Surg + Hist2 +
                           Hist5 + Hist6 + Hist8 + Hist9 + Hist11 + Hist22 + White + Black + 
                           StageIIA + StageIIB + StageIII + StageIIIA + StageIIIB + StageIIIC + StageIV, 
                         data = df, dist="weibull",x=TRUE,y=TRUE)
summary(fit.weibull)
sigma  <- fit.weibull$scale
alpha  <- 1 / sigma
eta    <- -fit.weibull$linear.predictors / sigma

r.wb <- df$SRV_TIME_MON^alpha * exp(eta)

fit4   <- survfit(Surv(r.wb, df$VSRTSADX) ~ 1)
H.wb  <- cumsum(fit4$n.event/fit4$n.risk)

plot(H.wb ~ fit4$time, type = 'l', main = 'Cox-Snell Residual Plot for \n Weibull Regression',
     ylab = 'Estimated Cumulative Hazard', xlab = 'Cox-Snell Residual')
abline(0, 1, col='red',  lty=2)


fit.exponential <- survreg(Surv(SRV_TIME_MON, VSRTSADX) ~ Dx_age + Sex + Lateral + ERSTATUS + PRSTATUS + Tumor_marker + 
                           Marital_Married + Marital_Sep + Marital_Widow + One_Primary + Grade2 + Grade3 + Grade4 + Surg + Hist2 +
                           Hist5 + Hist6 + Hist8 + Hist9 + Hist11 + Hist22 + White + Black + 
                           StageIIA + StageIIB + StageIII + StageIIIA + StageIIIB + StageIIIC + StageIV, 
                         data = df, dist="exponential",x=TRUE,y=TRUE)
summary(fit.exponential)

sigma <- fit.exponential$scale
eta   <- -fit.exponential$linear.predictors/sigma

r.exp <- df$SRV_TIME_MON * exp(eta)

fit5   <- survfit(Surv(r.exp, df$VSRTSADX) ~ 1)
H.exp <- cumsum(fit5$n.event / fit5$n.risk)

plot(H.exp ~ fit5$time, type = 'l', 
     main = 'Cox-Snell Residual Plot for \n Exponential Regression',
     ylab = 'Estimated Cumulative Hazard', xlab = 'Cox-Snell Residual')
abline(0, 1, col='red',  lty=2)

# fit_aic2 <- stepAIC(fit.lognormal, direction = "both")
# R.squared and L.squared of lognormal model
pam.survreg(fit.exponential)
pam.survreg(fit.weibull)
pam.survreg(fit.lognormal)

