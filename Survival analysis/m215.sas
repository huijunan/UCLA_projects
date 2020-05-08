*Creata a library named T1;
libname T1 "D:\";

filename in "D:\BREAST.txt";

*Import data;                                                                                   
data T1.data1;                                                                              
  infile in lrecl=32767 firstobs=1 lrecl=32767;                                                             
  input                                                                               
    @ 1   pt_id                $char8.  /* Patient ID */                              
    @ 9   rg_id                $char10. /* SEER registry */                           
    @ 19  Marital_Status       $char1.  /* Marital status at diagnosis */             
    @ 20  Race                 $char2.  /* Race/ethnicity */                           
    @ 24  SEX                  $char1.  /* Sex */                                     
    @ 25  Dx_age               $char3.  /* Age at diagnosis */                        
    @ 28  Birth_yr             $char4.  /* Year of birth */                           
    @ 35  SEQ_NUM              $char2.  /* Sequence number */                         
    @ 37  Dx_month             $char2.  /* Month of diagnosis */                      
    @ 39  Dx_year              $char4.  /* Year of diagnosis */                       
    @ 43  Primary_site         $char4.  /* Primary site ICD-O-2 (1973+) */            
    @ 47  Lateral              $char1.  /* Laterality */                              
    @ 48  Hist_O2              $char4.  /* Histologic Type ICD-O-2 */                 
    @ 52  Behavior_O2          $char1.  /* Behavior Code ICD-O-2*/                    
    @ 53  Hist_O3              $char4.  /* Histologic Type ICD-O-3 */                 
    @ 57  Behavior_O3          $char1.  /* Behavior code ICD-O-3 */                   
    @ 58  GRADE                $char1.  /* Grade */                                   
    @ 59  DX_CONF              $char1.  /* Diagnostic confirmation */                 
    @ 60  REPT_SRC             $char1.  /* Type of reporting source */                
    @ 61  EOD_sz               $char3.  /* EOD 10 - size (1988+) */                   
    @ 64  EOD_ex               $char2.  /* EOD 10 - extension */                      
    @ 66  EOD_pe               $char2.  /* EOD 10 - path extension */                 
    @ 68  EOD_nd               $char1.  /* EOD 10 - lymph node */                     
    @ 69  EOD_pn               $char2.  /* EOD 10 - positive lymph nodes examined */  
    @ 71  EOD_ne               $char2.  /* EOD 10 - number of lymph nodes examined */ 
    @ 73  EOD13                $char13. /* EOD--old 13 digit */                       
    @ 86  EOD2                 $char2.  /* EOD--old 2 digit */                        
    @ 88  EOD4                 $char4.  /* EOD--old 4 digit */                        
    @ 92  EOD_CODE             $char1.  /* Coding system for EOD */                   
    @ 93  TUMOR_1V             $char1.  /* Tumor marker 1 */                          
    @ 94  TUMOR_2V             $char1.  /* Tumor marker 2 */                          
    @ 95  TUMOR_3V             $char1.  /* Tumor marker 3 */                          
    @ 96  CSTUMORSZ            $char3.  /* CS Tumor size */                           
    @ 99  CSEXTEN              $char3.  /* CS Extension */                            
    @ 102 CSLYMPHN             $char3.  /* CS Lymph Nodes */                          
    @ 105 CSMETSDX             $char2.  /* CS Mets at DX */                           
    @ 107 CS_1site             $char3.  /* CS Site-Specific Factor 1 */               
    @ 110 CS_2site             $char3.  /* CS Site-Specific Factor 2 */               
    @ 113 CS_3site             $char3.  /* CS Site-Specific Factor 3 */               
    @ 116 CS_4site             $char3.  /* CS Site-Specific Factor 4 */               
    @ 119 CS_5site             $char3.  /* CS Site-Specific Factor 5 */               
    @ 122 CS_6site             $char3.  /* CS Site-Specific Factor 6 */               
    @ 125 CS_25site            $char3.  /* CS Site-Specific Factor 25 */              
    @ 128 DAJCCT               $char2.  /* Derived AJCC T */                          
    @ 130 DAJCCN               $char2.  /* Derived AJCC N */                          
    @ 132 DAJCCM               $char2.  /* Derived AJCC M */                          
    @ 134 DAJCCSTG             $char2.  /* Derived AJCC Stage Group */                
    @ 136 DSS1977S             $char1.  /* Derived SS1977 */                          
    @ 137 SCSSM2KO             $char1.  /* SEER Combined Summary Stage 2000 (2004+) */                     
    @ 141 CSVFIRST             $char6.  /* CS Version Input Original */               
    @ 147 CSVLATES             $char6.  /* CS Version Derived */                      
    @ 153 CSVCURRENT           $char6.  /* CS Version Input Current */                
    @ 159 SURGPRIF             $char2.  /* RX Summ--surg prim site */                 
    @ 161 SURGSCOF             $char1.  /* RX Summ--scope reg LN sur 2003+*/          
    @ 162 SURGSITF             $char1.  /* RX Summ--surg oth reg/dis */               
    @ 163 NUMNODES             $char2.  /* Number of lymph nodes */                   
    @ 166 NO_SURG              $char1.  /* Reason no cancer-directed surgery */       
    @ 170 SS_SURG              $char2.  /* Site specific surgery (1983-1997) */       
    @ 174 SURGSCOP             $char1.  /* Scope of lymph node surgery 98-02*/        
    @ 175 SURGSITE             $char1.  /* Surgery to other sites */                  
    @ 176 RECNOREC               $char2.  /* Record number */                           
    @ 191 TYPE_FU              $char1.  /* Type of followup expected */               
    @ 192 AGE_1REC             $char2.  /* Age recode <1 year olds */                 
    @ 199 SITERWHO             $char5.  /* Site recode ICD-O-3/WHO 2008 */            
    @ 204 ICDOTO9V             $char4.  /* Recode ICD-O-2 to 9 */                     
    @ 208 ICDOT10V             $char4.  /* Recode ICD-O-2 to 10 */                    
    @ 218 ICCC3WHO             $char3.  /* ICCC site recode ICD-O-3/WHO 2008 */       
    @ 221 ICCC3XWHO            $char3.  /* ICCC site rec extended ICD-O-3/ WHO 2008*/ 
    @ 224 BEHTREND             $char1.  /* Behavior recode for analysis */            
    @ 226 HISTREC              $char2.  /* Broad Histology recode */                  
    @ 228 HISTRECB             $char2.  /* Brain recode */                            
    @ 230 CS0204SCHEMA         $char3.  /* CS Schema v0204*/                          
    @ 233 RAC_RECA             $char1.  /* Race recode A */                           
    @ 234 RAC_RECY             $char1.  /* Race recode Y */                           
    @ 235 ORIGRECB             $char1.  /* Origin Recode NHIA */                      
    @ 236 HST_STGA             $char1.  /* SEER historic stage A */                   
    @ 237 AJCC_STG             $char2.  /* AJCC stage 3rd edition (1988+) */          
    @ 239 AJ_3SEER             $char2.  /* SEER modified AJCC stage 3rd ed (1988+) */ 
    @ 241 SSS77VZ              $char1.  /* SEER Summary Stage 1977 (1995-2000) */     
    @ 242 SSSM2KPZ             $char1.  /* SEER Summary Stage 2000 2000 (2001-2003) */
    @ 245 FIRSTPRM             $char1.  /* First malignant primary indicator */       
    @ 246 ST_CNTY              $char5.  /* State-county recode */                     
    @ 255 CODPUB               $char5.  /* Cause of death to SEER site recode */      
    @ 260 CODPUBKM             $char5.  /* COD to site rec KM */                      
    @ 265 STAT_REC             $char1.  /* Vital status recode (study cutoff used) */ 
    @ 266 IHSLINK              $char1.  /* IHS link */                                
    @ 267 SUMM2K               $char1.  /* Historic SSG 2000 Stage */                 
    @ 268 AYASITERWHO          $char2.  /* AYA site recode/WHO 2008 */                
    @ 270 LYMSUBRWHO           $char2.  /* Lymphoma subtype recode/WHO 2008 */        
    @ 272 VSRTSADX             $char1.  /* SEER cause of death classification */      
    @ 273 ODTHCLASS            $char1.  /* SEER other cause of death classification */
    @ 274 CSTSEVAL             $char1.  /* CS EXT/Size Eval */                        
    @ 275 CSRGEVAL             $char1.  /* CS Nodes Eval */                           
    @ 276 CSMTEVAL             $char1.  /* CS Mets Eval */                            
    @ 277 INTPRIM              $char1.  /* Primary by International Rules */          
    @ 278 ERSTATUS             $char1.  /* ER Status Recode Breast Cancer (1990+)*/   
    @ 279 PRSTATUS             $char1.  /* PR Status Recode Breast Cancer (1990+)*/   
    @ 280 CSSCHEMA             $char2.  /* CS Schema - AJCC 6th Edition */            
    @ 282 CS8SITE              $char3.  /* Cs Site-specific Factor 8 */               
    @ 285 CS10SITE             $char3.  /* CS Site-Specific Factor 10*/               
    @ 288 CS11SITE             $char3.  /* CS Site-Specific Factor 11*/               
    @ 291 CS13SITE             $char3.  /* CS Site-Specific Factor 13*/               
    @ 294 CS15SITE             $char3.  /* CS Site-Specific Factor 15*/               
    @ 297 CS16SITE             $char3.  /* CS Site-Specific Factor 16*/               
    @ 300 VASINV               $char1.  /* Lymph-vascular Invasion (2004+)*/          
    @ 301 SRV_TIME_MON         $char4.  /* Survival months */                         
    @ 305 SRV_TIME_MON_FLAG    $char1.  /* Survival months flag */                    
    @ 311 INSREC_PUB           $char1.  /* Insurance Recode (2007+) */                
    @ 312 DAJCC7T              $char3.  /* Derived AJCC T 7th ed */                   
    @ 315 DAJCC7N              $char3.  /* Derived AJCC N 7th ed */                   
    @ 318 DAJCC7M              $char3.  /* Derived AJCC M 7th ed */                   
    @ 321 DAJCC7STG            $char3.  /* Derived AJCC 7 Stage Group */              
    @ 324 ADJTM_6VALUE         $char2.  /* Adjusted AJCC 6th T (1988+) */             
    @ 326 ADJNM_6VALUE         $char2.  /* Adjusted AJCC 6th N (1988+) */             
    @ 328 ADJM_6VALUE          $char2.  /* Adjusted AJCC 6th M (1988+) */             
    @ 330 ADJAJCCSTG           $char2.  /* Adjusted AJCC 6th Stage (1988+) */         
    @ 332 CS7SITE              $char3.  /* CS Site-Specific Factor 7 */               
    @ 335 CS9SITE              $char3.  /* CS Site-specific Factor 9 */               
    @ 338 CS12SITE             $char3.  /* CS Site-Specific Factor 12 */              
    @ 341 HER2                 $char1.  /* Derived HER2 Recode (2010+) */             
    @ 342 BRST_SUB             $char1.  /* Breast Subtype (2010+) */                  
    @ 348 ANNARBOR             $char1.  /* Lymphoma - Ann Arbor Stage (1983+) */      
    @ 349 SCMETSDXB_PUB        $char1.  /* SEER Combined Mets at DX-bone (2010+) */
    @ 350 SCMETSDXBR_PUB       $char1.  /* SEER Combined Mets at DX-brain (2010+) */
    @ 351 SCMETSDXLIV_PUB      $char1.  /* SEER Combined Mets at DX-liver (2010+)*/
    @ 352 SCMETSDXLUNG_PUB     $char1.  /* SEER Combined Mets at DX-lung (2010+) */            
    @ 353 T_VALUE              $char2.  /* T value - based on AJCC 3rd (1988-2003) */ 
    @ 355 N_VALUE              $char2.  /* N value - based on AJCC 3rd (1988-2003) */ 
    @ 357 M_VALUE              $char2.  /* M value - based on AJCC 3rd (1988-2003) */ 
    @ 359 MALIGCOUNT           $char2.  /* Total number of in situ/malignant tumors for patient */        
    @ 361 BENBORDCOUNT         $char2.  /* Total number of benign/borderline tumors for patient */   
    @ 364 TUMSIZS              $char3.  /* Tumor Size Summary (2016+) */
    @ 367 DSRPSG               $char5.  /* Derived SEER Cmb Stg Grp (2016+) */
    @ 372 DASRCT               $char5.  /* Derived SEER Combined T (2016+) */
    @ 377 DASRCN               $char5.  /* Derived SEER Combined N (2016+) */
    @ 382 DASRCM               $char5.  /* Derived SEER Combined M (2016+) */
    @ 387 DASRCTS              $char1.  /* Derived SEER Combined T Src (2016+) */
    @ 388 DASRCNS              $char1.  /* Derived SEER Combined N Src (2016+) */
    @ 389 DASRCMS              $char1.  /* Derived SEER Combined M Src (2016+) */
    @ 390 TNMEDNUM             $char2.  /* TNM Edition Number (2016+) */
    @ 392  METSDXLN            $char1.  /* Mets at DX-Distant LN (2016+) */
    @ 393  METSDXO             $char1.  /* Mets at DX-Other (2016+) */     ;        
run;

data T1.data2;
	set T1.data1(where=(Behavior_O3 = "3" and Dx_year > '1988' and rg_id="0000001501"));
	if Tumor_1V='2' and Tumor_2V='2' then Tumor_marker=0;
	else if Tumor_1V="" and Tumor_2V="" then Tumor_marker=.;
	else Tumor_marker=1;
	drop NHIADE;
run;

proc sql;
	create table T1.patient_ids as 
	select pt_id
			,count(1) as n
	from T1.data2
	group by pt_id
	having n=1;
quit;

data T1.data3;
	set T1.data2;
	drop  rg_id Race  RAC_RECY IHSLINK ORIGRECB AGE_1REC EOD_pe 
         EOD2 DAJCCM SITERWHO HISTRECB cs0204schema CSSCHEMA CS8SITE 
         CS10SITE CS11SITE CS13SITE CS16SITE VASINV  CS9SITE  CS12SITE 
         CS_25site  ANNARBOR  Primary_site Hist_O2 Hist_O3 Behavior_O2   
         Behavior_O3 EOD_code  Tumor_1V Tumor_2V Tumor_3V EOD13       
         EOD4 ODTHCLASS STAT_REC NO_SURG  CSVFIRST CSVLATES CSVCURRENT 
         ICDOTO9V ICDOT10V ICCC3WHO ICCC3XWHO BEHTREND ST_CNTY TNMEDNUM  
         AYASITERWHO LYMSUBRWHO CSSCHEMA SURGSCOF SURGSCOP Dx_conf Rept_src
         RECNOREC CODPUBKM     
         EOD_sz EOD_ex EOD_nd EOD_pn EOD_ne CSTUMORSZ CSEXTEN INTPRIM
         CSLYMPHN CSMETSDX CS_1site CS_2site CS_3site CS_4site
         CS_5site CS_6site DAJCCT DAJCCN DAJCCM DAJCCSTG DSS1977S
         SCSSM2KO HST_STGA AJCC_STG AJ_3SEER SSS77VZ SSSM2KPZ
         SUMM2K CSTSEVAL CSRGEVAL CSMTEVAL CS15SITE DAJCC7T
         DAJCC7N DAJCC7M DAJCC7STG CS7SITE CS9SITE CS12SITE T_VALUE
         N_VALUE M_VALUE MALIGCOUNT BENBORDCOUNT TUMSIZS
         DASRCTS DASRCNS DASRCMS ;
run;

proc sort data=T1.data3;
	by pt_id;
run;

proc sort data=T1.patient_ids;
	by pt_id;
run;

data T1.data3_N;
	length ADJTM_6VALUE $16. ADJAJCCSTG $4.;
	merge T1.data3
			T1.patient_ids(in=in1 drop=n);
	by pt_id;
	if in1 and CODPUB ne '41000';
run;

data T1.data3_N;
	set T1.data3_N;
	length Stage $4.;
	if ADJTM_6VALUE="0" then ADJTM_6VALUE="T0";
	else if ADJTM_6VALUE="5" then ADJTM_6VALUE="Tis";
	else if ADJTM_6VALUE in ("11","12","15","18") then ADJTM_6VALUE="T1";
	else if ADJTM_6VALUE="20" then ADJTM_6VALUE="T2";
	else if ADJTM_6VALUE="30" then ADJTM_6VALUE="T3";
	else if ADJTM_6VALUE in ("41","42","43","44") then ADJTM_6VALUE="T4";
	else if ADJTM_6VALUE="60" then ADJTM_6VALUE="Any T, Mets";
	else if ADJTM_6VALUE="88" then ADJTM_6VALUE="NA";
	else if ADJTM_6VALUE="99" then ADJTM_6VALUE="TX";

	if ADJNM_6VALUE="0" then ADJNM_6VALUE="N0";
	else if ADJNM_6VALUE="10" then ADJNM_6VALUE="N1";
	else if ADJNM_6VALUE="20" then ADJNM_6VALUE="N2";
	else if ADJNM_6VALUE="30" then ADJNM_6VALUE="N3";
	else if ADJNM_6VALUE="88" then ADJNM_6VALUE="NA";
	else if ADJNM_6VALUE="99" then ADJNM_6VALUE="NX";

	if ADJM_6VALUE="0" then ADJM_6VALUE="M0";
	else if ADJM_6VALUE="10" then ADJM_6VALUE="M1";
	else if ADJM_6VALUE="88" then ADJM_6VALUE="NA";
	else if ADJM_6VALUE="99" then ADJM_6VALUE="NX";

/*	DSRPSG_n=input(DSRPSG,8.);*/
/*	drop DSRPSG;*/
/*	rename DSRPSG_n=DSRPSG;*/

	if DASRCT="88" then DASRCT="NA";
	else if DASRCT="c0" then DASRCT="T0";
	else if DASRCT in ("c1","p1","c1A","p1A","c1B","p1B","c1C","p1C","T1c","c1MI","p1MI") then DASRCT="T1";
	else if DASRCT in ("c2","p2") then DASRCT="T2";
	else if DASRCT in ("c3","p3") then DASRCT="T3";
	else if DASRCT in ("c4","p4","T4","c4A","p4A","T4a","c4B","p4B","c4C","p4C","c4D","p4D") then DASRCT="T4b";
	else if DASRCT="cx" then DASRCT="TX";
	else if DASRCT="pIS" then DASRCT="Tis";

	if DASRCN="88" then DASRCN="NA";
	else if DASRCN in ("c0","p0","p0I-","p0I+","p0M-","p0M+") then DASRCN="N0";
	else if DASRCN in ("c1","p1","p1A","p1B","p1C","p1MI") then DASRCN="N1";
	else if DASRCN in ("c2","p2","c2A","p2A","c2B") then DASRCN="N2";
	else if DASRCN in ("c3","p3","p3A","c3B","p3B","c3C") then DASRCN="N3";
	else if DASRCN in ("cx","px") then DASRCN="NX";
	
	if DASRCM="88" then DASRCM="NA";
	else if DASRCM="c0" then DASRCM="M0";
	else if DASRCM in ("c1","p1") then DASRCM="M1";

	if ADJAJCCSTG="10" then ADJAJCCSTG="I";
	else if ADJAJCCSTG="32" then ADJAJCCSTG="IIA";
	else if ADJAJCCSTG="33" then ADJAJCCSTG="IIB";
	else if ADJAJCCSTG="51" then ADJAJCCSTG="III";
	else if ADJAJCCSTG="52" then ADJAJCCSTG="IIIA";
	else if ADJAJCCSTG="53" then ADJAJCCSTG="IIIB";
	else if ADJAJCCSTG="54" then ADJAJCCSTG="IIIC";
	else if ADJAJCCSTG="70" then ADJAJCCSTG="IV";
	else if ADJAJCCSTG in ("88","99") then ADJAJCCSTG="NA";

	if DSRPSG="1" then Stage="0";
	else if DSRPSG="2" then Stage="IA";
	else if DSRPSG="3" then Stage="IB";
	else if DSRPSG="4" then Stage="IIA";
	else if DSRPSG="5" then Stage="IIB";
	else if DSRPSG="6" then Stage="III";
	else if DSRPSG="7" then Stage="IIIA";
	else if DSRPSG="8" then Stage="IIIB";
	else if DSRPSG="9" then Stage="IIIC";
	else if DSRPSG="10" then Stage="IV";
	else if DSRPSG in ("11","12") then Stage="NA";
	else Stage=DSRPSG;
	
	if ADJTM_6VALUE ne "" then T_Stage=ADJTM_6VALUE;
	else if DASRCT ne "" then T_Stage=DASRCT;
	else T_Stage="";
	if ADJNM_6VALUE ne "" then N_Stage=ADJNM_6VALUE;
	else if DASRCN ne "" then N_Stage=DASRCN;
	else N_Stage="";
	if ADJM_6VALUE ne "" then M_Stage=ADJM_6VALUE;
	else if DASRCM ne "" then M_Stage=DASRCM;
	else M_Stage="";
	if ADJAJCCSTG ne "" then Stage=ADJAJCCSTG;
	else if Stage ne "" then Stage=Stage;
	else Stage="";
	drop pt_id  ADJTM_6VALUE  ADJNM_6VALUE  ADJM_6VALUE  DASRCT  DASRCN  DASRCM  ADJAJCCSTG  DSRPSG;
run;

data T1.breast;
	length Marital_Status $2. RAC_RECA $2. Grade $2. ERSTATUS $2. PRSTATUS $2. ;
	set T1.data3_N(where=(SRV_TIME_MON_FLAG='1' and Lateral<='2' and VSRTSADX ne '9' and SRV_TIME_MON ne '0' and Stage ne '0'));
	if Marital_Status="9" then Marital_Status="NA";
	if RAC_RECA="9" then RAC_RECA="NA";
	if Grade="9" then Grade="NA";
	if ERSTATUS in ("4","3") then ERSTATUS="NA";
	if PRSTATUS in ("4","3") then PRSTATUS="NA";
	if T_Stage="NA" then T_Stage="";
	if N_Stage="NA" then N_Stage="";
	if M_Stage="NA" then M_Stage="";
	if Marital_Status="NA" then Marital_Status="";
	if RAC_RECA="NA" then RAC_RECA="";
	if ERSTATUS="NA" then ERSTATUS="";
	if PRSTATUS="NA" then PRSTATUS="";
	if Grade="NA" then Grade="";
	if Stage="NA" then Stage="";
	drop Birth_yr  Dx_month  Dx_year  TYPE_FU  FIRSTPRM  SRV_TIME_MON_FLAG  
	    SURGSITF  NUMNODES  SURGSITE  INSREC_PUB  SCMETSDXB_PUB  SCMETSDXBR_PUB  SCMETSDXLIV_PUB  SS_SURG 
	    SCMETSDXLUNG_PUB  METSDXLN  METSDXO  her2 brst_sub;
run;

data T1.breast;
	set T1.breast;
	if cmiss(of _character_)>0 or nmiss(of _numeric_) then delete;
run;

data T1.df;
	set T1.breast;
	if Marital_Status="2" then Marital_Married=1;
	else Marital_Married=0;
	if Marital_Status in ("3","4") then Marital_Sep=1;
	else Marital_Sep=0;
	if Marital_Status="5" then Marital_Widow=1;
	else Marital_Widow=0;
	if Seq_num="0" then One_Primary=1;
	else One_Primary=0;
	if Grade="2" then Grade2=1;
	else Grade2=0;
	if Grade="3" then Grade3=1;
	else Grade3=0;
	if Grade="4" then Grade4=1;
	else Grade4=0;

	if SURGPRIF="0" then Surg=0;
	else Surg=1;
	if HISTREC="2" then Hist2=1;
	else Hist2=0;
	if HISTREC="5" then Hist5=1;
	else Hist5=0;
	if HISTREC="6" then Hist6=1;
	else Hist6=0;
	if HISTREC="8" then Hist8=1;
	else Hist8=0;
	if HISTREC="9" then Hist9=1;
	else Hist9=0;
	if HISTREC="11" then Hist11=1;
	else Hist11=0;
	if HISTREC="22" then Hist22=1;
	else Hist22=0;
	if RAC_RECA="1" then White=1;
	else White=0;

	if RAC_RECA="2" then Black=1;
	else Black=0;
	if ERSTATUS="1" then ERSTATUS=1;
	else ERSTATUS=0;
	if PRSTATUS="6" then PRSTATUS=1;
	else PRSTATUS=0;
	if Stage="IIA" then StageIIA=1;
	else StageIIA=0;
	if Stage="IIB" then StageIIB=1;
	else StageIIB=0;
	if Stage="III" then StageIII=1;
	else StageIII=0;
	if Stage="IIIA" then StageIIIA=1;
	else StageIIIA=0;
	if Stage="IIIB" then StageIIIB=1;
	else StageIIIB=0;
	if Stage="IIIC" then StageIIIC=1;
	else StageIIIC=0;
	if Stage="IV" then StageIV=1;
	else StageIV=0;
	drop  Marital_Status   Seq_num  Grade  SURGPRIF  HISTREC  RAC_RECA  T_Stage 
          N_Stage  M_Stage Stage;
run;
