# Define a list mapping latent variables (constructs) to their indicator variables
constructs <- list(
  ex_passw = c("cch01_score", "cch012_score_r", "cch013_score"),
  ex_phish = c("cch021_cch021_r", "cch022_cch022", "cch023_cch023_r"),
  ex_web = c("cch031_cch031_r", "cch032_cch032_r", "cch033_cch033"),
  
  big5_extra = c("big5_big11", "big5_big12", "big5_big13_r", "big5_big14_r"), 
  big5_agree = c("big5_big21", "big5_big22", "big5_big23_r", "big5_big24_r"),
  big5_consc = c("big5_big31", "big5_big32", "big5_big33_r", "big5_big34_r"), 
  big5_neuro = c("big5_big41", "big5_big42", "big5_big43_r", "big5_big44_r"), 
  big5_imag = c("big5_big51", "big5_big52_r", "big5_big53_r", "big5_big54_r"),
  
  ra = c("ra7_ra1", "ra_ra1_r", "ra_ra2_r", "ra_ra3_r", "ra_ra4", "ra_ra5_r", "ra_ra6"),
  
  fomo_social = c("fomo1_fomo11", "fomo1_fomo12", "fomo1_fomo13", "fomo1_fomo14"),
  fomo_socmed = c("fomo3_fomo31", "fomo3_fomo32", "fomo3_fomo33", "fomo3_fomo34", "fomo3_fomo35"),
  fomo_novel = c("fomo2_fom021", "fomo2_fom022", "fomo2_fom023", "fomo2_fom024", "fomo2_fom025", 
            "fomo2_fom026", "fomo2_fom027"),
  
  csm = c("csb_csm4", "csb_csm6", "csb_csm5", "csb_csm2", "csb_csm1", 
          "csb_csm3", "csb_csm10", "csb_csm7", "csb_csm8", "csb_csm9",
          "csb2_csm11", "csb2_csm12", "csb2_csm13", "csb2_csm14", 
          "csb2_csm15", "csb2_csm16", "csb2_csm17", "csb2_csm18"),
  
  pv = c("pv_pv1", "pv_pv2", "pv_pv3", "pv_pv4"),
  pe = c("pe_pe1", "pe_pe2", "pe_pe3", "pe_pe4", "pe_pe5", "pe_pe6"),
  
  tue = c("tue_tue1", "tue_tue2", "tue_tue3", "tue_tue4", "tue_tue5", "tue_tue6"),
  tud = c("tud_tud1", "tud_tud2", "tud_tud3"),
  
  tr = c("tr_tr1", "tr_tr2", "tr_tr3", "tr_tr4"),
  
  cchk = c("cchk_cch11", "cchk_cch21", "cchk_cch31", "cchk_cch41_r", "cchk_cch51_r", 
           "cchk_cch61_r", "cchk_cch71", "cchk_cch81"),
  
  ccha = c("ccha_cch12_r", "ccha_cch22_r", "ccha_cch32", "ccha_cch42_r", "ccha_cch52_r", 
           "ccha_cch62_r", "ccha_cch72_r", "ccha_cch82_r"),
  
  cchb = c("cchb_cch13", "cchb_cch23_r", "cchb_cch33_r", "cchb_cch43", "cchb_cch53", 
           "cchb_cch63", "cchb_cch73_r", "cchb_cch83"),
  
  si = c("si_si1", "si_si2", "si_si3", "si_si4"),
  
  co = c("co_co1", "co_co2", "co_co3", "co_co4"),
  
  ori = c("ori_ori1", "ori_ori2", "ori_ori3"),
  orh = c("orh_orh1", "orh_orh2", "orh_orh3"),
  
  dem = c("dem1", "dem2", "dem3", "dem4", "dem5", "dem6", "dem7", "dem8", "dem9", 
                   "dem10", "dem11", "dem12", "dem13", "dem14"),
  
  other = c("interviewtime", "group_time_exercise")
)

# Sample Usage
# alpha(survey_min[, constructs$pv])
