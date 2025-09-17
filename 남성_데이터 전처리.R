options("install.lock"=FALSE)
library(haven)
library(descr)
library(gmodels)
library(dplyr)
library(psych)
setwd("C:\\Users\\123cl\\OneDrive\\바탕 화면\\kyrbs2023_sas")

d<-read_sas("kyrbs2023.sas7bdat")

d$SEX<-ifelse(d$SEX==1, "M", "F") #m=26769, f=26111
s <- subset(d, !is.na(SEX)) 
sm <- subset(s, SEX == "M")




sm$total_score <- rowSums(sm[, c("INT_SP_OU_1", "INT_SP_OU_2", "INT_SP_OU_3", "INT_SP_OU_4", 
                                   "INT_SP_OU_5", "INT_SP_OU_6", "INT_SP_OU_7", "INT_SP_OU_8", 
                                   "INT_SP_OU_9", "INT_SP_OU_10")], na.rm = TRUE)
sm$SP<-ifelse(sm$total_score<23, 0, 1)




sm$GRADE<-as.factor(sm$GRADE)

sm<-sm %>% mutate(e_s_rcrd=ifelse(E_S_RCRD==1|E_S_RCRD==2, 1,
                                 ifelse(E_S_RCRD==3, 2,
                                        ifelse(E_S_RCRD==4|E_S_RCRD==5, 3, NA))))
sm$e_s_rcrd<-as.factor(sm$e_s_rcrd)

sm<-sm %>% mutate(e_ses=ifelse(E_SES==1|E_SES==2, 1,
                                  ifelse(E_SES==3, 2,
                                         ifelse(E_SES==4|E_SES==5, 3, NA))))
sm$e_ses<-as.factor(sm$e_ses)

sm<-sm %>% mutate(e_res=ifelse(E_RES>=2, 2, 1))
sm$e_res<-as.factor(sm$e_res)

sm<-sm %>% mutate(e_aid=ifelse(E_AID>=2, 2, 1))
sm$e_aid<-as.factor(sm$e_aid)

sm <- sm %>%
  mutate(E_EDU_F_num = as.numeric(as.character(E_EDU_F)),  # factor → numeric
         e_ecu_f = ifelse(E_EDU_F_num >= 4, 4, E_EDU_F_num))
sm$e_ecu_f<-as.factor(sm$e_ecu_f)

sm <- sm %>%
  mutate(E_EDU_M_num = as.numeric(as.character(E_EDU_M)),  # factor → numeric
         e_ecu_m = ifelse(E_EDU_M_num >= 4, 4, E_EDU_M_num))
sm$e_ecu_m<-as.factor(sm$e_ecu_m)

sm <- sm %>%
  mutate(E_KRN_F = as.numeric(as.character(E_KRN_F)),           # 숫자로 변환
         E_KRN_F = ifelse(E_KRN_F > 9, 3, E_KRN_F),             # 값 수정
         E_KRN_F = factor(E_KRN_F)) %>%                         # 팩터 변환
  rename(e_krn_f = E_KRN_F)                                     # 변수명 변경


sm <- sm %>%
  mutate(E_KRN_M = as.numeric(as.character(E_KRN_M)),           # 숫자로 변환
         E_KRN_M = ifelse(E_KRN_M > 9, 3, E_KRN_M),             # 값 수정
         E_KRN_M = factor(E_KRN_M)) %>%                         # 팩터 변환
  rename(e_krn_M = E_KRN_M)                                     # 변수명 변경



sm <- sm %>%
  mutate(PR_HT_num = as.numeric(as.character(PR_HT)),  # 문자열 → 숫자
         pr_ht = case_when(
           PR_HT_num %in% c(1, 2) ~ 2,
           PR_HT_num >= 3 ~ 1,
           TRUE ~ NA_real_
         ))
sm$pr_ht <- as.factor(sm$pr_ht)

sm<-sm %>% mutate(pr_bi=ifelse(PR_BI==1|PR_BI==2, 1,
                               ifelse(PR_BI==3, 2,
                                      ifelse(PR_BI==4|PR_BI==5, 3, NA))))
sm$PR_BI<-as.factor(sm$PR_BI)

sm <- sm %>%
  mutate(PR_HD_num = as.numeric(as.character(PR_HD)),  # factor → numeric
         pr_hd = case_when(
           PR_HD_num %in% c(1, 2) ~ 2,
           PR_HD_num >= 3 ~ 1,
           TRUE ~ NA_real_
         ))
sm$pr_hd<-as.factor(sm$pr_hd)

bmi_quartiles <- quantile(sm$BMI, probs = c(0.05, 0.85, 0.95), na.rm = TRUE)
sm <- sm %>%
  mutate(BMI = WT / (HT/100)^2,
         BMI_group = case_when(
           BMI >= bmi_quartiles[3] ~ 1,                      # 비만 (상위 5%)
           BMI >= bmi_quartiles[2] & BMI < bmi_quartiles[3] ~ 2,  # 과체중 (85~95%)
           BMI >= bmi_quartiles[1] & BMI < bmi_quartiles[2] ~ 3,  # 정상 (5~85%)
           BMI < bmi_quartiles[1] ~ 4                         # 저체중 (하위 5%)
         ))


sm<-sm %>% mutate(wc_mn=ifelse(WC_MN==1, 1,
                               ifelse(WC_MN==1|WC_MN==4, 2,
                                      ifelse(WC_MN==3, 3, NA))))
sm$wc_mn<-as.factor(sm$wc_mn)

sm<-sm %>% mutate(f_br=ifelse(F_BR>=6, 2, 1))
sm$f_br<-as.factor(sm$f_br)

sm <- sm %>% mutate(f_fru = ifelse(F_FRUIT >= 1, 2, 1))
sm$f_fru <- as.factor(sm$f_fru)

sm<-sm %>% mutate(f_ff=ifelse(F_FASTFOOD==1|F_FASTFOOD==2, 1, 2))
sm$f_ff<-as.factor(sm$f_ff)

sm<-sm %>% mutate(f_drink=ifelse(F_SWD_A==1|F_SWD_A==2,1, 2))
sm$f_drink<-as.factor(sm$f_drink)

sm<-sm %>% mutate(f_edu=ifelse(F_EDU>=2, 2, 1))
sm$f_edu<-as.factor(sm$f_edu)

sm<-sm %>% mutate(f_wat=ifelse(F_WAT==5, 2, 1))
sm$f_wat<-as.factor(sm$f_wat)

sm<-sm %>% mutate(pa_tot=ifelse(PA_TOT>=6, 2, 1))
sm$pa_tot<-as.factor(sm$pa_tot)

sm<-sm %>% mutate(pa_vig=ifelse(PA_VIG_D>=4,  2, 1))
sm$pa_vig<-as.factor(sm$pa_vig)

sm<-sm %>% mutate(pa_msc=ifelse(PA_MSC>=4, 2, 1))
sm$pa_msc<-as.factor(sm$pa_msc)

sm<-sm %>% mutate(o_br_fq=ifelse(O_BR_FQ>=4, 2, 1))
sm$o_br_fq<-as.factor(sm$o_br_fq)

sm<-sm %>% mutate(o_br_s=ifelse(O_BR_S>=3, 2, 1))
sm$pa_tot<-as.factor(sm$pa_tot)

sm<-sm %>% mutate(o_slnt=ifelse(O_SLNT==2, 2, 1))
sm$pa_tot<-as.factor(sm$pa_tot)


hw_variables <- c("HW_SPML_S", "HW_SPRM_S", "HW_SPML_H", "HW_SPRM_H", "HW_SPGO_H")
sm$hw <- ifelse(rowMeans(sm[, hw_variables], na.rm = TRUE) <= 2, 0, 1)


sm<-sm %>% mutate(hw_edu=ifelse(HW_EDU==2, 2, 1))
sm$hw_edu<-as.factor(sm$hw_edu)

sm<-sm %>% mutate(as_dg_lt=ifelse(AS_DG_LT==2, 2, 1))
sm$as_dg_lt<-as.factor(sm$as_dg_lt)

sm<-sm %>% mutate(rh_dg_lt=ifelse(RH_DG_LT==2, 2, 1))
sm$rh_dg_lt<-as.factor(sm$rh_dg_lt)

sm<-sm %>% mutate(ecz_dg_lt=ifelse(ECZ_DG_LT==2,  2, 1))
sm$ecz_dg_lt<-as.factor(sm$ecz_dg_lt)

sm<-sm %>% mutate(v_trt=ifelse(V_TRT>=2, 2, 1))
sm$v_trt<-as.factor(sm$v_trt)

sm<-sm %>% mutate(ac_lt=ifelse(AC_LT>=2, 2, 1))
sm$ac_lt<-as.factor(sm$ac_lt)

sm<-sm %>% mutate(ac_days=ifelse(AC_DAYS==1 | AC_DAYS==9999, 1, 2))
sm$ac_days<-as.factor(sm$ac_days)

sm<-sm %>% mutate(tc_lt=ifelse(TC_LT>=2,  2, 1))
sm$tc_lt<-as.factor(sm$tc_lt)

sm<-sm %>% mutate(tc_days=ifelse(TC_DAYS==1 | TC_DAYS==9999, 1, 2))
sm$tc_days<-as.factor(sm$tc_days)

sm<-sm %>% mutate(s_si=ifelse(S_SI>=2,  2, 1))
sm$s_si<-as.factor(sm$s_si)

sm<-sm %>% mutate(s_edu=ifelse(S_EDU==2, 2, 1))
sm$s_edu<-as.factor(sm$s_edu)

sm<-sm %>% mutate(dr_hab_pur=ifelse(DR_HAB_PUR==2, 2, 1))
sm$dr_hab_pur<-as.factor(sm$dr_hab_pur)

sm <- sm %>%
  mutate(sp_t = (INT_SPWD_TM + INT_SPWK_TM) / 7)
q <- quantile(sm$sp_t, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
sm <- sm %>%
  mutate(sp_t_group = case_when(
    sp_t >= q[3] ~ 1,  # 상
    sp_t >= q[2] ~ 2,  # 중상
    sp_t >= q[1] ~ 3,  # 중하
    TRUE ~ 4           # 하
  ))



sm<-sm %>% mutate(m_slp_en=ifelse(M_SLP_EN>=3, 1, 2))
sm$m_slp_en<-as.factor(sm$m_slp_en)

sm<-sm %>% mutate(m_str=ifelse(M_STR>=3, 1, 2))
sm$m_str<-as.factor(sm$m_str)

sm<-sm %>% mutate(m_lon=ifelse(M_LON>=3, 1, 2))
sm$m_lon<-as.factor(sm$m_lon)

sm<-sm %>% mutate(m_sad=ifelse(M_SAD==1, 1, 2))
sm$m_sad<-as.factor(sm$m_sad)

sm<-sm %>% mutate(m_sui_con=ifelse(M_SUI_CON==1, 1, 2))
sm$m_sui_con<-as.factor(sm$m_sui_con)

sm<-sm %>% mutate(m_sui_pln=ifelse(M_SUI_PLN==1, 1, 2))
sm$m_sui_pln<-as.factor(sm$m_sui_pln)

sm<-sm %>% mutate(m_sui_att=ifelse(M_SUI_ATT==1, 1, 2))
sm$m_sui_att<-as.factor(sm$m_sui_att)

gad_variables <- c("M_GAD_1", "M_GAD_2", "M_GAD_3", "M_GAD_4", "M_GAD_5", "M_GAD_6", "M_GAD_7")
sm[, gad_variables] <- apply(sm[, gad_variables], 2, function(x) {
  ifelse(x >= 1 & x <= 4, x - 1, x)  # 기존 값이 1~4이면 0~3으로 변환
})
summary(sm[, gad_variables])
sm$GAD_total <- rowSums(sm[, gad_variables], na.rm = TRUE)
sm$GAD <- ifelse(sm$GAD_total >= 10, 1, 0)



sm<- sm %>% filter(!is.na(GRADE))
sm <- sm %>% filter(!is.na(SEX))
sm <- sm %>%filter(!is.na(e_s_rcrd))
sm<- sm %>% filter(!is.na(e_ses))
sm <- sm %>% filter(!is.na(e_res))
sm <- sm %>% filter(!is.na(e_aid))
sm <- sm %>%filter(!is.na(pr_ht))
sm<- sm %>% filter(!is.na(pr_bi))
sm <- sm %>% filter(!is.na(pr_hd))
sm <- sm %>% filter(!is.na(wc_mn))
sm <- sm %>%filter(!is.na(f_br))
sm<- sm %>% filter(!is.na(f_fru))
sm <- sm %>% filter(!is.na(f_drink))
sm <- sm %>% filter(!is.na(f_ff))
sm <- sm %>%filter(!is.na(f_edu))
sm<- sm %>% filter(!is.na(f_wat))
sm <- sm %>% filter(!is.na(pa_tot))
sm <- sm %>% filter(!is.na(pa_vig))
sm <- sm %>%filter(!is.na(pa_msc))
sm <- sm %>% filter(!is.na(o_br_fq))
sm <- sm %>%filter(!is.na(o_br_s))
sm<- sm %>% filter(!is.na(o_slnt))
sm <- sm %>% filter(!is.na(hw))
sm <- sm %>% filter(!is.na(hw_edu))
sm <- sm %>%filter(!is.na(as_dg_lt))
sm<- sm %>% filter(!is.na(rh_dg_lt))
sm <- sm %>% filter(!is.na(ecz_dg_lt))
sm <- sm %>% filter(!is.na(v_trt))
sm <- sm %>% filter(!is.na(ac_lt))
sm <- sm %>% filter(!is.na(tc_lt))
sm <- sm %>% filter(!is.na(tc_days))
sm <- sm %>% filter(!is.na(s_si))
sm <- sm %>% filter(!is.na(s_edu))
sm <- sm %>% filter(!is.na(dr_hab_pur))
sm <- sm %>% filter(!is.na(SP))
sm <- sm %>% filter(!is.na(m_slp_en))
sm <- sm %>% filter(!is.na(m_str))
sm <- sm %>% filter(!is.na(m_sad))
sm <- sm %>% filter(!is.na(GAD))
sm <- sm %>% filter(!is.na(m_sui_con))
sm <- sm %>% filter(!is.na(m_sui_pln))
sm <- sm %>% filter(!is.na(m_sui_att))

table(sm$SP)

write.csv(sm, "all_m.csv")




select <- dplyr::select
d1_frame <- glm(SP ~ GRADE+e_s_rcrd+e_ses+e_res+e_aid+
                   pr_ht+pr_bi+pr_hd+BMI+wc_mn+f_br+f_drink+f_ff+f_edu+f_wat+
                   pa_tot+pa_vig+pa_msc+o_br_fq+o_br_s+o_slnt+hw+hw_edu+as_dg_lt+rh_dg_lt+
                   ecz_dg_lt+v_trt+ac_lt+ac_days+tc_lt+tc_days+s_si+s_edu+dr_hab_pur+sp_t+
                   m_slp_en+m_str+m_lon+GAD+m_sui_con+m_sui_pln+m_sui_att, 
                 data = sm, family = "binomial")

summary(d1_frame)


#backward
d1_back<-step(d1_frame, direction = "backward")
summary(d1_back) 
d1_backward <- stepAIC(d1_frame, direction = "backward")

#forward
sm4_for<-step(sm4_1, direction = "forward")
summary(sm4_for) #제거 X
sm4_forward <- stepAIC(sm4_1, direction = "forward")

#단계적 선택
d1_stepwise <- step(d1_frame, direction = "both")
summary(d1_stepwise) 
d1_stepwise <- stepAIC(d1_frame, direction = "both")




sm2 <- na.omit(sm)  # 결측치가 있는 행 전체 제거

# 새 데이터(sm2)로 모델 적합
d1_frame <- glm(SP ~ GRADE + e_s_rcrd + e_ses + e_res + e_aid +
                  pr_ht + pr_bi + pr_hd + BMI + wc_mn + f_br + f_drink + f_ff + f_edu + f_wat +
                  pa_tot + pa_vig + pa_msc + o_br_fq + o_br_s + o_slnt + hw + hw_edu + as_dg_lt + rh_dg_lt +
                  ecz_dg_lt + v_trt + ac_lt + ac_days + tc_lt + tc_days + s_si + s_edu + dr_hab_pur + sp_t +
                  m_slp_en + m_str + m_lon + GAD + m_sui_con + m_sui_pln + m_sui_att,
                data = sm2, family = "binomial")
one_level_factors2 <- sapply(sm2, function(x) is.factor(x) && length(levels(x)) == 1)
names(sm2)[one_level_factors2]





sm3 <- sm2[, !(names(sm2) %in% "f_fru")]   # f_fru 변수 완전 삭제
# 또는
sm3 <- subset(sm2, select = -f_fru)
d1_frame <- glm(SP ~ GRADE+e_s_rcrd+e_ses+e_res+e_aid+
                  pr_ht+pr_bi+pr_hd+BMI+wc_mn+f_br+f_drink+f_ff+f_edu+f_wat+
                  pa_tot+pa_vig+pa_msc+o_br_fq+o_br_s+o_slnt+hw+hw_edu+as_dg_lt+rh_dg_lt+
                  ecz_dg_lt+v_trt+ac_lt+ac_days+tc_lt+tc_days+s_si+s_edu+dr_hab_pur+sp_t+
                  m_slp_en+m_str+m_lon+GAD+m_sui_con+m_sui_pln+m_sui_att, data = sm3, family = "binomial")

