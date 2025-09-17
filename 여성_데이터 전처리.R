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
sf <- subset(s, SEX == "F")




sf$total_score <- rowSums(sf[, c("INT_SP_OU_1", "INT_SP_OU_2", "INT_SP_OU_3", "INT_SP_OU_4", 
                                 "INT_SP_OU_5", "INT_SP_OU_6", "INT_SP_OU_7", "INT_SP_OU_8", 
                                 "INT_SP_OU_9", "INT_SP_OU_10")], na.rm = TRUE)
sf$SP<-ifelse(sf$total_score<23, 0, 1)

sf %>% rename(ctype=CTYPE)



sf$GRADE<-as.factor(sf$GRADE)

sf<-sf %>% mutate(e_s_rcrd=ifelse(E_S_RCRD==1|E_S_RCRD==2, 1,
                                  ifelse(E_S_RCRD==3, 2,
                                         ifelse(E_S_RCRD==4|E_S_RCRD==5, 3, NA))))
sf$e_s_rcrd<-as.factor(sf$e_s_rcrd)

sf<-sf %>% mutate(e_ses=ifelse(E_SES==1|E_SES==2, 1,
                               ifelse(E_SES==3, 2,
                                      ifelse(E_SES==4|E_SES==5, 3, NA))))
sf$e_ses<-as.factor(sf$e_ses)

sf<-sf %>% mutate(e_res=ifelse(E_RES>=2, 2, 1))
sf$e_res<-as.factor(sf$e_res)

sf<-sf %>% mutate(e_aid=ifelse(E_AID>=2, 1, 0))
sf$e_aid<-as.factor(sf$e_aid)


sf <- sf %>%
  mutate(PR_HT_num = as.numeric(as.character(PR_HT)),  # 문자열 → 숫자
         pr_ht = case_when(
           PR_HT_num %in% c(1, 2) ~ 1,
           PR_HT_num >= 3 ~ 0,
           TRUE ~ NA_real_
         ))
sf$pr_ht <- as.factor(sf$pr_ht)

sf<-sf %>% mutate(pr_bi=ifelse(PR_BI==1|PR_BI==2, 1,
                               ifelse(PR_BI==3, 2,
                                      ifelse(PR_BI==4|PR_BI==5, 3, NA))))
sf$PR_BI<-as.factor(sf$PR_BI)

sf <- sf %>%
  mutate(PR_HD_num = as.numeric(as.character(PR_HD)),  # factor → numeric
         pr_hd = case_when(
           PR_HD_num %in% c(1, 2) ~ 1,
           PR_HD_num >= 3 ~ 0,
           TRUE ~ NA_real_
         ))
sf$pr_hd<-as.factor(sf$pr_hd)

bmi_quartiles <- quantile(sf$BMI, probs = c(0.05, 0.85, 0.95), na.rm = TRUE)
sf <- sf %>%
  mutate(BMI = WT / (HT/100)^2,
         BMI_group = case_when(
           BMI >= bmi_quartiles[3] ~ 1,                      # 비만 (상위 5%)
           BMI >= bmi_quartiles[2] & BMI < bmi_quartiles[3] ~ 2,  # 과체중 (85~95%)
           BMI >= bmi_quartiles[1] & BMI < bmi_quartiles[2] ~ 3,  # 정상 (5~85%)
           BMI < bmi_quartiles[1] ~ 4                         # 저체중 (하위 5%)
         ))


sf<-sf %>% mutate(wc_mn=ifelse(WC_MN==1, 1,
                               ifelse(WC_MN==1|WC_MN==4, 2,
                                      ifelse(WC_MN==3, 3, NA))))
sf$wc_mn<-as.factor(sf$wc_mn)

sf<-sf %>% mutate(f_br=ifelse(F_BR>=6, 1, 0))
sf$f_br<-as.factor(sf$f_br)

sf <- sf %>% mutate(f_fru = ifelse(F_FRUIT >= 2, 2, 0))
sf$f_fru <- as.factor(sf$f_fru)

sf<-sf %>% mutate(f_ff=ifelse(F_FASTFOOD==1|F_FASTFOOD==2, 0, 1))
sf$f_ff<-as.factor(sf$f_ff)

sf<-sf %>% mutate(f_drink=ifelse(F_SWD_A==1|F_SWD_A==2, 0, 1))
sf$f_drink<-as.factor(sf$f_drink)

sf<-sf %>% mutate(f_edu=ifelse(F_EDU>=2, 1, 0))
sf$f_edu<-as.factor(sf$f_edu)

sf<-sf %>% mutate(f_wat=ifelse(F_WAT==5, 1, 0))
sf$f_wat<-as.factor(sf$f_wat)

sf<-sf %>% mutate(pa_tot=ifelse(PA_TOT>=6, 1, 0))
sf$pa_tot<-as.factor(sf$pa_tot)

sf<-sf %>% mutate(pa_vig=ifelse(PA_VIG_D>=4, 1, 0))
sf$pa_vig<-as.factor(sf$pa_vig)

sf<-sf %>% mutate(pa_msc=ifelse(PA_MSC>=4, 1, 0))
sf$pa_msc<-as.factor(sf$pa_msc)

sf<-sf %>% mutate(o_br_fq=ifelse(O_BR_FQ>=4, 1, 0))
sf$o_br_fq<-as.factor(sf$o_br_fq)

sf<-sf %>% mutate(o_br_s=ifelse(O_BR_S>=3, 0, 1))
sf$pa_tot<-as.factor(sf$pa_tot)

sf<-sf %>% mutate(o_slnt=ifelse(O_SLNT==2, 1, 0))
sf$pa_tot<-as.factor(sf$pa_tot)


hw_variables <- c("HW_SPML_S", "HW_SPRM_S", "HW_SPML_H", "HW_SPRM_H", "HW_SPGO_H")
sf$hw <- ifelse(rowMeans(sf[, hw_variables], na.rm = TRUE) <= 2, 0, 1)


sf<-sf %>% mutate(hw_edu=ifelse(HW_EDU==2, 1, 0))
sf$hw_edu<-as.factor(sf$hw_edu)

sf<-sf %>% mutate(as_dg_lt=ifelse(AS_DG_LT==2, 1, 0))
sf$as_dg_lt<-as.factor(sf$as_dg_lt)

sf<-sf %>% mutate(rh_dg_lt=ifelse(RH_DG_LT==2, 1, 0))
sf$rh_dg_lt<-as.factor(sf$rh_dg_lt)

sf<-sf %>% mutate(ecz_dg_lt=ifelse(ECZ_DG_LT==2, 1, 0))
sf$ecz_dg_lt<-as.factor(sf$ecz_dg_lt)

sf<-sf %>% mutate(v_trt=ifelse(V_TRT>=2, 1, 0))
sf$v_trt<-as.factor(sf$v_trt)

sf<-sf %>% mutate(ac_lt=ifelse(AC_LT>=2, 1, 0))
sf$ac_lt<-as.factor(sf$ac_lt)

sf<-sf %>% mutate(ac_days=ifelse(AC_DAYS==1 | AC_DAYS==9999, 0, 1))
sf$ac_days<-as.factor(sf$ac_days)

sf<-sf %>% mutate(tc_lt=ifelse(TC_LT>=2, 1, 0))
sf$tc_lt<-as.factor(sf$tc_lt)

sf<-sf %>% mutate(tc_days=ifelse(TC_DAYS==1 | TC_DAYS==9999, 0, 1))
sf$tc_days<-as.factor(sf$tc_days)

sf<-sf %>% mutate(s_si=ifelse(S_SI>=2, 1, 0))
sf$s_si<-as.factor(sf$s_si)

sf<-sf %>% mutate(s_edu=ifelse(S_EDU==2, 1, 0))
sf$s_edu<-as.factor(sf$s_edu)

sf<-sf %>% mutate(dr_hab_pur=ifelse(DR_HAB_PUR==2, 1, 0))
sf$dr_hab_pur<-as.factor(sf$dr_hab_pur)

sf <- sf %>%
  mutate(sp_t = (INT_SPWD_TM + INT_SPWK_TM) / 7)
q <- quantile(sf$sp_t, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
sf <- sf %>%
  mutate(sp_t_group = case_when(
    sp_t >= q[3] ~ 1,  # 상
    sp_t >= q[2] ~ 2,  # 중상
    sp_t >= q[1] ~ 3,  # 중하
    TRUE ~ 4           # 하
  ))



sf<-sf %>% mutate(m_slp_en=ifelse(M_SLP_EN>=3, 0, 1))
sf$m_slp_en<-as.factor(sf$m_slp_en)

sf<-sf %>% mutate(m_str=ifelse(M_STR>=3, 0, 1))
sf$m_str<-as.factor(sf$m_str)

sf<-sf %>% mutate(m_lon=ifelse(M_LON>=3, 0, 1))
sf$m_lon<-as.factor(sf$m_lon)

sf<-sf %>% mutate(m_sad=ifelse(M_SAD==1, 0, 1))
sf$m_sad<-as.factor(sf$m_sad)

sf<-sf %>% mutate(m_sui_con=ifelse(M_SUI_CON==1, 0, 1))
sf$m_sui_con<-as.factor(sf$m_sui_con)

sf<-sf %>% mutate(m_sui_pln=ifelse(M_SUI_PLN==1, 0, 1))
sf$m_sui_pln<-as.factor(sf$m_sui_pln)

sf<-sf %>% mutate(m_sui_att=ifelse(M_SUI_ATT==1, 0, 1))
sf$m_sui_att<-as.factor(sf$m_sui_att)

gad_variables <- c("M_GAD_1", "M_GAD_2", "M_GAD_3", "M_GAD_4", "M_GAD_5", "M_GAD_6", "M_GAD_7")
sf[, gad_variables] <- apply(sf[, gad_variables], 2, function(x) {
  ifelse(x >= 1 & x <= 4, x - 1, x)  # 기존 값이 1~4이면 0~3으로 변환
})
summary(sf[, gad_variables])
sf$GAD_total <- rowSums(sf[, gad_variables], na.rm = TRUE)
sf$GAD <- ifelse(sf$GAD_total >= 10, 1, 0)


sf <- sf %>% filter(!is.na(GRADE))
sf <- sf %>% filter(!is.na(SEX))
sf <- sf %>% filter(!is.na(e_s_rcrd))
sf <- sf %>% filter(!is.na(e_ses))
sf <- sf %>% filter(!is.na(e_res))
sf <- sf %>% filter(!is.na(e_aid))
sf <- sf %>% filter(!is.na(pr_ht))
sf <- sf %>% filter(!is.na(pr_bi))
sf <- sf %>% filter(!is.na(pr_hd))
sf <- sf %>% filter(!is.na(wc_mn))
sf <- sf %>% filter(!is.na(f_br))
sf <- sf %>% filter(!is.na(f_fru))
sf <- sf %>% filter(!is.na(f_drink))
sf <- sf %>% filter(!is.na(f_ff))
sf <- sf %>% filter(!is.na(f_edu))
sf <- sf %>% filter(!is.na(f_wat))
sf <- sf %>% filter(!is.na(pa_tot))
sf <- sf %>% filter(!is.na(pa_vig))
sf <- sf %>% filter(!is.na(pa_msc))
sf <- sf %>% filter(!is.na(o_br_fq))
sf <- sf %>% filter(!is.na(o_br_s))
sf <- sf %>% filter(!is.na(o_slnt))
sf <- sf %>% filter(!is.na(hw))
sf <- sf %>% filter(!is.na(hw_edu))
sf <- sf %>% filter(!is.na(as_dg_lt))
sf <- sf %>% filter(!is.na(rh_dg_lt))
sf <- sf %>% filter(!is.na(ecz_dg_lt))
sf <- sf %>% filter(!is.na(v_trt))
sf <- sf %>% filter(!is.na(ac_lt))
sf <- sf %>% filter(!is.na(tc_lt))
sf <- sf %>% filter(!is.na(tc_days))
sf <- sf %>% filter(!is.na(s_si))
sf <- sf %>% filter(!is.na(s_edu))
sf <- sf %>% filter(!is.na(dr_hab_pur))
sf <- sf %>% filter(!is.na(SP))
sf <- sf %>% filter(!is.na(m_slp_en))
sf <- sf %>% filter(!is.na(m_str))
sf <- sf %>% filter(!is.na(m_sad))
sf <- sf %>% filter(!is.na(GAD))
sf <- sf %>% filter(!is.na(m_sui_con))
sf <- sf %>% filter(!is.na(m_sui_pln))
sf <- sf %>% filter(!is.na(m_sui_att))

table(sf$SP)


write.csv(sf, "all_f.csv")














select <- dplyr::select
d2_frame <- glm(SP ~ GRADE+e_s_rcrd+e_ses+e_res+e_aid+
                  pr_ht+pr_bi+pr_hd+BMI+wc_mn+f_br+f_fru+f_drink+f_ff+f_edu+f_wat+
                  pa_tot+pa_vig+pa_msc+o_br_fq+o_br_s+o_slnt+hw+hw_edu+as_dg_lt+rh_dg_lt+
                  ecz_dg_lt+v_trt+ac_lt+ac_days+tc_lt+tc_days+s_si+s_edu+dr_hab_pur+sp_t+
                  m_slp_en+m_str+m_lon+GAD+m_sui_con+m_sui_pln+m_sui_att, 
                data = sf, family = "binomial")


d2_stepwise <- step(d2_frame, direction = "both")
summary(d2_stepwise)
d2_stepwise <- stepAIC(d2_frame, direction = "both")













# --- 1. 필요한 라이브러리 로드 (필요시) ---
# library(MASS) # step 함수는 기본 stats 패키지에 있지만, 명시적으로 로드할 수도 있습니다.

# --- 2. 원본 데이터프레임 정의 (사용자 데이터프레임 이름으로 변경) ---
# 이 예제에서는 사용자님의 데이터프레임 이름을 'sm'으로 가정합니다.
# sm <- read.csv("your_data_file.csv") # 예시: CSV 파일에서 데이터 로드

# --- 3. step 함수에 사용될 모든 변수 목록 정의 ---
# 종속 변수
dependent_var <- "SP"

# 초기 모델에 포함되었던 독립 변수들 (step 출력의 첫 번째 모델 공식 기준)
initial_independent_vars <- c(
  "GRADE", "e_s_rcrd", "e_res", "e_aid", "pr_ht", "pr_hd", "BMI", 
  "wc_mn", "f_br", "f_fru", "f_drink", "f_ff", "f_edu", "f_wat", "pa_tot", 
  "pa_msc", "o_br_fq", "o_slnt", "hw", "hw_edu", "as_dg_lt", "rh_dg_lt", 
  "ecz_dg_lt", "v_trt", "tc_lt", "tc_days", "s_si", "s_edu", "dr_hab_pur", 
  "sp_t", "m_slp_en", "m_str", "m_lon", "GAD", "m_sui_con", "m_sui_pln", 
  "m_sui_att"
)

# step 함수가 추가로 고려했던 변수들 (step 출력에서 '+'로 표시된 변수들)
additional_vars_to_consider <- c(
  "o_br_s", "e_ses", "ac_days", "pr_bi", "pa_vig", "ac_lt"
)

# step 함수에 사용될 모든 변수 목록 (종속변수 + 모든 독립변수 후보)
all_vars_for_stepwise <- unique(c(dependent_var, initial_independent_vars, additional_vars_to_consider))

# --- 4. 결측치 제거된 데이터 프레임 생성 ---
# 원본 데이터 'sm'에서 NA가 있는 행 제거
if (exists("sf") && is.data.frame(sf)) {
  # sm 데이터프레임에 all_vars_for_stepwise에 있는 모든 변수가 있는지 확인
  missing_cols <- setdiff(all_vars_for_stepwise, names(sf))
  if (length(missing_cols) > 0) {
    stop(paste("다음 변수들이 'sf' 데이터프레임에 없습니다:", paste(missing_cols, collapse=", ")))
  }
  
  sf_complete <- sf[complete.cases(sf[, all_vars_for_stepwise]), ]
  
  # 결측치 제거 후 데이터 행 수 확인
  print(paste("원본 데이터 행 수:", nrow(sf)))
  print(paste("결측치 제거 후 데이터 행 수 (sf_complete):", nrow(sf_complete)))
  
  if (nrow(sf_complete) == 0) {
    stop("모든 변수를 기준으로 결측치를 제거한 후 남은 데이터가 없습니다. 변수 목록이나 데이터를 확인해주세요.")
  }
} else {
  stop("'sf'이라는 이름의 데이터프레임이 없거나 R 환경에 로드되지 않았습니다. 데이터를 먼저 로드해주세요.")
}

# --- 5. sm_complete에서 단일 수준 변수 확인 및 모델 공식 조정 ---
# 초기 모델 공식 문자열 생성
formula_str <- paste(dependent_var, "~", paste(initial_independent_vars, collapse = " + "))

# f_fru 변수가 sm_complete 내에서 단일 수준인지 확인
if ("f_fru" %in% names(sf_complete)) {
  if (length(unique(sf_complete$f_fru)) < 2) {
    print("주의: 'f_fru' 변수가 sm_complete 데이터에서 단일 수준(또는 NA만 존재)을 가집니다. 모델 공식에서 제외합니다.")
    # f_fru를 공식에서 제거 (앞에 + 기호와 공백까지 함께 제거)
    formula_str <- gsub("\\+\\s*f_fru\\b", "", formula_str)
    formula_str <- gsub("f_fru\\s*\\+", "", formula_str) # 맨 앞에 있을 경우
    formula_str <- gsub("\\bf_fru\\b", "", formula_str) # 단독으로 있을 경우 (이 경우는 거의 없음)
    # 정리: 혹시 " +  + " 같은 공백이 생겼을 수 있으므로 정리
    formula_str <- gsub("\\+\\s*\\+", "+", formula_str)
    formula_str <- gsub("\\s*\\+\\s*$", "", formula_str) # 끝에 +가 남는 경우
  }
}

# (선택 사항) 다른 변수들도 단일 수준인지 확인하고 싶다면 반복문 사용
# for (var_name in initial_independent_vars) {
#   if (var_name %in% names(sm_complete)) { # 해당 변수가 sm_complete에 있는지 먼저 확인
#     if (length(unique(na.omit(sm_complete[[var_name]]))) < 2) { # na.omit 추가하여 NA만 있는 경우도 처리
#       print(paste("주의:", var_name, "변수가 sm_complete 데이터에서 단일 수준입니다. 모델 공식에서 제외합니다."))
#       formula_str <- gsub(paste0("\\+\\s*", var_name, "\\b"), "", formula_str)
#       formula_str <- gsub(paste0(var_name, "\\s*\\+"), "", formula_str)
#       formula_str <- gsub(paste0("\\b", var_name, "\\b"), "", formula_str)
#       formula_str <- gsub("\\+\\s*\\+", "+", formula_str)
#       formula_str <- gsub("\\s*\\+\\s*$", "", formula_str)
#     }
#   }
# }

print(paste("사용될 모델 공식:", formula_str))

# --- 6. 정제된 데이터로 초기 GLM 모델 다시 적합 ---
# formula_str이 비어있지 않은지 확인 (모든 변수가 제거된 경우 방지)
if (!grepl("~\\s*\\S+", formula_str)) {
  stop("모델 공식에 독립 변수가 남아있지 않습니다. 변수 목록 및 데이터 확인이 필요합니다.")
}

d2_frame_complete <- glm(as.formula(formula_str), 
                         data = sf_complete, 
                         family = "binomial")

print("초기 모델 (d2_frame_complete) 요약:")
print(summary(d2_frame_complete))

# --- 7. 새로운 모델로 step 함수 실행 ---
# scope를 정의하여 step 함수가 고려할 변수의 범위를 명확히 할 수 있습니다.
# lower_formula <- as.formula(paste(dependent_var, "~ 1")) # 최소 모델 (절편만)
# upper_formula <- as.formula(paste(dependent_var, "~", paste(setdiff(all_vars_for_stepwise, c(dependent_var, "f_fru_ problematic_if_any")), collapse=" + ")))
# 위 upper_formula는 f_fru 같은 단일 수준 변수가 있다면 미리 제거해줘야 함.
# 간단하게는 d1_frame_complete 모델의 변수들로부터 시작하게 할 수 있음.

print("단계별 변수 선택을 시작합니다...")
# trace=1로 설정하면 각 단계를 보여줍니다. 원치 않으면 trace=0 또는 trace=FALSE.
step_model2 <- step(d2_frame_complete, direction = "both", trace = 1)

# --- 8. 최종 선택된 모델 확인 ---
print("최종 선택된 모델 요약:")
print(summary(step_model2))

print("최종 모델 공식:")
print(formula(step_model2))
