#' WITCH default meta_param information. Useful if they are missing in the input files.
#'
#'
#' @export
#' @examples
#' \dontrun{
#' witch_default_meta_param()
#' }
witch_default_meta_param <- function() {
"parameter,value
I,sum
K,sum
Q,sum
BAU_Q,sum
COST_EN,mean
COST_PES,mean
I_EN,sum
K_EN,sum
MCOST_INV,mean
MCOST_PES,mean
Q_EN,sum
Q_IN,sum
Q_PES,sum
SHARE_EL,mean
COST_EMI,mean
CUM_EMI,sum
Q_EMI,sum
BAU_Q_EMI,sum
I_RD,sum
K_RD,sum
K_RD_F,sum
SPILL,mean
ABAT,sum
Q_WBIO,sum
Q_REDD,sum
MCOST_EMI,mean
I_EN_WINDOFF,sum
I_EN_WINDON,sum
K_EN_WINDOFF,sum
K_EN_WINDON,sum
Q_EN_WINDOFF,sum
Q_EN_WINDON,sum
I_EN_PV,sum
I_EN_CSP,sum
K_EN_PV,sum
K_EN_CSP,sum
Q_EN_PV,sum
Q_EN_CSP,sum
Q_EL_FLEX,sum
K_EN_GRID,sum
I_EN_GRID,sum
ADDOILCAP,sum
COST_OIL,mean
CUM_OIL,sum
I_OIL,sum
I_OUT,sum
OILCAP,sum
OILPROD,sum
Q_EMI_OUT,sum
Q_OUT,sum
RF,max
TEMP,max
TRF,max
W_EMI,max
WCUM_EMI,max
OMEGA,max
QEL_EDV,sum
QEL_FR_EDV,sum
emi_cap,sum
ken_policy,sum
ren_share,mean
temp_valid_hadcrut4,mean
" -> defmap
  dm <- fread(defmap)
  dm[, type := "nagg"]
  dm = rbind(dm,
             data.table(
               parameter = dm$parameter,
               type = "nweight",
               value = "gdp"
             ))
  setcolorder(dm, c("parameter", "type", "value"))
  return(dm)
}
