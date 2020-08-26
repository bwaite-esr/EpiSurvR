#' Read an EpiSurv csv.
#' This works with new and old extracts.
#' Currently only knows the common fields.
#' Disease specific ones you will have to add yourself.
#'
#' @param file your extract
#' @param col_types extra column formats to add. pass as a cols() function
#' @keywords EpiSurv
#' @keywords csv
#' @keywords formatting
#' @export
#' @examples
#' # no examples
#' @import tibble
#' @import dplyr
#' @import readr
#' @import magrittr
#' @import rlang
#' 


read_episurv_csv <- function(file,col_types = NULL,...){

#******************************************************************************
# check what type of csv it is. ####
#******************************************************************************


  encoding <- guess_encoding(file)$encoding

  if(length(intersect(c("UTF-16","UTF-16LE"),encoding)) > 0){
    data <- read.csv(file = file,encoding = "UTF-16",stringsAsFactors = FALSE)
  } else{
    data <- read_csv(file = file,col_types = cols(.default = col_character()))
  }

  data_columns <- colnames(data)

#******************************************************************************
  # levels ####
#******************************************************************************


  ageGrp1_levels <- c(
  "<1",
  "1 to 4",
  "5 to 9",
  "10 to 14",
  "15 to 19",
  "20 to 29",
  "30 to 39",
  "40 to 49",
  "50 to 59",
  "60 to 69",
  "70+",
  "Unknown"
  )

ageGrp2_levels <- c(
  "<1",
  "1 to 4",
  "5 to 14",
  "15 to 24",
  "25 to 44",
  "45 to 64",
  "65+",
  "Unknown"
  )
accuracy_levels <- c(
  "Exact",
  "Nearest",
  "Street",
  "TA",
  "Default Office"
  )

dhb_levels <- tibble::tribble(
  ~levels, ~labels,
  "Northland",  1,
  "Waitemata",  2,
  "Auckland",   3,
  "Counties Manukau",   4,
  "Waikato",  5,
  "Lakes",  6,
  "Bay of Plenty",  7,
  "Tairawhiti",   8,
  "Taranaki",   9,
  "Hawke's Bay", 10,
  "Whanganui", 11,
  "MidCentral",  12,
  "Hutt Valley", 13,
  "Capital and Coast", 14,
  "Wairarapa", 15,
  "Nelson Marlborough",  16,
  "West Coast",  17,
  "Canterbury",  18,
  "South Canterbury",  19,
  "Southern",  22
)

mpameu_levels <- c("Maori", "Pacific Peoples", "Asian", "Middle Eastern/Latin American/African","European or Other", "Unknown")
yes_no_levels <- c("Yes","No","Unknown")

status_levels <- c("Under investigation","Probable","Confirmed","Not a Case")

sex_levels <- c("Male","Female","Indeterminate","Unknown")
dep_levels <- c(1,2,3,4,5,6,7,8,9,10,0)

health_stat_levels <- c("Recovered","Not recovered","Death","Unknown")

#******************************************************************************
# column specifications ####
#******************************************************************************

columns <- cols("#" = col_integer(),
  "accuracy" = col_factor(levels = accuracy_levels,ordered = TRUE),
  "age" = col_character(),
  "ageGrp1" = col_factor(levels = ageGrp1_levels, ordered = TRUE),
  "ageGrp2" = col_factor(levels = ageGrp2_levels, ordered = TRUE),
  "ageInYears" = col_integer(),
  "ageYMD" = col_integer(),
  "aidsCode" = col_character(),
  "cau" = col_integer(),
  "cauName" = col_character(),
  "crf" = col_character(),
  "caseAddress" = col_character(),
  "caseAddressX" = col_character(),
  "caseAddressY" = col_character(),
  "caseGuid" = col_character(),
  "clinicalNotificationComment" = col_character(),
  "clinicalReportDate" = col_character(),
  "clinicalReportingSource" = col_character(),
  "clinicalReportingSourceName" = col_character(),
  "clinicalReportingSourceOrganisation" = col_character(),
  "clinicalReportingSourcePhone" = col_integer(),
  "createdByUser" = col_character(),
  "createdDateTime" = col_date(format = "%d/%m/%Y"),
  "dhb" = col_factor(levels = dhb_levels$levels),
  "dhbNorthSouth" = col_integer(),
  "dateOfBirth" = col_date(format = "%d/%m/%Y"),
  "died" = col_factor(levels = yes_no_levels),
  "diedDt" = col_date(format = "%d/%m/%Y"),
  "diedDtUnknown" = col_factor(levels = yes_no_levels),
  "diedFromDisease" = col_factor(levels = yes_no_levels),
  "diedOther" = col_character(),
  "disRptGrp2" = col_character(),
  "disRptGrp3" = col_character(),
  "disease" = col_character(),
  "diseaseCause" = col_character(),
  "earliestDate" = col_date(format = "%d/%m/%Y"),
  "epiSurvNumber" = col_character(),
  "ethChinese" = col_factor(levels = yes_no_levels),
  "ethCookIslandMaori" = col_factor(levels = yes_no_levels),
  "ethIndian" = col_factor(levels = yes_no_levels),
  "ethMaori" = col_factor(levels = yes_no_levels),
  "ethNZEuropean" = col_factor(levels = yes_no_levels),
  "ethNiuean" = col_factor(levels = yes_no_levels),
  "ethOther" = col_factor(levels = yes_no_levels),
  "ethSamoan" = col_factor(levels = yes_no_levels),
  "ethSpecify1Code" = col_factor(),
  "ethSpecify2Code" = col_factor(),
  "ethTongan" = col_factor(levels = yes_no_levels),
  "ethnicities" = col_character(),
  "ethnicityGrouping" = col_character(),
  "ethnicityPrioritised" = col_character(),
  "ethnicityPrioritisedMPAMEU" = col_factor(levels = mpameu_levels,ordered = TRUE),
  "ethnicityPrioritisedMPAOEU" = col_character(),
  "ethnicityPrioritisedMPOEU" = col_character(),
  "gpAddress" = col_character(),
  "gpAddressX" = col_integer(),
  "gpAddressY" = col_integer(),
  "gpName" = col_character(),
  "gpPhone" = col_character(),
  "gpPractice" = col_character(),
  "givenName" = col_character(),
  "hd" = col_character(), # could be factor
  "hdNorthSouth" = col_character(),
  "hosp" = col_factor(levels = yes_no_levels),
  "hospAddress" = col_character(),
  "hospAddressX" = col_character(),
  "hospAddressY" = col_character(),
  "hospDt" = col_date(format = "%d/%m/%Y"),
  "hospDtUnknown" = col_factor(levels = yes_no_levels),
  "hospName" = col_character(),
  "hospTime" = col_character(),
  "hospTimeUnknown" = col_factor(levels = yes_no_levels),
  "iaidCategory" = col_character(),
  "investigationMethod"= col_character(),
  "investigationPriority"= col_character(),
  "investigationReceived"= col_character(),
  "investigationStatus"= col_character(),
  "localRef"= col_character(),
  "moeiD1" = col_integer(),
  "moeiD2" = col_integer(),
  "mailingAddress" = col_character(),
  "mailingStreet" = col_character(),
  "mailingSuburb" = col_character(),
  "mailingTownCity" = col_character(),
  "meshblock06" = col_integer(),
  "meshblock13" = col_integer(),
  "modifiedByUser" = col_character(),
  "modifiedDateTime" = col_date(format = "%d/%m/%Y"),
  "nhi" = col_character(),
  "nzDep2006" = col_factor(levels = dep_levels,ordered = TRUE),
  "nzDep2013" = col_factor(levels = dep_levels,ordered = TRUE),
  "nzEuro" = col_factor(levels = yes_no_levels),
  "nzMaori" = col_factor(levels = yes_no_levels),
  "nameofCase" = col_character(),
  "occupation" = col_character(),
  "occupationAsCoded" = col_character(),
  "occupationCode" = col_character(), # keep leading 0s
  "officerName" = col_character(),
  "onsetDt" = col_date(format = "%d/%m/%Y"),
  "onsetDtApprox" = col_factor(levels = yes_no_levels),
  "onsetDtUnknown" = col_factor(levels = yes_no_levels),
  "onsetTime" = col_character(), # come back to this one
  "onsetTimeUnknown" = col_factor(levels = yes_no_levels),
  "other" = col_factor(levels = yes_no_levels),
  "otherEthnicitySpecify1" = col_character(),
  "otherEthnicitySpecify2" = col_character(),
  "otherEuro" = col_factor(levels = yes_no_levels),
  "otherSpecify" = col_character(),
  "outbrk" = col_factor(levels = yes_no_levels),
  "outbrkNo" = col_character(),
  "phs" = col_character(),
  "pacificIs" = col_factor(levels = yes_no_levels),
  "phHome" = col_character(),
  "phOther" = col_character(),
  "phWork" = col_character(),
  "placeOfWork1" = col_character(),
  "placeOfWork1Address" = col_character(),
  "placeOfWork1Type" = col_character(),
  "placeOfWork2" = col_character(),
  "placeOfWork2Address" = col_character(),
  "placeOfWork2Type" = col_character(),
  "priorEthnicity" = col_character(),
  "regionalCouncil" = col_character(),
  "reportDate" = col_date(format = "%d/%m/%Y"),
  "reportName" = col_character(),
  "reportOrganisation" = col_character(),
  "reportPhone" = col_character(),
  "reportSrc" = col_character(),
  "rptMth" = col_integer(),
  "rptYear"= col_integer(),
  "sentForInvestigation" = col_character(), # maybe revisit
  "sex" = col_factor(levels = sex_levels),
  "shiftedCaseAddressX" = col_integer(),
  "shiftedCaseAddressY" = col_integer(),
  "status" = col_factor(levels = status_levels),
  "suburb" = col_character(),
  "surname" = col_character(),
  "survWeek" = col_integer(),
  "survWeekEndDate" = col_date(format = "%d/%m/%Y"),
  "ta" = col_character(),
  "taNorthSouth" = col_integer(),
  "tbAsymptomatic" = col_factor(levels = yes_no_levels),
  "unknownEthnicity" = col_factor(levels = yes_no_levels),
  "urbanRuralProfile2006" = col_character(), # could_revisit,
  "urbanRuralProfile2013" = col_character(), # could_revisit,
  "userFieldLabel1" = col_character(),
  "userFieldLabel2" = col_character(),
  "userFieldLabel3" = col_character(),
  "userFieldValue1" = col_character(),
  "userFieldValue2" = col_character(),
  "userFieldValue3" = col_character(),
  "ymd" = col_factor(levels = c("Days","Months","Years"),ordered = TRUE),
   "EpiSurvNumber" = col_character(),
  "Disease" = col_character(),
  "Status" = col_factor(levels = status_levels),
  "ReportDate" = col_date(format = "%d/%m/%Y"),
   "NHI" = col_character(),
   "CaseAddress" = col_character(),
   "Sex" = col_factor(levels = sex_levels),
   "Age" = col_character(),
   "AgeGrp1" = col_factor(levels = ageGrp1_levels,ordered = TRUE),
   "EthnicityPrioritisedMPAMEU" = col_factor(levels = mpameu_levels),
   "DHB" = col_factor(levels = dhb_levels$levels,ordered = TRUE),
  "PHS" = col_character(),
  "Occupation" = col_character(),
  "PlaceOfWork1" = col_character(),
  "Died" = col_factor(levels = yes_no_levels),
  "DiedDt" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "DiedFromDisease" = col_factor(levels = yes_no_levels),
  "EarliestDate" = col_date(format = "%d/%m/%Y"),
  "Hosp" = col_factor(levels = yes_no_levels),
  "HospName" = col_character(),
  "OnsetDt" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "Outbrk" = col_factor(levels = yes_no_levels),
  "OutbrkNo" = col_character(),
  "CreatedDateTime" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "ModifiedDateTime" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "FitClinDes" = col_factor(levels = yes_no_levels),
  "EpiCont" = col_factor(levels = yes_no_levels),
  "EpiContID" = col_character(),
  "ResidCountry" = col_character(),
  "HlthStat" = col_factor(levels = health_stat_levels),
  "ICU" = col_factor(levels = yes_no_levels),
  "ContID1" = col_character(),
  "ContID2" = col_character(),
  "ContID3" = col_character(),
  "ContID4" = col_character(),
  "ContID5" = col_character(),
  "ContLocation" = col_character(),
  "ContProbConf" = col_factor(levels = yes_no_levels),
  "HealthWorker" = col_factor(levels = yes_no_levels),
  "DtArrived" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "LastCountry" = col_character(),
  "LastDtDeparted" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "LastDtEntered" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "Overseas" = col_factor(levels = yes_no_levels),
  "SecCountry" = col_character(),
  "SecDtDeparted" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "SecDtEntered" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "ThirdCountry" = col_character(),
  "ThirdDtDeparted" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "ThirdDtEntered" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "Comments" = col_character(),
  "IsolatedToDate" = col_date(format = "%d/%m/%Y"),
  "Flight1DepDt" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "Flight1No" = col_character(),
  "Flight2DepDt" = col_date(format = "%d/%m/%Y %I:%M:%S %p"),
  "Flight2No" = col_character(),
  .default = col_character()
)

#******************************************************************************
# take appropriate columns and return file ####
#******************************************************************************


columns$cols <- columns$cols[c(intersect(data_columns,names(columns$cols)))]

if(!is.null(col_types)){
columns$cols <- list_merge(columns$cols,!!!col_types$cols)
}

formatted_data <- type_convert(data,col_types = columns)

return(formatted_data)

}
