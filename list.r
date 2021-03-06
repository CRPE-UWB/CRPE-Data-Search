# Manually Written Lists for The Dropdown Menus
library(shiny)

## Lists ------------------------------------------------------------------
#fipst codes for all states and territories
FIPST <- c(01, 02, 04, 05, 06, 08, 09, 10, 11, 12,
           13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
           25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36,
           37, 38, 39, 40, 41, 42, 44, 45, 46, 47, 48,
           49, 50, 51, 53, 54, 55, 56, 58, 59, 60, 61, 66,
           69, 72, 78)
# Names for the Options OF Dropdown Menu: State
names(FIPST) <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO',
                  'CT', 'DE', 'DC', 'FL', 'GA', 'HI',
                  'ID', 'IL', 'IN', 'IA', 'KS', 'KY',
                  'LA', 'ME', 'MD', 'MA', 'MI', 'MN',
                  'MS', 'MO', 'MT', 'NE', 'NV', 'NH',
                  'NJ', 'NM', 'NY', 'NC', 'ND', 'OH',
                  'OK', 'OR', 'PA', 'RI', 'SC', 'SD',
                  'TN', 'TX', 'UT', 'VT', 'VA', 'WA',
                  'WV', 'WI', 'WY', 'DOD Dependents Schools - overseas', 
                  'Bureau of Indian Education', 'American Samoa', 'DOD dependents - domestic', 
                  'Guam', 'Northern Marinas', 'Puerto Rico', 'Virgin Islands')
DATASETS<-c("Schools", "GradRates", "OCR", "StatePerformance", "StateEnrollment", "Finance")
names(DATASETS)<- c("Schools", "GradRates- edfacts enrollment/ graduation rates", "OCR- office of civil rights math enrollment", 
             "StatePerformance- various state governments", "StateEnrollment- various state governments", "Finance- F33 survey")



STATES <- names(FIPST)
FINANCE <- c("LEAID" 
             ,"FIPST" 
             ,"NAME"
             ,"YEAR"
             ,"FALL_MEMBERSHIP"
             ,"REV_TOTAL"
             ,"FEDREV_TOTAL"
             ,"FEDREV_STATE_TITLEI"
             ,"FEDREV_STATE_IDEA"
             ,"FEDREV_STATE_MATHSCIENCE+TEACHERQUALITY"
             ,"FEDREV_STATE_SAFE+DRUGFREE"
             ,"FEDREV_STATE_VOCATIONAL+TECHED"
             ,"FEDREV_STATE_BILINGUALED"
             ,"FEDREV_STATE_OTHER"
             ,"FEDREV_STATE_CHILDNUTRITIONACT"
             ,"FEDREV_NONSPECIFIED"
             ,"FEDREV_DIRECT_IMPACTAID"
             ,"FEDREV_DIRECT_INDIANED"
             ,"FEDREV_DIRECT_OTHERTOTALSTATEREV"
             ,"STATEREV_TOTAL"
             ,"STATEREV_GENFORMULA+ASSISTANCE"
             ,"STATEREV_STAFFIMPROV_PROGRAMS"
             ,"STATEREV_SPED_PROGRAMS"
             ,"STATEREV_COMPENSATORY+BASIC+SKILLSPROGRAM"
             ,"STATEREV_BILINGUALED"
             ,"STATEREV_GIFTED+TALENTED_PROGRAMS"
             ,"STATEREV_VOCATIONALED_PROGRAMS"
             ,"STATEREV_SCHOOLLUNCH"
             ,"STATEREV_CAPITALOUTLAY+DEBTSERVICES"
             ,"STATEREV_TRANSPORTATION"
             ,"STATEREV_OTHER"
             ,"STATEREV_NONSPECIFIED"
             ,"STATEREV_ONBEHALF_EMPLOYEE+BENEFITS"
             , "STATEREV_ONBEHALF_NOTEMPLOYEE+BENEFITS"
             ,"LOCALREV_TOTAL"
             ,"LOCALREV_PARENTGOVCONRIBUTIONS"
             ,"LOCALREV_PROPERTY_TAXES"
             ,"LOCALREV_GENERALSALES_TAXES"
             ,"LOCALREV_PUBLICUTILITY_TAXES"
             ,"LOCALREV_INDIVIDUAL+CORPORATEINCOME_TAXES"
             ,"LOCALREV_ALLOTHER_TAXES"
             ,"LOCALREV_FROMOTHERSCHOOLSYSTEMS"
             ,"LOCALREV_FROMCITIES+COUNTIES"
             ,"LOCALREV_TUITIONFEESFROMPUPILS+PARENTS"
             ,"LOCALREV_TRANSPORTATIONFEESFROMPUPILS+PARENTS"
             ,"LOCALREV_SCHOOLLUNCH"
             ,"LOCALREV_TEXTBOXSALES+RENTALS"
             ,"LOCALREV_DISTRICTACTIVITY+RECEIPTS"
             ,"LOCALREV_STUDENTFEES+NONSPECIFIED"
             ,"LOCALREV_OTEHRSALES+SERVICES"
             ,"LOCALREV_RENTS+ROYALTIES"
             ,"LOCALREV_SALEOFPROPERTY"
             ,"LOCALREV_INTERESTEARNINGS"
             ,"LOCALREV_FINES+FORFEITS"
             ,"LOCALREV_PRIVATECONTRIBUTIONS"
             ,"LOCALREV_MISC"
             ,"LOCALREV_NCES_CENSUSBUREAUSTATEREV"
             ,"CURRENTEXPEND_TOTAL"
             ,"CURRENTEXPEND_ELEMSECONDEXPEND_TOTAL"
             ,"CURRENTEXPEND_INSTRUCTION_TOTAL"
             ,"CURRENTEXPEND_INSTRUCTION"
             ,"PAYMENTS_TOPRIVATESCH"
             ,"PAYMENTS_TOCHARTERSCH"
             ,"CURRENTEXPEND_SUPPORTSERVICES_TOTAL"
             ,"CURRENTEXPEND_SUPPORTSERVICES_PUPILS"
             ,"CURRENTEXPEND_SUPPORTSERVICES_INSTRUCTIONALSTAFF"
             ,"CURRENTEXPEND_SUPPORTSERVICES_GENERALADMIN"
             ,"CURRENTEXPEND_SUPPORTSERVICES_SCHOOLADMIN"
             ,"CURRENTEXPEND_SUPPORTSERVICES_OP+MAINTENANCEOFPLANT"
             ,"CURRENTEXPEND_SUPPORTSERVICES_STUDENTTRANSPORTATION"
             ,"CURRENTEXPEND_SUPPORTSERVICES_BUSINESS+CENTRAL+OTHER"
             ,"CURRENTEXPEND_SUPPORTSERVICES_NONSPECIFIED"
             ,"CURRENTEXPEND_OTHERELEMSECOND_TOTAL"
             ,"CURRENTEXPEND_FOODSERVICES"
             ,"CURRENTEXPEND_ENTERPRISEOPS"
             ,"CURRENTEXPEND_OTHER"
             , "NONELEMSECONDEXPEND_TOTAL"
             , "NONELEMSECONDEXPEND_COMMUNITYSERVICES"
             ,"NONELEMSECONDEXPEND_ADULTED"
             , "NONELEMSECONDEXPEND_OTHER"
             , "CAPITALOUTLAY_TOTAL"
             ,"CAPITALOUTLAY_CONSTRUCTION"
             ,"CAPITALOUTLAY_LAND+EXISTINGSTRUCTURES"
             , "CAPITALOUTLAY_INSTRUCTIONALEQUIPMENT"
             ,"CAPITALOUTLAY_OTHEREQUIPMENT"
             ,"CAPITALOUTLAY_NONSPECIFEQUIPMENT"
             ,"PAYMENTS_TOSTATEGOV"
             ,"PAYMENTS_TOLOCALGOV"
             , "PAYMENTS_TOOTHERSCHOOLSYSTEMS"
             ,"INTERESTONDEBT"
             ,"SALARIES_TOTAL"
             , "SALARIES_INSTRUCTION"
             ,"SALARIES_TEACHER_REGED"
             , "SALARIES_TEACHER_SPED"
             ,"SALARIES_TEACHER_VOCATIONALED"
             ,"SALARIES_TEACHER_OTHERED"
             , "SALARIES_SUPPORTSERVICES_PUPILS"
             ,"SALARIES_SUPPORTSERVICES_INSTRUCTIONALSTAFF"
             ,"SALARIES_SUPPORTSERVICES_GENERALADMIN"
             ,"SALARIES_SUPPORTSERVICES_SCHOOLADMIN"
             ,"SALARIES_SUPPORTSERVICES_OP+MAINTENANCEOFPLANT"
             , "SALARIES_SUPPORTSERVICES_STUDTRANSPORTATION"
             ,"SALARIES_SUPPORTSERVICES_BUSINESS+CENTRAL_OTHERS"
             , "SALARIES_FOODSERVICES"
             , "EMPBENEFITS_TOTAL"
             ,"EMPBENEFITS_INSTRUCTION"
             ,"EMPBENEFITS_SUPPORTSERVICES_PUPILS"
             ,"EMPBENEFITS_SUPPORTSERVICES_INSTRUCTIONALSTAFF"
             ,"EMPBENEFITS_SUPPORTSERVICES_BUSINESS+CENTRAL_OTHERS"
             ,"EMPBENEFITS_FOODSERVICES"
             ,"EMPBENEFITS_SUPPORTSERVICES_GENERALADMIN"
             ,"EMPBENEFITS_SUPPORTSERVICES_SCHOOLADMIN"
             ,"EMPBENEFITS_SUPPORTSERVICES_OP+MAINTENANCEOFPLANT"
             ,"EMPBENEFITS_SUPPORTSERVICES_STUDTRANSPORTATION"
             , "EMPBENEFITS_ENTERPRISEOPS"
             ,"TEXTBOOKS"
             ,"LONGTERMDEBT_OUTSTANDINGATBEGOFFISCALYR"
             , "LONGTERMDEBT_ISSUEDDURINGFISCALYR"
             , "LONGETERMDEBT_RETIREDDURINGFISCALYR"
             ,"LONGETERMDEBT_OUTSTANDINGATENDOFFISCALYR"
             ,"SHORTTERMDEBT_OUSTANDINGATBEGOFFISCALYR"
             ,"SHORTTERMDEBT_OUSTANDINGATENDOFFISCALYR"
             ,"ASSETS_SINKINGFUND"
             , "ASSETS_BONDFUND"
             ,"ASSETS_OTHERFUNDS"
             ,"ARRA_REV_TITLEI"
             ,"ARRA_CURRENTEXP"
             , "ARRA_CAPITALOUTLAYEXP"
             , "FEDREV_STATE_TITLEVPTA"
             , "FIPSCO" 
             , "CMSA" 
             , "AGCHRT" 
             , "CONUM" 
             , "CSA" 
             , "CBSA" 
             , "CURRENTEXPEND_SUPORTSERVICES_BUSINESS" 
             , "CURRENTEXPEND_SUPPORTSERVICES_CENTRAL"
             , "CURRENTEXPEND_SUPPORTSERVICES_OTHER"
             , "SALARIES_SUPPORTSERVICES_BUSINESS" 
             ,"SALARIES_SUPPORTSERVICES_CENTRAL"
             ,"SALARIES_SUPPORTSERVICES_OTHER"
             , "EMPBENEFITS_SUPPORTSERVICES_BUSINESS"
             , "EMPBENEFITS_SUPPORTSERVICES_CENTRAL"
             , "EMPBENEFITS_SUPPORTSERVICES_OTHER"
             
)


