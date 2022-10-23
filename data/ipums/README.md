# Raw 

To make `ipums_raw.dta`:

1. Go to [](https://usa.ipums.org/usa-action/variables/group)
1. Build a data file with the following variables   
   ```
   Variables
   ---------

	Type Variable    Label
	H    YEAR        Census year
	H    SAMPLE      IPUMS sample identifier
	H    SERIAL      Household serial number
	H    CBSERIAL    Original Census Bureau household serial number
	H    NUMPREC     Number of person records following
	H    HHWT        Household weight
	H    CLUSTER     Household cluster for variance estimation
	H    REGION      Census region and division
	H    STATEFIP    State (FIPS code)
	H    COUNTYFIP   County (FIPS code)
	H    DENSITY     Population-weighted density of PUMA
	H    STRATA      Household strata for variance estimation
	H    GQ          Group quarters status
	H    ROOMS       Number of rooms
	H    CINETHH     Access to internet
	H    CILAPTOP    Laptop, desktop, or notebook computer
	H    CISMRTPHN   Smartphone
	H    CITABLET    Tablet or other portable wireless computer
	H    CIHAND      Handheld computer
	H    CIOTHCOMP   Other computer equipment
	H    CIDATAPLN   Cellular data plan for a smartphone or other mobile device
	H    CIHISPEED   Broadband (high speed) Internet service such as cable,
                     fiber optic, or DSL service
	H    CISAT       Satellite internet service
	H    CIDIAL      Dial-up service
	H    CIOTHSVC    Other internet service
	H    NFAMS       Number of families in household
	P    PERNUM      Person number in sample unit
	P    PERWT       Person weight
	P    SEX         Sex
	P    AGE         Age
	P    RACE        Race [general version]
	P    RACED       Race [detailed version]
	P    HISPAN      Hispanic origin [general version]
	P    HISPAND     Hispanic origin [detailed version]
	P    SCHOOL      School attendance
	P    EDUC        Educational attainment [general version]
	P    EDUCD       Educational attainment [detailed version]
	P    GRADEATT    Grade level attending [general version]
	P    GRADEATTD   Grade level attending [detailed version]
	P    EMPSTAT     Employment status [general version]
	P    EMPSTATD    Employment status [detailed version]
	P    INCTOT      Total personal income
	P    FTOTINC     Total family income
	P    POVERTY     Poverty status
	```
1. Choose the follow sample years

	```
	Sample
	------

	2016 ACS     1.0%
	2017 ACS     1.0%
	2018 ACS     1.0%
	2019 ACS     1.0%

	```
1. Rename the downloaded data file as `ipums_raw.dta`
