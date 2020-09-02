Metadata for Morbey et al. 2017. Evaluation of sex differences in the stopover behavior and post-departure movements of wood-warblers.
Behav Ecol (doi to be determined)

Data files:

Data file 1: morbey_LPBO.csv
Data file 2: morbey_radio.csv
Data file 3: morbey_tailwind.csv
Data file 4: morbey_blood.csv
Date file 5: morbey_diel.csv
Data file 6: morbey_flights.csv

Missing values are coded as NA

#######################################

Data file 1: morbey_LPBO.csv

This file has first capture dates for birds processed through the migration monitoring program at Long Point Bird Observatory.
Documentation about this program can be found here:

LPBO. 2015. Long Point Bird Observatory: Migration Monitoring Protocol. www.bsc-eoc.org/download/LPBOMigration.

Variables and codes:

spcd: 4 letter species code
     AMRE: American Redstart
     BTBW: Black-throated Blue Warbler
     COYE: Common Yellowthroat
     MAWA: Magnolia Warbler
year: year of sampling
dayMay1: day of May when bird was captured and processed for the first time
sex: sex
     M: male
     F: female
age: age group classified by plumage
     SY: second year
     ASY: after second year

 
#######################################

Data file 2: morbey_radio.csv

This file contains information on birds in the radio telemetry study. Birds were released at Old Cut Research Station (42.584 N, 80.397 W).

Variables and codes:

spcd: 4 letter species code
     BTBW: Black-throated Blue Warbler
     MAWA: Magnolia Warbler
band: USFWS band code
id: radio transmitter identification number
year: year of capture
month: month of year (numeric)
day: day of month (numeric)
sex: sex 
     m: male
     f: female
sex_method: method for classifying sex
     mol: molecular genetic
     plum: by plumage following Pyle 1997
age: age group classified by plumage
     SY: second year
     ASY: after second year
wing: flattened wing chord (mm)
tarsus: tarsus length (mm)
wt: weight (g)
fat: amount of fat mass, measured by QMR (g)
lean: amount of lean mass, measured by QMR (g)
mlos: minimum of stay (stopover duration), measured in days as departure day - capture day + 1
depart_night: last night detected at Old Cut, Lower Big Creek, Bird Studies Canada, or Long Point Eco-Adventures (mm/dd/YY)
depart_type: code for the type of departure
     Uninterrupted: left Old Cut with no evidence of stopping elsewhere in array
     Interrupted: left Old Cut with evidence of stopping elsewhere in array
     LPBOTip: left Old Cut with evidence of passing the LPBOTip receiver
     PostRelocation: relocated away from Old Cut before leaving the Old Cut region 
depart_time: departure time from Old Cut for Interrupted, Uninterrupted or LPBOTip depart_types (number of hours after sunset)
array_nights: minimum number of nights when a bird was in array on or after depart_night
last_day: last day detected at Old Cut with evidence of a departure that night (added on 5/16/2019)


#######################################

Data file 3: morbey_tailwind.csv

Extracted weather data from the RNCEP dataset. For documentation, see:

Kanamitsu M, Ebisuzaki W, Woollen J, Yang S-K, Hnilo JJ, Fiorino M, Potter GL. 2002. NCEP-DOE AMIP-II Reanalysis (R-2).
  Bull Am Meteorol Soc. 83:1631-1643.

Kemp MU, Emiel van Loon E, Shamoun-Baranes J, Bouten W. 2012. RNCEP: global weather and climate data at your fingertips. 
  Methods Ecol Evol. 3:65-70. doi: 10.1111/j.2041-210X.2011.00138.x.

Variables and codes:

year: year
day: day of May
tw_925mb: estimated tailwind at 925 mb, interpolated from using the function NCEP.interpol in package RNCEP in R

#######################################

Data file 4: morbey_blood.csv

Data for plasma metabolite study. Birds were captured at Old Cut Research Station (42.584 N, 80.397 W).

Variables and codes:

year: year of capture
month: month of year (numeric)
day: day of month (numeric)
band: USFWS band code
spcd: 4 letter species code
     AMRE: American Redstart
     BTBW: Black-throated Blue Warbler
     COYE: Common Yellowthroat
     MAWA: Magnolia Warbler
id: radio transmitter identification number
sex: sex 
     m: male
     f: female
sex_method: method for classifying sex
     mol: molecular genetic
     plum: by plumage following Pyle 1997
age: age group classified by plumage
     SY: second year
     ASY: after second year
wing: flattened wing chord (mm)
tarsus: tarsus length (mm)
wt: weight (g)
capture_time: capture time of day (proportion of day)
bleed_time: time from capture to bleeding (min)
glyc: glycogen (mmol/L)
trig: triglyceride (mmol/L)
buty: beta hydroxybutyrate (mmol/L)
fat: amount of fat mass, measured by QMR (g)
lean: amount of lean mass, measured by QMR (g)

#######################################

Data file 5: morbey_diel.csv

Data for the analysis of onset and end of diel activity at Old Cut Research Station (42.584 N, 80.397 W).

Variables and codes:

year: year of capture
spcd: 4 letter species code
     BTBW: Black-throated Blue Warbler
     MAWA: Magnolia Warbler
id: radio transmitter identification number
date: date of observation (mm/dd/YY)
onset: first period of diel activity (hh:mm)
civil_twilight: time of civil twilight (hh:mm)
onset_twilight: onset of activity relative to civil_twilight
end: last period of diel activity (hh:mm)
sunset: time of sunset (hh:mm)
end_sunset: end of activity relative to sunset

#######################################

Data file 6: morbey_flights.csv

Data on inter-tower flights after departure from the Old Cut region (within 20 km of 42.485 N, 80.397 W).

spcd: 4 letter species code
     BTBW: Black-throated Blue Warbler
     MAWA: Magnolia Warbler
year: year of capture
id: radio transmitter identification number
sex: sex classified by plumage (BTBW) or genetic marker (MAWA)
     m: male
     f: female
datetime: date and time of detection (mm/dd/YY hh:mm:ss)
x: longitude of tower (decimal degrees)
y: latitude of tower (decimal degrees)
xlag: longitude of previous tower (decimal degrees)
ylag: latitude of previous tower (decimal degrees)
dt: time between towers (s)
dist: distance between towers (km)
ground_speed: travel speed in m/s
uwind: estimated uwind component at 925 mb (m/s)
vwind: estimated vwind component at 925 mb (m/s)
tailwind: estimated tailwind component (or ground_speed) at 925 mb (m/s)
air_speed: air speed (ground_speed-tailwind, m/s)
bearing: bearing of inter-tower movement (degrees, where 0 = N)
