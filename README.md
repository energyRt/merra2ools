
# merra2ools

(UNDER CONSTRUCTION)

## Overview

*merra2ools* package offers a set of tools to evaluate potential hourly
output of solar and wind energy sources based on MERRA-2 data subset
(forthcoming).

The MERRA-2 subset includes:  
- **locid** - location IDs, an index of locations in MERRA-2 dataset,
from 1 to 207936;  
- **UTC** - date and time in UTC timezone;  
- **T10M** - 10-meter air temperature (K);  
- **W10M** - 10-meter wind speed (calculated sqrt(V10M^2 + U10M^2) where
V10M and U10M are northward and eastward wind at 10-meter, m/s);  
- **W50M** - 50-meter wind speed (calculated sqrt(V50M^2 + U50M^2) where
V50M and U50M are northward and eastward wind at 50-meter, m/s);  
- **PS\_hPa** - surface pressure (hPa);  
- **SWGDN** - Incident shortwave land (W/m^2);  
- **ALBEDO** - surface albedo (index \[0,1\]).

All variables are hourly averages, UTC-time is given for a middle of
every hour.

The package reproduces basic algorithms of solar geometry, irradiance
decomposition, and the Plane-Of-Array models for different types of
trackers for evaluation.

<img src="images/poa_fl_40y_avr_24steps.png" width="2550" />

<img src="images/wind_50m_40y_avr_24steps.png" width="3150" />
