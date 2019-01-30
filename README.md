# POSH
Compile ACS and HUD data to calculate and map Housing Choice Voucher assisted share of units by tract.

## Picture of Subsidized Households
[Public HUD data](https://www.huduser.gov/portal/datasets/assthsg.html) were compiled for all census tracts with available data in the state of Maryland using a program not included in this repo. The source data were downloaded in excels. A future iteration may query the HUD egis portal API instead. The advantage of downloading and compiling all these data is that it allows us to build a panel. The egis portal only has current statistical attributes.

## Usage
Running CityMap.R will source base_tract.R and base_hcv.R to prepare and reproject the data. Filtering happens a little downstream, before defining functions and mapping. Make sure everything is in the same directory and you have your [Census API key](https://api.census.gov/data/key_signup.html) set as a default option. 
