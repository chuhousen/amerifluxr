# amerifluxr
An R programmatic interface for querying, handling, and summarizing AmeriFlux (https://ameriflux.lbl.gov/) data and metadata.  

## Target user
- Advanced users who use data from many AmeriFlux sites
- Users who handle data from multiple sources, e.g., AmeriFlux, PhenoCam, MODIS...

## A tentative workflow (task list)
1 [x] List the sites, with basic site general info     
2 [x] See if and how much data & metadata are available
  - [x] Flux/met data (BASE) year availability (site-year level)
  - [x] Flux/met data variable availability (variable-year level)
  - [x] Metadata (BIF) availability (variable-group level)
  - [x] Metadata availability (variable level 
3 Subset a target site list (no specific func, could be achived using data.table)
4 [x] Download data & metadata
  - BASE & BIF downloads are done separately through AmeriFlux UI (https://ameriflux.lbl.gov/data/download-data/)
  - [x] Access measurement height data product through R
5 [ ] Parse data & metadata
  - [ ] BASE: read data files
    - [ ] parse timestamps
    - [ ] parse variable names/qualifiers 
    - [ ] get variable default units
  - [ ] BIF: read files
    - [ ] parse variable groups 
6 [ ] Clean & summarize data
  - [ ] Clean out-range data in BASE, based on physical range limit
7 [ ] Do data visualization & any further processing 
  - [ ] Diurnal-seasonal plot
  - Some of this might be done using other packages
    - **REddyProc** handles u*-filtering, gap-filling, partitioning, and certain time-series plots 
    - **FluxnetLSM** converts data into NetCDF, gap-filling
    - **openair** creates windrose, time-series plots
8 [ ] List target sites’ contacts & DOI
  - [ ] Parse and subset info from BIF

## working group
- Housen Chu (Berkeley National Lab)
- Koen Hufkens (Ghent University / BlueGreen Labs)
