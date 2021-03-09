# amerifluxr
An R programmatic interface for querying, handling, and summarizing AmeriFlux (https://ameriflux.lbl.gov/) data and metadata.  

## Target user
- Advanced users who use data from many AmeriFlux sites
- Users who handle data from multiple sources, e.g., AmeriFlux, PhenoCam, MODIS...

## A tentative workflow (task list)
- [*] List the sites, with basic site general info     
- [ ] See if and how much data & metadata are available
      - [*] Flux/met data (BASE) year availability (site-year level)
      - [ ] Flux/met data variable availability (variable-year level)
      - [ ] Metadata (BIF) availability (variable-group level)
      - [ ] Metadata availability (variable level 
- [ ] Subset a target site list
- [ ] Download data & metadata
      - BASE & BIF downloads are done separately through AmeriFlux UI (https://ameriflux.lbl.gov/data/download-data/)
      - [ ] Access measurement height data product through R
- [ ] Parse data & metadata
      - [ ] BASE: read data files
            - [ ] parse timestamps
            - [ ] parse variable names/qualifiers 
            - [ ] get variable default units
      - [ ] BIF: read files
            - [ ] parse variable groups 
- [ ] Clean & summarize data
      - [ ] Clean out-range data in BASE, based on physical range limit
- [ ] Do data visualization & any further processing 
      - [ ] Diurnal-seasonal plot
      - Some of this might be done using other packages
        - **REddyProc** handles u*-filtering, gap-filling, partitioning, and certain time-series plots 
        - **FluxnetLSM** converts data into NetCDF, gap-filling
        - **openair** creates windrose, time-series plots
- [ ] List target sites’ contacts & DOI
      - [ ] Parse and subset info from BIF

## working group
- Housen Chu (Berkeley National Lab)
- Koen Hufkens (Ghent University / BlueGreen Labs)
