# CU Football — Transfer Portal Grading Model & App
 
An R Shiny application and statistical grading model built to evaluate and compare the full **2025 transfer portal class for the University of Colorado Football program**. Developed as a final project for the University of Colorado Sports Analytics program.
 
---
 
## Project Overview
 
Evaluating transfer portal players is one of the most important and time-sensitive tasks in modern college football roster management. This project builds a data-driven grading model that scores incoming transfer players based on their production, strength of schedule, and conference context. It also resents the results in an interactive Shiny dashboard.
 
---
 
## Model Details
 
The grading model incorporates:
 
- **Percentile-rank-based composite scoring** — players are scored relative to the pool, not on arbitrary absolute thresholds
- **Strength of Schedule (SOS) adjustment** — production is weighted using **SP+ ratings** to account for the quality of competition faced
- **Conference bonus (`conf_bonus`)** — a multiplier applied based on the competitive level of the player's previous conference
- **Position-specific scoring** — metrics and weights are tailored by position group (QB, RB, WR, TE, EDGE, DL, LB, CB, S, K)
- **NIL market value estimation** — estimates NIL value based on position, star rating, conference, SP+ rank, and production bonuses
---
 
## App Features
 
- **Player leaderboard** — sortable composite scores across the full portal class
- **Radar charts** — multi-dimensional player profiles by position
- **Multi-player comparison tool** — side-by-side evaluation of portal targets
- **Scheme fit analysis** — contextualizes player profiles within CU's system
- **NIL estimator** — market value estimates for portal players
- **CU-branded dark UI** — built for a polished, program-specific presentation
---
 
## Contributors
 
| Name | Role |
|------|------|
| Micah Copple | Grading model development, Shiny app development, dashboard UI |
| Ryan Lynch | Shiny app development, dashboard UI |
| Sean Pearson | Exploratory data analysis, research |
| Cole Dondaville | Exploratory data analysis, research |
 
---
 
## Tech Stack
 
- R / Shiny
- tidyverse, ggplot2, scales
- DT, shinyjs, fmsb, ggrepel
---
 
## Repository Structure
 
```
├── cu_portal_app/
│   └── app.R                  # Final version of the Shiny app
├── TransferPortalApp/
│   ├── app.R                  # Earlier development version
│   └── download.csv           # Public transfer portal data
├── SportsAnalyticsFinal.Rmd   # Full analysis and write-up
├── SportsAnalyticsFinal.html  # Rendered report
└── transfer_stats.rds         # Merged dataset (public sources)
```
 
---
 
## Data
 
`transfer_stats.rds` is a merged dataset constructed from publicly available transfer portal and player statistics sources. No proprietary or private data is included in this repository.
 
---
 
## Running the App
 
1. Open `cu_portal_app/app.R` in RStudio
2. Install required packages if needed:
```r
install.packages(c("shiny", "tidyverse", "DT", "shinyjs", 
                   "scales", "ggrepel", "fmsb"))
```
3. Click **Run App** in RStudio
---
 
## Academic Context
 
This project was completed as a **final project** for the University of Colorado Data Science and Statistics program.
