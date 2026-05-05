transfer_stats <- readRDS("transfer_stats.rds")

library(shiny)
library(tidyverse)
library(DT)
library(scales)
library(ggrepel)
library(fmsb)

# ============================================================
# DATA PREP — uses transfer_stats already in environment
# ============================================================
transfer_stats <- transfer_stats[!duplicated(transfer_stats$full_name), ]

transfer_stats <- transfer_stats %>%
  mutate(
    headshot_url = paste0(
      "https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/",
      athlete_id, ".png&w=96&h=70&cb=1"
    )
  )

# ============================================================
# THEME
# ============================================================
CU_GOLD <- "#CFB87C"
CU_BG   <- "#0d0d0d"
CU_TEXT <- "#dddddd"
CU_GRID <- "#222222"
CU_TEAL <- "#1D9E75"
CU_BLUE <- "#378ADD"

theme_cu <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.background   = element_rect(fill = CU_BG,    color = NA),
      panel.background  = element_rect(fill = CU_BG,    color = NA),
      panel.grid.major  = element_line(color = CU_GRID,  linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      axis.text         = element_text(color = CU_TEXT,  size = 9),
      axis.title        = element_text(color = "#888",   size = 9),
      plot.title        = element_text(color = CU_GOLD,  size = 11, face = "bold", margin = margin(b = 4)),
      plot.subtitle     = element_text(color = "#666",   size = 9,  margin = margin(b = 8)),
      legend.background = element_rect(fill = CU_BG,    color = NA),
      legend.text       = element_text(color = CU_TEXT,  size = 9),
      legend.title      = element_text(color = "#888",   size = 9),
      plot.margin       = margin(10, 14, 10, 10)
    )
}

# ============================================================
# SCHEME WEIGHTS
# ============================================================
scheme_weights <- list(
  QB = list(
    "Air Raid"    = c(pct_passing_ypa=.35, pct_passing_td=.30, pct_passing_pct=.25, pct_int_rate=.10),
    "Spread RPO"  = c(pct_passing_ypa=.40, pct_passing_td=.25, pct_passing_pct=.20, pct_int_rate=.15),
    "Pro Style"   = c(pct_passing_yds=.30, pct_passing_pct=.30, pct_passing_td=.25, pct_int_rate=.15),
    "West Coast"  = c(pct_passing_pct=.35, pct_passing_ypa=.30, pct_passing_td=.20, pct_int_rate=.15)
  ),
  RB = list(
    "Power Run"   = c(pct_rushing_yds=.40, pct_rushing_car=.35,  pct_rushing_td=.25),
    "Zone Run"    = c(pct_rushing_ypc=.45, pct_rushing_yds=.30,  pct_rushing_long=.25),
    "Spread"      = c(pct_rushing_ypc=.35, pct_rushing_long=.30, pct_rushing_td=.35),
    "I-Formation" = c(pct_rushing_yds=.40, pct_rushing_td=.35,   pct_rushing_car=.25)
  ),
  WR = list(
    "Air Raid"   = c(pct_receiving_yds=.40, pct_receiving_ypr=.35, pct_receiving_td=.25),
    "Spread"     = c(pct_receiving_rec=.40, pct_receiving_yds=.35, pct_receiving_td=.25),
    "West Coast" = c(pct_receiving_rec=.45, pct_receiving_ypr=.30, pct_receiving_td=.25),
    "Pro Style"  = c(pct_receiving_yds=.35, pct_receiving_td=.40,  pct_receiving_rec=.25)
  ),
  TE = list(
    "Pro Style"    = c(pct_receiving_yds=.35, pct_receiving_td=.40,  pct_receiving_rec=.25),
    "West Coast"   = c(pct_receiving_rec=.40, pct_receiving_ypr=.35, pct_receiving_td=.25),
    "12 Personnel" = c(pct_receiving_yds=.35, pct_receiving_rec=.35, pct_receiving_td=.30),
    "Spread"       = c(pct_receiving_ypr=.45, pct_receiving_td=.30,  pct_receiving_rec=.25)
  ),
  EDGE = list(
    "4-3 Under"   = c(pct_defensive_sacks=.40, pct_qb_hur=.35, pct_defensive_tfl=.25),
    "3-4 OLB"     = c(pct_defensive_tfl=.35,   pct_qb_hur=.35, pct_defensive_sacks=.30),
    "Odd Front"   = c(pct_qb_hur=.40, pct_defensive_sacks=.35, pct_defensive_tfl=.25),
    "Wide-9 Tech" = c(pct_defensive_sacks=.50, pct_qb_hur=.30, pct_defensive_tfl=.20)
  ),
  DL = list(
    "3-4 Nose"    = c(pct_defensive_tot=.40,   pct_defensive_solo=.35, pct_defensive_tfl=.25),
    "4-3 DT"      = c(pct_defensive_sacks=.35, pct_defensive_tfl=.35,  pct_defensive_tot=.30),
    "Odd Front"   = c(pct_qb_hur=.40, pct_defensive_tfl=.35, pct_defensive_sacks=.25),
    "Gap Control" = c(pct_defensive_tot=.45,   pct_defensive_solo=.30, pct_defensive_sacks=.25)
  ),
  LB = list(
    "4-3 MLB"         = c(pct_defensive_tot=.35, pct_defensive_tfl=.30,  pct_defensive_solo=.25, pct_defensive_int=.10),
    "3-4 ILB"         = c(pct_defensive_tot=.40, pct_defensive_solo=.30, pct_defensive_tfl=.20,  pct_defensive_pd=.10),
    "Cover 2 Hook"    = c(pct_defensive_pd=.35,  pct_defensive_int=.35,  pct_defensive_tot=.30),
    "Hybrid (Nickel)" = c(pct_defensive_pd=.30,  pct_qb_hur=.25, pct_defensive_int=.25, pct_defensive_tfl=.20)
  ),
  CB = list(
    "Man Cover 1"    = c(pct_defensive_pd=.40,  pct_defensive_int=.35, pct_defensive_tfl=.25),
    "Cover 2"        = c(pct_defensive_tot=.35, pct_defensive_pd=.35,  pct_defensive_int=.30),
    "Cover 3 Zone"   = c(pct_defensive_tot=.40, pct_defensive_pd=.30,  pct_interceptions_yds=.30),
    "Press Coverage" = c(pct_defensive_int=.45, pct_defensive_pd=.35,  pct_defensive_tfl=.20)
  ),
  S = list(
    "Cover 2 FS"  = c(pct_defensive_pd=.35,  pct_defensive_int=.35, pct_defensive_tot=.30),
    "Cover 3 SS"  = c(pct_defensive_tot=.40, pct_defensive_solo=.30, pct_defensive_pd=.30),
    "Single High" = c(pct_defensive_int=.40, pct_interceptions_yds=.30, pct_defensive_pd=.30),
    "Quarters"    = c(pct_defensive_pd=.40,  pct_defensive_int=.30, pct_defensive_tot=.30)
  ),
  K = list(
    "High-Vol Attack" = c(pct_kicking_fga=.45, pct_kicking_fgm=.30, pct_xpm_pct=.25),
    "Conservative"    = c(pct_kicking_pct=.50, pct_xpm_pct=.30,     pct_kicking_fgm=.20),
    "Long Range Spec" = c(pct_kicking_pct=.40, pct_kicking_fga=.35, pct_kicking_fgm=.25)
  )
)

score_schemes <- function(df, pos) {
  sw <- scheme_weights[[pos]]
  if (is.null(sw) || nrow(df) == 0)
    return(df %>% mutate(best_scheme = NA_character_, scheme_breakdown = NA_character_))
  ss <- purrr::imap_dfc(sw, function(w, nm) {
    ca <- intersect(names(w), names(df))
    if (!length(ca)) return(tibble(!!nm := rep(0, nrow(df))))
    ws <- w[ca]; ws <- ws / sum(ws)
    tibble(!!nm := round(as.numeric(as.matrix(df[, ca, drop = FALSE]) %*% ws * 100), 1))
  })
  df$best_scheme      <- apply(ss, 1, function(r) names(which.max(r)))
  df$scheme_breakdown <- apply(ss, 1, function(r) {
    s <- sort(r, decreasing = TRUE)
    paste(names(s), round(s), sep = ": ", collapse = " | ")
  })
  df
}

# ============================================================
# NIL ESTIMATOR
# ============================================================
nil_position_base <- c(
  QB=800000, RB=250000, WR=300000, TE=200000, EDGE=280000,
  DL=220000, LB=180000, CB=260000, S=190000,  K=80000,
  P=60000,   OT=200000, IOL=150000, LS=40000
)
nil_star_mult <- c(`5`=4.50, `4`=2.20, `3`=1.00, `2`=0.45, `1`=0.20)
nil_conf_mult <- c(
  "SEC"=1.40, "Big Ten"=1.35, "Big 12"=1.20, "ACC"=1.15,
  "American Athletic"=0.85, "Mountain West"=0.82, "Sun Belt"=0.78,
  "Mid-American"=0.65, "Conference USA"=0.62, "FBS Independents"=0.70,
  "MVFC"=0.50, "CAA"=0.48, "Big Sky"=0.46, "Southern"=0.44,
  "Southland"=0.42, "Patriot"=0.40, "Pioneer"=0.38, "Big South-OVC"=0.40,
  "NEC"=0.36, "Ivy"=0.38, "FCS Independents"=0.38,
  "SWAC"=0.32, "MEAC"=0.30, "SIAC"=0.28, "UAC"=0.28
)
nil_sp_mult <- function(r) {
  case_when(r<=10~1.35, r<=25~1.20, r<=50~1.10, r<=75~1.00, r<=100~0.90, r<=130~0.80, TRUE~0.75)
}
estimate_nil <- function(df) {
  df %>% mutate(
    nil_base    = coalesce(nil_position_base[Position], 100000),
    nil_star    = coalesce(nil_star_mult[as.character(coalesce(as.integer(Stars), 3L))], 1.0),
    nil_conf    = coalesce(nil_conf_mult[conference], 0.50),
    nil_sp      = nil_sp_mult(coalesce(origin_sp_ranking, 100L)),
    nil_bonus   = pmax(0, pmin(0.80, (coalesce(composite_score, 50) - 40) / 100)),
    nil_raw     = nil_base * nil_star * nil_conf * nil_sp * (1 + nil_bonus),
    nil_value   = pmin(2500000, round(nil_raw / 5000) * 5000),
    nil_display = case_when(
      nil_value >= 1000000 ~ paste0("$", round(nil_value/1000000, 2), "M"),
      nil_value >= 1000    ~ paste0("$", formatC(round(nil_value/1000)*1000, format="d", big.mark=","), ""),
      TRUE                 ~ paste0("$", nil_value)
    ),
    nil_display = gsub("\\$([0-9,]+)000$", "$\\1K", nil_display)
  )
}

# ============================================================
# HEAD-TO-HEAD CONFIG
# ============================================================
h2h_metrics <- list(
  QB   = list(cols=c("passing_yds","passing_td","passing_pct","passing_ypa","passing_int","rushing_yds","Stars"),
              labels=c("Pass Yards","Pass TDs","Completion %","Yards/Att","Interceptions","Rush Yards","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,FALSE,TRUE,TRUE)),
  RB   = list(cols=c("rushing_yds","rushing_td","rushing_ypc","rushing_car","rushing_long","receiving_yds","receiving_rec","Stars"),
              labels=c("Rush Yards","Rush TDs","Yards/Carry","Carries","Long Rush","Rec Yards","Receptions","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  WR   = list(cols=c("receiving_yds","receiving_rec","receiving_ypr","receiving_td","Stars"),
              labels=c("Rec Yards","Receptions","Yards/Rec","Rec TDs","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE)),
  TE   = list(cols=c("receiving_yds","receiving_rec","receiving_ypr","receiving_td","Stars"),
              labels=c("Rec Yards","Receptions","Yards/Rec","Rec TDs","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE)),
  EDGE = list(cols=c("defensive_sacks","defensive_tfl","defensive_qb_hur","defensive_solo","defensive_tot","Stars"),
              labels=c("Sacks","TFL","QB Hurries","Solo Tackles","Total Tackles","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  DL   = list(cols=c("defensive_sacks","defensive_tfl","defensive_qb_hur","defensive_solo","defensive_tot","Stars"),
              labels=c("Sacks","TFL","QB Hurries","Solo Tackles","Total Tackles","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  LB   = list(cols=c("defensive_tot","defensive_tfl","defensive_solo","interceptions_int","defensive_pd","Stars"),
              labels=c("Total Tackles","TFL","Solo Tackles","Interceptions","Passes Defended","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  CB   = list(cols=c("defensive_pd","interceptions_int","interceptions_yds","defensive_solo","defensive_tfl","Stars"),
              labels=c("Passes Defended","Interceptions","INT Yards","Solo Tackles","TFL","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  S    = list(cols=c("defensive_tot","defensive_pd","interceptions_int","defensive_solo","defensive_tfl","Stars"),
              labels=c("Total Tackles","Passes Defended","Interceptions","Solo Tackles","TFL","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE)),
  K    = list(cols=c("kicking_fgm","kicking_fga","kicking_pct","kicking_xpm","kicking_xpa","Stars"),
              labels=c("FG Made","FG Attempted","FG %","XP Made","XP Attempted","Stars"),
              higher=c(TRUE,TRUE,TRUE,TRUE,TRUE,TRUE))
)

# ============================================================
# VIZ CONFIG
# ============================================================
viz_cfg <- list(
  QB   = list(bar_col="passing_yds",    bar_label="Passing Yards",
              scat_x="passing_pct",     scat_y="passing_ypa",
              scat_xl="Completion %",   scat_yl="Yards Per Attempt",
              pctile_cols=c("passing_yds","passing_td","passing_pct","passing_ypa","rushing_yds"),
              pctile_labels=c("Pass Yds","Pass TDs","Comp%","YPA","Rush Yds")),
  RB   = list(bar_col="rushing_yds",    bar_label="Rushing Yards",
              scat_x="rushing_car",     scat_y="rushing_ypc",
              scat_xl="Carries",        scat_yl="Yards Per Carry",
              pctile_cols=c("rushing_yds","rushing_td","rushing_ypc","rushing_car","receiving_yds","receiving_rec"),
              pctile_labels=c("Rush Yds","Rush TDs","YPC","Carries","Rec Yds","Receptions")),
  WR   = list(bar_col="receiving_yds",  bar_label="Receiving Yards",
              scat_x="receiving_rec",   scat_y="receiving_ypr",
              scat_xl="Receptions",     scat_yl="Yards Per Reception",
              pctile_cols=c("receiving_yds","receiving_rec","receiving_ypr","receiving_td"),
              pctile_labels=c("Rec Yds","Receptions","YPR","Rec TDs")),
  TE   = list(bar_col="receiving_yds",  bar_label="Receiving Yards",
              scat_x="receiving_rec",   scat_y="receiving_ypr",
              scat_xl="Receptions",     scat_yl="Yards Per Reception",
              pctile_cols=c("receiving_yds","receiving_rec","receiving_ypr","receiving_td"),
              pctile_labels=c("Rec Yds","Receptions","YPR","Rec TDs")),
  EDGE = list(bar_col="defensive_sacks", bar_label="Sacks",
              scat_x="defensive_sacks",  scat_y="defensive_qb_hur",
              scat_xl="Sacks",           scat_yl="QB Hurries",
              pctile_cols=c("defensive_sacks","defensive_tfl","defensive_qb_hur","defensive_solo"),
              pctile_labels=c("Sacks","TFL","Hurries","Solo Tkl")),
  DL   = list(bar_col="defensive_tfl",  bar_label="Tackles For Loss",
              scat_x="defensive_tfl",   scat_y="defensive_tot",
              scat_xl="TFL",            scat_yl="Total Tackles",
              pctile_cols=c("defensive_sacks","defensive_tfl","defensive_qb_hur","defensive_tot"),
              pctile_labels=c("Sacks","TFL","Hurries","Total Tkl")),
  LB   = list(bar_col="defensive_tot",  bar_label="Total Tackles",
              scat_x="defensive_solo",  scat_y="defensive_tot",
              scat_xl="Solo Tackles",   scat_yl="Total Tackles",
              pctile_cols=c("defensive_tot","defensive_tfl","defensive_pd","interceptions_int"),
              pctile_labels=c("Total Tkl","TFL","PDs","INTs")),
  CB   = list(bar_col="defensive_pd",   bar_label="Passes Defended",
              scat_x="interceptions_int", scat_y="defensive_pd",
              scat_xl="Interceptions", scat_yl="Passes Defended",
              pctile_cols=c("defensive_pd","interceptions_int","defensive_solo","defensive_tfl"),
              pctile_labels=c("PDs","INTs","Solo Tkl","TFL")),
  S    = list(bar_col="defensive_tot",  bar_label="Total Tackles",
              scat_x="defensive_pd",    scat_y="interceptions_int",
              scat_xl="Passes Defended", scat_yl="Interceptions",
              pctile_cols=c("defensive_tot","defensive_pd","interceptions_int","defensive_solo"),
              pctile_labels=c("Total Tkl","PDs","INTs","Solo Tkl")),
  K    = list(bar_col="kicking_pct",   bar_label="FG Percentage",
              scat_x="kicking_fga",    scat_y="kicking_pct",
              scat_xl="FG Attempted",  scat_yl="FG Percentage",
              pctile_cols=c("kicking_fgm","kicking_fga","kicking_pct"),
              pctile_labels=c("FGM","FGA","FG%"))
)

make_pos_analytics <- function(pos) {
  lp <- tolower(pos)
  tagList(
    br(),
    fluidRow(
      column(6, plotOutput(paste0("viz_",lp,"_bar"),   height="320px")),
      column(6, plotOutput(paste0("viz_",lp,"_scat"),  height="320px"))
    ),
    br(),
    fluidRow(
      column(6, plotOutput(paste0("viz_",lp,"_dist"),  height="280px")),
      column(6, plotOutput(paste0("viz_",lp,"_stars"), height="280px"))
    ),
    tags$hr(style="border-color:#222;margin:20px 0"),
    fluidRow(
      column(4, div(class="section-label","Player drill-down"), uiOutput(paste0("sel_",lp,"_player"))),
      column(4, plotOutput(paste0("viz_",lp,"_pctile"), height="260px")),
      column(4, plotOutput(paste0("viz_",lp,"_rel"),    height="260px"))
    )
  )
}

pos_palette <- c(QB="#CFB87C", RB="#1D9E75", WR="#378ADD", TE="#9B59B6",
                 EDGE="#E74C3C", DL="#F39C12", LB="#16A085", CB="#3498DB",
                 S="#E67E22", K="#95A5A6")

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$head(tags$style(HTML("
    @import url('https://fonts.googleapis.com/css2?family=Oswald:wght@400;600;700&family=Source+Sans+3:wght@300;400;600&display=swap');
    *{box-sizing:border-box}
    body{background-color:#0a0a0a;color:#f0f0f0;font-family:'Source Sans 3',sans-serif}
    .header{background:linear-gradient(135deg,#1a1a1a 0%,#0d0d0d 100%);border-bottom:3px solid #CFB87C;
            padding:20px 40px;display:flex;align-items:center;gap:20px;margin-bottom:30px}
    .header-title{font-family:'Oswald',sans-serif;font-size:26px;font-weight:700;color:#CFB87C;
                  letter-spacing:2px;text-transform:uppercase}
    .header-subtitle{font-size:12px;color:#888;letter-spacing:1px;text-transform:uppercase;margin-top:2px}
    .cu-badge{background:#CFB87C;color:#000;font-family:'Oswald',sans-serif;font-weight:700;
              font-size:22px;width:50px;height:50px;display:flex;align-items:center;
              justify-content:center;border-radius:4px;flex-shrink:0}
    .section-label{font-family:'Oswald',sans-serif;font-size:11px;font-weight:600;color:#CFB87C;
                   letter-spacing:2px;text-transform:uppercase;margin-bottom:10px;
                   padding-bottom:6px;border-bottom:1px solid #222}
    .weight-total{font-family:'Oswald',sans-serif;font-size:13px;padding:8px 12px;
                  border-radius:3px;margin:4px 0;text-align:center;font-weight:600}
    .weight-total.ok{background:#1a2e1a;color:#4caf50;border:1px solid #2e5c2e}
    .weight-total.bad{background:#2e1a1a;color:#f44336;border:1px solid #5c2e2e}
    .apply-btn{width:100%;background:#CFB87C;border:none;color:#000;font-family:'Oswald',sans-serif;
               font-size:14px;font-weight:700;letter-spacing:2px;padding:12px;cursor:pointer;
               border-radius:3px;margin-top:12px;text-transform:uppercase}
    .apply-btn:hover{background:#e8d08e}
    .nav-tabs{border-bottom:2px solid #CFB87C !important;margin-bottom:20px}
    .nav-tabs>li>a{font-family:'Oswald',sans-serif !important;font-size:13px !important;
      letter-spacing:1px !important;text-transform:uppercase !important;color:#888 !important;
      background:#111 !important;border:1px solid #333 !important;border-bottom:none !important;
      margin-right:4px !important;border-radius:3px 3px 0 0 !important}
    .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover
      {background:#CFB87C !important;color:#000 !important;border-color:#CFB87C !important}
    .tab-content{background:transparent !important}
    .dataTables_wrapper{color:#f0f0f0}
    table.dataTable thead th{background:#1a1a1a !important;color:#CFB87C !important;
      font-family:'Oswald',sans-serif !important;font-size:12px !important;
      letter-spacing:1px !important;text-transform:uppercase !important;
      border-bottom:2px solid #CFB87C !important;border-top:none !important}
    table.dataTable tbody tr{background:#0d0d0d !important;border-bottom:1px solid #1a1a1a !important}
    table.dataTable tbody tr:hover{background:#1a1a1a !important}
    table.dataTable tbody td{color:#ddd !important;font-size:13px !important;
      border:none !important;padding:8px 12px !important}
    .dataTables_info,.dataTables_length,.dataTables_filter,.dataTables_paginate
      {color:#666 !important;font-size:12px !important}
    .dataTables_paginate .paginate_button{color:#666 !important;border:1px solid #333 !important;
      background:#111 !important;border-radius:3px !important}
    .dataTables_paginate .paginate_button.current{background:#CFB87C !important;
      color:#000 !important;border-color:#CFB87C !important}
    .dataTables_filter input{background:#1a1a1a !important;border:1px solid #333 !important;
      color:#f0f0f0 !important;border-radius:3px !important;padding:4px 8px !important}
    .irs--shiny .irs-bar{background:#CFB87C !important;border-color:#CFB87C !important}
    .irs--shiny .irs-handle{background:#CFB87C !important;border-color:#b8a060 !important}
    .irs--shiny .irs-from,.irs--shiny .irs-to,.irs--shiny .irs-single
      {background:#CFB87C !important;color:#000 !important}
    .irs--shiny .irs-min,.irs--shiny .irs-max{color:#666 !important}
    .irs--shiny .irs-line{background:#333 !important}
    .well{background:#111 !important;border:1px solid #222 !important;border-radius:4px !important}
    .overall-note{font-size:12px;color:#666;font-style:italic;text-align:center;
      padding:12px;border:1px solid #222;border-radius:4px;margin-top:8px}
    .scheme-badge{display:inline-block;background:#1a2e1a;color:#4caf50;border:1px solid #2e5c2e;
      border-radius:3px;padding:2px 6px;font-size:11px;font-family:'Oswald',sans-serif;letter-spacing:.5px}
    .player-headshot{height:38px;width:auto;border-radius:3px;object-fit:cover;
      vertical-align:middle;background:#1a1a1a}
    .h2h-card{background:#111;border:1px solid #222;border-radius:6px;padding:0;overflow:hidden;margin-bottom:20px}
    .h2h-header{display:grid;grid-template-columns:1fr 80px 1fr;background:#1a1a1a;border-bottom:2px solid #CFB87C;padding:0}
    .h2h-player{padding:14px 16px;text-align:center}
    .h2h-player-name{font-family:'Oswald',sans-serif;font-size:16px;font-weight:700;color:#CFB87C;letter-spacing:1px;margin-bottom:4px}
    .h2h-player-sub{font-size:11px;color:#888;letter-spacing:.5px}
    .h2h-vs{display:flex;align-items:center;justify-content:center;font-family:'Oswald',sans-serif;font-size:18px;font-weight:700;color:#444;border-left:1px solid #222;border-right:1px solid #222}
    .h2h-row{display:grid;grid-template-columns:1fr 120px 1fr;border-bottom:1px solid #1a1a1a;align-items:center;min-height:44px}
    .h2h-row:last-child{border-bottom:none}
    .h2h-row:hover{background:#161616}
    .h2h-val{padding:10px 14px;font-size:14px;font-weight:500}
    .h2h-val-left{text-align:right;color:#ddd}
    .h2h-val-right{text-align:left;color:#ddd}
    .h2h-val.winner{color:#CFB87C;font-weight:700}
    .h2h-metric{padding:8px 10px;text-align:center;font-size:11px;color:#666;font-family:'Oswald',sans-serif;letter-spacing:.5px;text-transform:uppercase;background:#0d0d0d;border-left:1px solid #1a1a1a;border-right:1px solid #1a1a1a}
    .h2h-bar-row{display:grid;grid-template-columns:1fr 120px 1fr;align-items:center;padding:2px 0}
    .h2h-bar-wrap{padding:0 14px}
    .h2h-bar-outer{height:5px;background:#222;border-radius:3px;overflow:hidden}
    .h2h-bar-inner{height:100%;border-radius:3px;background:#CFB87C}
    .h2h-bar-inner.loser{background:#333}
    .h2h-footer{display:grid;grid-template-columns:1fr 120px 1fr;background:#1a1a1a;border-top:2px solid #CFB87C;padding:12px 0}
    .h2h-footer-val{text-align:center;font-family:'Oswald',sans-serif;font-size:13px;font-weight:600;color:#888;padding:0 14px}
    .h2h-footer-val.winner{color:#4caf50;font-size:15px}
    .h2h-footer-mid{text-align:center;font-size:11px;color:#555;padding-top:4px;border-left:1px solid #222;border-right:1px solid #222}
    .h2h-img{width:56px;height:42px;object-fit:cover;border-radius:3px;background:#1a1a1a;display:block;margin:0 auto 4px}
  "))),
  
  div(class="header",
      div(class="cu-badge","CU"),
      div(div(class="header-title","Transfer Portal Valuation"),
          div(class="header-subtitle","University of Colorado Football · 2025 Portal Class"))),
  
  fluidRow(
    column(3, wellPanel(uiOutput("dynamic_sidebar"))),
    column(9,
           tabsetPanel(id="main_tabs",
                       tabPanel("Visuals", value="VIS",
                                br(),
                                fluidRow(
                                  column(7, plotOutput("viz_top_overall",    height="540px")),
                                  column(5, plotOutput("viz_score_vs_nil",   height="480px", click="viz_score_vs_nil_click"),
                                         uiOutput("viz_score_vs_nil_clicked"))
                                ),
                                br(),
                                fluidRow(
                                  column(6, plotOutput("viz_pos_depth",    height="420px")),
                                  column(6, plotOutput("viz_origin_conf",  height="420px"))
                                )
                       ),
                       tabPanel("Quarterbacks",   value="QB",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_qb")),   tabPanel("Analytics",make_pos_analytics("QB")))),
                       tabPanel("Running Backs",  value="RB",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_rb")),   tabPanel("Analytics",make_pos_analytics("RB")))),
                       tabPanel("Wide Receivers", value="WR",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_wr")),   tabPanel("Analytics",make_pos_analytics("WR")))),
                       tabPanel("Tight Ends",     value="TE",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_te")),   tabPanel("Analytics",make_pos_analytics("TE")))),
                       tabPanel("Edge Rushers",   value="EDGE", tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_edge")), tabPanel("Analytics",make_pos_analytics("EDGE")))),
                       tabPanel("D Linemen",      value="DL",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_dl")),   tabPanel("Analytics",make_pos_analytics("DL")))),
                       tabPanel("Linebackers",    value="LB",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_lb")),   tabPanel("Analytics",make_pos_analytics("LB")))),
                       tabPanel("Cornerbacks",    value="CB",   tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_cb")),   tabPanel("Analytics",make_pos_analytics("CB")))),
                       tabPanel("Safeties",       value="S",    tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_s")),    tabPanel("Analytics",make_pos_analytics("S")))),
                       tabPanel("Kickers",        value="K",    tabsetPanel(tabPanel("Rankings",br(),DTOutput("tbl_k")),    tabPanel("Analytics",make_pos_analytics("K")))),
                       tabPanel("Scheme Fit", value="SCHEME",
                                br(),
                                fluidRow(
                                  column(4,
                                         selectInput("scheme_pos","Position",choices=c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"),selected="EDGE"),
                                         uiOutput("scheme_filter_ui"), br(),
                                         div(class="overall-note","Scores 0-99 percentile fit per scheme.")),
                                  column(8, plotOutput("viz_scheme_bars", height="500px"))
                                ),
                                br(), DTOutput("tbl_scheme")
                       ),
                       tabPanel("Player Radar", value="RADAR",
                                br(),
                                fluidRow(
                                  column(4,
                                         selectInput("radar_pos","Position",choices=c("QB","RB","WR","TE","EDGE","DL","LB","CB","S"),selected="EDGE"),
                                         uiOutput("player_selector")),
                                  column(8, plotOutput("player_radar", height="420px"))
                                )
                       ),
                       tabPanel("Compare Players", value="COMPARE",
                                br(),
                                fluidRow(
                                  column(3,
                                         selectInput("cmp_pos","Position",choices=c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"),selected="EDGE"),
                                         uiOutput("cmp_player_a_ui"),
                                         uiOutput("cmp_player_b_ui"),
                                         br(),
                                         div(class="section-label","Display Options"),
                                         checkboxInput("cmp_show_radar",     "Show radar overlay",          value=TRUE),
                                         checkboxInput("cmp_show_scorecard", "Show head-to-head scorecard", value=TRUE),
                                         checkboxInput("cmp_show_table",     "Show metrics table",          value=FALSE)
                                  ),
                                  column(9,
                                         conditionalPanel("input.cmp_show_scorecard == true", uiOutput("h2h_scorecard")),
                                         conditionalPanel("input.cmp_show_radar == true",     br(), plotOutput("cmp_radar", height="400px")),
                                         conditionalPanel("input.cmp_show_table == true",     br(), DTOutput("cmp_table"))
                                  )
                                )
                       ),
                       tabPanel("NIL Estimator", value="NIL",
                                br(),
                                fluidRow(
                                  column(4,
                                         selectInput("nil_pos","Position",choices=c("All","QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"),selected="All"),
                                         selectInput("nil_conf","Conference",choices=c("All","SEC","Big Ten","Big 12","ACC","American Athletic","Mountain West","Sun Belt","Mid-American","Conference USA","MVFC","CAA","Big Sky","FCS Independents"),selected="All"),
                                         sliderInput("nil_stars","Min Stars",min=1,max=5,value=1,step=1),
                                         sliderInput("nil_budget","CU Budget Cap ($K)",min=50,max=2500,value=500,step=25),
                                         div(class="overall-note","Estimates based on 2024-25 portal market rates.")
                                  ),
                                  column(8,
                                         plotOutput("viz_nil_dist",   height="280px"), br(),
                                         plotOutput("viz_nil_by_pos", height="280px")
                                  )
                                ),
                                br(), DTOutput("tbl_nil")
                       ),
                       tabPanel("Overall", value="OVR", br(), DTOutput("tbl_overall"))
           )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  make_total_ui <- function(total) {
    cls <- if(total==100) "weight-total ok" else "weight-total bad"
    msg <- if(total==100) paste0("✓ ",total,"%") else paste0("⚠ ",total,"% — must equal 100%")
    div(class=cls, msg)
  }
  
  make_dt <- function(df, top_n=30) {
    if(is.null(df)||nrow(df)==0) return(datatable(data.frame(Message="No data available")))
    df2 <- df %>% head(top_n) %>% mutate(Rank=row_number()) %>% select(Rank,everything())
    col_defs <- list()
    if("Photo" %in% names(df2)) {
      idx <- which(names(df2)=="Photo")-1
      col_defs <- c(col_defs, list(list(targets=idx, width="55px",
                                        render=JS("function(d,t,r,m){if(!d||d==='NA')return '<div style=\"width:42px;height:32px;background:#1a1a1a;border-radius:3px;\"></div>';return '<img src=\"'+d+'\" class=\"player-headshot\" onerror=\"this.style.display=\\'none\\'\" />';}"))))
    }
    if("Best Scheme" %in% names(df2)) {
      idx <- which(names(df2)=="Best Scheme")-1
      col_defs <- c(col_defs, list(list(targets=idx,
                                        render=JS("function(d,t,r,m){return d?'<span class=\"scheme-badge\">'+d+'</span>':''}"))))
    }
    if("Scheme Breakdown" %in% names(df2)) {
      idx <- which(names(df2)=="Scheme Breakdown")-1
      col_defs <- c(col_defs, list(list(targets=idx,
                                        render=JS("function(d,t,r,m){return d?'<span style=\"font-size:11px;color:#888;font-style:italic;\">'+d+'</span>':''}"))))
    }
    datatable(df2,
              options=list(pageLength=30, dom="ftp", ordering=TRUE,
                           columnDefs=if(length(col_defs)) col_defs else list()),
              rownames=FALSE, escape=FALSE, selection="none", class="cell-border") %>%
      formatStyle("Score", color=CU_GOLD, fontWeight="bold") %>%
      formatStyle("Rank",  color=CU_GOLD, fontWeight="bold") %>%
      { if("NIL Est." %in% names(df2)) formatStyle(.,"NIL Est.",color="#4caf50",fontWeight="bold") else . }
  }
  
  norm_w <- function(...) {
    vals <- c(...)
    if(sum(vals)==0) vals <- rep(1, length(vals))
    vals / sum(vals)
  }
  
  cap_scores <- function(df) df %>% mutate(composite_score = pmin(composite_score, 99))
  
  # ============================================================
  # SCORING REACTIVES
  # ============================================================
  calc_qb <- eventReactive(input$recalc_qb, {
    req(input$w_qb_ypa,input$w_qb_td,input$w_qb_yds,input$w_qb_pct,input$w_qb_int,input$w_qb_rush,input$w_qb_stars)
    w <- norm_w(input$w_qb_ypa,input$w_qb_td,input$w_qb_yds,input$w_qb_pct,input$w_qb_int,input$w_qb_rush,input$w_qb_stars)
    df <- transfer_stats %>% filter(Position=="QB",!is.na(passing_att),passing_att>=50)
    if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      int_rate        = coalesce(passing_int,0)/coalesce(passing_att,1),
      passing_yds_adj = coalesce(passing_yds,0)*coalesce(sos_off_multiplier,1),
      passing_td_adj  = coalesce(passing_td,0) *coalesce(sos_off_multiplier,1),
      rushing_yds_adj = coalesce(rushing_yds,0)*coalesce(sos_off_multiplier,1),
      pct_passing_ypa = percent_rank(coalesce(passing_ypa,0)),
      pct_passing_td  = percent_rank(passing_td_adj),
      pct_passing_yds = percent_rank(passing_yds_adj),
      pct_passing_pct = percent_rank(coalesce(passing_pct,0)),
      pct_int_rate    = 1 - percent_rank(int_rate),
      pct_rushing_yds = percent_rank(rushing_yds_adj),
      pct_stars       = percent_rank(coalesce(Stars,0)),
      composite_score = (pct_passing_ypa*w[1]+pct_passing_td*w[2]+pct_passing_yds*w[3]+
                           pct_passing_pct*w[4]+pct_int_rate*w[5]+pct_rushing_yds*w[6]+
                           pct_stars*w[7])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"QB"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="QB",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             `Pass Yds`=passing_yds,TD=passing_td,`Comp%`=passing_pct,YPA=passing_ypa,
             `INT Rate`=int_rate,`Rush Yds`=rushing_yds,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_rb <- eventReactive(input$recalc_rb, {
    req(input$w_rb_ypc,input$w_rb_yds,input$w_rb_car,input$w_rb_td,input$w_rb_long,
        input$w_rb_rec_yds,input$w_rb_rec,input$w_rb_stars)
    w <- norm_w(input$w_rb_ypc,input$w_rb_yds,input$w_rb_car,input$w_rb_td,input$w_rb_long,
                input$w_rb_rec_yds,input$w_rb_rec,input$w_rb_stars)
    df <- transfer_stats %>% filter(Position=="RB",!is.na(rushing_car),rushing_car>=50)
    if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      rushing_yds_adj   = coalesce(rushing_yds,0)  *coalesce(sos_off_multiplier,1),
      rushing_td_adj    = coalesce(rushing_td,0)   *coalesce(sos_off_multiplier,1),
      rushing_car_adj   = coalesce(rushing_car,0)  *coalesce(sos_off_multiplier,1),
      receiving_yds_adj = coalesce(receiving_yds,0)*coalesce(sos_off_multiplier,1),
      receiving_rec_adj = coalesce(receiving_rec,0)*coalesce(sos_off_multiplier,1),
      pct_rushing_ypc   = percent_rank(coalesce(rushing_ypc,0)),
      pct_rushing_yds   = percent_rank(rushing_yds_adj),
      pct_rushing_car   = percent_rank(rushing_car_adj),
      pct_rushing_td    = percent_rank(rushing_td_adj),
      pct_rushing_long  = percent_rank(coalesce(rushing_long,0)),
      pct_receiving_yds = percent_rank(receiving_yds_adj),
      pct_receiving_rec = percent_rank(receiving_rec_adj),
      pct_stars         = percent_rank(coalesce(Stars,0)),
      composite_score   = (pct_rushing_ypc*w[1]+pct_rushing_yds*w[2]+pct_rushing_car*w[3]+
                             pct_rushing_td*w[4]+pct_rushing_long*w[5]+
                             pct_receiving_yds*w[6]+pct_receiving_rec*w[7]+
                             pct_stars*w[8])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"RB"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="RB",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             `Rush Yds`=rushing_yds,TD=rushing_td,YPC=rushing_ypc,Carries=rushing_car,
             `Rec Yds`=receiving_yds,Rec=receiving_rec,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_wr <- eventReactive(input$recalc_wr, {
    req(input$w_wr_yds,input$w_wr_rec,input$w_wr_ypr,input$w_wr_td,input$w_wr_stars)
    w <- norm_w(input$w_wr_yds,input$w_wr_rec,input$w_wr_ypr,input$w_wr_td,input$w_wr_stars)
    df <- transfer_stats %>% filter(Position=="WR",!is.na(receiving_rec),receiving_rec>=10)
    if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      receiving_yds_adj = coalesce(receiving_yds,0)*coalesce(sos_off_multiplier,1),
      receiving_rec_adj = coalesce(receiving_rec,0)*coalesce(sos_off_multiplier,1),
      receiving_td_adj  = coalesce(receiving_td,0) *coalesce(sos_off_multiplier,1),
      pct_receiving_yds = percent_rank(receiving_yds_adj),
      pct_receiving_rec = percent_rank(receiving_rec_adj),
      pct_receiving_ypr = percent_rank(coalesce(receiving_ypr,0)),
      pct_receiving_td  = percent_rank(receiving_td_adj),
      pct_stars         = percent_rank(coalesce(Stars,0)),
      composite_score   = (pct_receiving_yds*w[1]+pct_receiving_rec*w[2]+
                             pct_receiving_ypr*w[3]+pct_receiving_td*w[4]+
                             pct_stars*w[5])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"WR"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="WR",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             `Rec Yds`=receiving_yds,Rec=receiving_rec,YPR=receiving_ypr,TD=receiving_td,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_te <- eventReactive(input$recalc_te, {
    req(input$w_te_td,input$w_te_yds,input$w_te_rec,input$w_te_ypr,input$w_te_stars)
    w <- norm_w(input$w_te_td,input$w_te_yds,input$w_te_rec,input$w_te_ypr,input$w_te_stars)
    df <- transfer_stats %>% filter(Position=="TE",!is.na(receiving_rec),receiving_rec>=10)
    if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      receiving_yds_adj = coalesce(receiving_yds,0)*coalesce(sos_off_multiplier,1),
      receiving_rec_adj = coalesce(receiving_rec,0)*coalesce(sos_off_multiplier,1),
      receiving_td_adj  = coalesce(receiving_td,0) *coalesce(sos_off_multiplier,1),
      pct_receiving_td  = percent_rank(receiving_td_adj),
      pct_receiving_yds = percent_rank(receiving_yds_adj),
      pct_receiving_rec = percent_rank(receiving_rec_adj),
      pct_receiving_ypr = percent_rank(coalesce(receiving_ypr,0)),
      pct_stars         = percent_rank(coalesce(Stars,0)),
      composite_score   = (pct_receiving_td*w[1]+pct_receiving_yds*w[2]+
                             pct_receiving_rec*w[3]+pct_receiving_ypr*w[4]+
                             pct_stars*w[5])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"TE"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="TE",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             `Rec Yds`=receiving_yds,Rec=receiving_rec,YPR=receiving_ypr,TD=receiving_td,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_edge <- eventReactive(input$recalc_edge, {
    req(input$w_edge_sacks,input$w_edge_tfl,input$w_edge_hur,input$w_edge_solo,input$w_edge_tot,input$w_edge_stars)
    w <- norm_w(input$w_edge_sacks,input$w_edge_tfl,input$w_edge_hur,input$w_edge_solo,input$w_edge_tot,input$w_edge_stars)
    df <- transfer_stats %>% filter(Position=="EDGE"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_defensive_sacks = percent_rank(coalesce(defensive_sacks,0) *coalesce(sos_def_multiplier,1)),
      pct_defensive_tfl   = percent_rank(coalesce(defensive_tfl,0)   *coalesce(sos_def_multiplier,1)),
      pct_qb_hur          = percent_rank(coalesce(defensive_qb_hur,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_solo  = percent_rank(coalesce(defensive_solo,0)  *coalesce(sos_def_multiplier,1)),
      pct_defensive_tot   = percent_rank(coalesce(defensive_tot,0)   *coalesce(sos_def_multiplier,1)),
      pct_stars           = percent_rank(coalesce(Stars,0)),
      composite_score     = (pct_defensive_sacks*w[1]+pct_defensive_tfl*w[2]+pct_qb_hur*w[3]+
                               pct_defensive_solo*w[4]+pct_defensive_tot*w[5]+
                               pct_stars*w[6])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"EDGE"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="EDGE",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             Sacks=defensive_sacks,TFL=defensive_tfl,Hurries=defensive_qb_hur,
             Solo=defensive_solo,Total=defensive_tot,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_dl <- eventReactive(input$recalc_dl, {
    req(input$w_dl_sacks,input$w_dl_tfl,input$w_dl_hur,input$w_dl_solo,input$w_dl_tot,input$w_dl_stars)
    w <- norm_w(input$w_dl_sacks,input$w_dl_tfl,input$w_dl_hur,input$w_dl_solo,input$w_dl_tot,input$w_dl_stars)
    df <- transfer_stats %>% filter(Position=="DL"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_defensive_sacks = percent_rank(coalesce(defensive_sacks,0) *coalesce(sos_def_multiplier,1)),
      pct_defensive_tfl   = percent_rank(coalesce(defensive_tfl,0)   *coalesce(sos_def_multiplier,1)),
      pct_qb_hur          = percent_rank(coalesce(defensive_qb_hur,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_solo  = percent_rank(coalesce(defensive_solo,0)  *coalesce(sos_def_multiplier,1)),
      pct_defensive_tot   = percent_rank(coalesce(defensive_tot,0)   *coalesce(sos_def_multiplier,1)),
      pct_stars           = percent_rank(coalesce(Stars,0)),
      composite_score     = (pct_defensive_sacks*w[1]+pct_defensive_tfl*w[2]+pct_qb_hur*w[3]+
                               pct_defensive_solo*w[4]+pct_defensive_tot*w[5]+
                               pct_stars*w[6])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"DL"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="DL",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             Sacks=defensive_sacks,TFL=defensive_tfl,Hurries=defensive_qb_hur,
             Solo=defensive_solo,Total=defensive_tot,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_lb <- eventReactive(input$recalc_lb, {
    req(input$w_lb_sacks,input$w_lb_tfl,input$w_lb_hur,input$w_lb_solo,
        input$w_lb_tot,input$w_lb_fum_rec,input$w_lb_int,input$w_lb_pd,input$w_lb_stars)
    w <- norm_w(input$w_lb_sacks,input$w_lb_tfl,input$w_lb_hur,input$w_lb_solo,
                input$w_lb_tot,input$w_lb_fum_rec,input$w_lb_int,input$w_lb_pd,input$w_lb_stars)
    df <- transfer_stats %>% filter(Position=="LB"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_defensive_sacks = percent_rank(coalesce(defensive_sacks,0) *coalesce(sos_def_multiplier,1)),
      pct_defensive_tfl   = percent_rank(coalesce(defensive_tfl,0)   *coalesce(sos_def_multiplier,1)),
      pct_qb_hur          = percent_rank(coalesce(defensive_qb_hur,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_solo  = percent_rank(coalesce(defensive_solo,0)  *coalesce(sos_def_multiplier,1)),
      pct_defensive_tot   = percent_rank(coalesce(defensive_tot,0)   *coalesce(sos_def_multiplier,1)),
      pct_fumbles_rec     = percent_rank(coalesce(fumbles_rec,0)      *coalesce(sos_def_multiplier,1)),
      pct_defensive_int   = percent_rank(coalesce(interceptions_int,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_pd    = percent_rank(coalesce(defensive_pd,0)    *coalesce(sos_def_multiplier,1)),
      pct_stars           = percent_rank(coalesce(Stars,0)),
      composite_score     = (pct_defensive_sacks*w[1]+pct_defensive_tfl*w[2]+pct_qb_hur*w[3]+
                               pct_defensive_solo*w[4]+pct_defensive_tot*w[5]+pct_fumbles_rec*w[6]+
                               pct_defensive_int*w[7]+pct_defensive_pd*w[8]+
                               pct_stars*w[9])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"LB"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="LB",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             Sacks=defensive_sacks,TFL=defensive_tfl,Hurries=defensive_qb_hur,
             Solo=defensive_solo,Total=defensive_tot,
             `Fumbles Rec`=fumbles_rec,Interceptions=interceptions_int,`Passes Defended`=defensive_pd,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  calc_cb <- eventReactive(input$recalc_cb, {
    req(input$w_cb_tfl,input$w_cb_solo,input$w_cb_tot,input$w_cb_int,input$w_cb_pd,input$w_cb_int_yds,input$w_cb_stars)
    w <- norm_w(input$w_cb_tfl,input$w_cb_solo,input$w_cb_tot,input$w_cb_int,input$w_cb_pd,input$w_cb_int_yds,input$w_cb_stars)
    df <- transfer_stats %>% filter(Position=="CB"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_defensive_tfl     = percent_rank(coalesce(defensive_tfl,0)    *coalesce(sos_def_multiplier,1)),
      pct_defensive_solo    = percent_rank(coalesce(defensive_solo,0)   *coalesce(sos_def_multiplier,1)),
      pct_defensive_tot     = percent_rank(coalesce(defensive_tot,0)    *coalesce(sos_def_multiplier,1)),
      pct_defensive_int     = percent_rank(coalesce(interceptions_int,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_pd      = percent_rank(coalesce(defensive_pd,0)     *coalesce(sos_def_multiplier,1)),
      pct_interceptions_yds = percent_rank(coalesce(interceptions_yds,0)*coalesce(sos_def_multiplier,1)),
      pct_stars             = percent_rank(coalesce(Stars,0)),
      composite_score       = (pct_defensive_tfl*w[1]+pct_defensive_solo*w[2]+pct_defensive_tot*w[3]+
                                 pct_defensive_int*w[4]+pct_defensive_pd*w[5]+
                                 pct_interceptions_yds*w[6]+pct_stars*w[7])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"CB"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="CB",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             TFL=defensive_tfl,Solo=defensive_solo,Total=defensive_tot,
             Interceptions=interceptions_int,`Passes Defended`=defensive_pd,
             `Interception Yards`=interceptions_yds,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  s_scores <- eventReactive(input$recalc_s, {
    req(input$w_s_tfl,input$w_s_solo,input$w_s_tot,input$w_s_int,input$w_s_pd,input$w_s_stars)
    w <- norm_w(input$w_s_tfl,input$w_s_solo,input$w_s_tot,input$w_s_int,input$w_s_pd,input$w_s_stars)
    df <- transfer_stats %>% filter(Position=="S"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_defensive_tfl     = percent_rank(coalesce(defensive_tfl,0)    *coalesce(sos_def_multiplier,1)),
      pct_defensive_solo    = percent_rank(coalesce(defensive_solo,0)   *coalesce(sos_def_multiplier,1)),
      pct_defensive_tot     = percent_rank(coalesce(defensive_tot,0)    *coalesce(sos_def_multiplier,1)),
      pct_defensive_int     = percent_rank(coalesce(interceptions_int,0)*coalesce(sos_def_multiplier,1)),
      pct_defensive_pd      = percent_rank(coalesce(defensive_pd,0)     *coalesce(sos_def_multiplier,1)),
      pct_interceptions_yds = percent_rank(coalesce(interceptions_yds,0)*coalesce(sos_def_multiplier,1)),
      pct_fumbles_rec       = percent_rank(coalesce(fumbles_rec,0)       *coalesce(sos_def_multiplier,1)),
      pct_stars             = percent_rank(coalesce(Stars,0)),
      composite_score       = (pct_defensive_tfl*w[1]+pct_defensive_solo*w[2]+pct_defensive_tot*w[3]+
                                 pct_defensive_int*w[4]+pct_defensive_pd*w[5]+
                                 pct_stars*w[6])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"S"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="S",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             TFL=defensive_tfl,Solo=defensive_solo,Total=defensive_tot,
             Interceptions=interceptions_int,`Passes Defended`=defensive_pd,
             `Int Yds`=interceptions_yds,`Fumbles Rec`=fumbles_rec,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  k_scores <- eventReactive(input$recalc_k, {
    req(input$w_k_fgm,input$w_k_fga,input$w_k_pct,input$w_k_xpa_pct,input$w_k_stars)
    w <- norm_w(input$w_k_fgm,input$w_k_fga,input$w_k_pct,input$w_k_xpa_pct,input$w_k_stars)
    df <- transfer_stats %>% filter(Position=="K"); if(!nrow(df)) return(NULL)
    df <- df %>% mutate(
      pct_kicking_fgm = percent_rank(coalesce(kicking_fgm,0)*coalesce(sos_off_multiplier,1)),
      pct_kicking_fga = percent_rank(coalesce(kicking_fga,0)*coalesce(sos_off_multiplier,1)),
      pct_kicking_pct = percent_rank(coalesce(kicking_pct,0)*coalesce(sos_off_multiplier,1)),
      pct_xpm_pct     = percent_rank(if_else(!is.na(kicking_xpa)&kicking_xpa>0,
                                             coalesce(kicking_xpm,0)/kicking_xpa*coalesce(sos_off_multiplier,1),0)),
      pct_stars       = percent_rank(coalesce(Stars,0)),
      composite_score = (pct_kicking_fgm*w[1]+pct_kicking_fga*w[2]+
                           pct_kicking_pct*w[3]+pct_xpm_pct*w[4]+
                           pct_stars*w[5])*100*coalesce(conf_bonus,1))
    df <- cap_scores(df); df <- score_schemes(df,"K"); df <- estimate_nil(df)
    df %>% arrange(desc(composite_score)) %>% mutate(Position="K",Score=round(composite_score,1)) %>%
      select(Position,Photo=headshot_url,Player=full_name,From=Origin,To=Destination,
             Conference=conference,Stars,Eligibility,
             FGM=kicking_fgm,FGA=kicking_fga,`FG%`=kicking_pct,XPM=kicking_xpm,XPA=kicking_xpa,
             Score,`NIL Est.`=nil_display,`Best Scheme`=best_scheme,`Scheme Breakdown`=scheme_breakdown)
  }, ignoreNULL=FALSE)
  
  scored_for_pos <- function(pos) switch(pos,
                                         QB=calc_qb(), RB=calc_rb(), WR=calc_wr(), TE=calc_te(),
                                         EDGE=calc_edge(), DL=calc_dl(), LB=calc_lb(), CB=calc_cb(),
                                         S=s_scores(), K=k_scores())
  
  all_scores <- reactive({
    dfs <- lapply(c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"), scored_for_pos)
    dfs <- dfs[!sapply(dfs, is.null)]
    if(!length(dfs)) return(NULL)
    bind_rows(dfs) %>% distinct(Player, .keep_all=TRUE)
  })
  
  overall_data <- reactive({
    parts <- lapply(c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"), scored_for_pos)
    if(any(sapply(parts, is.null))) return(NULL)
    bind_rows(lapply(parts, \(d) d %>%
                       select(any_of(c("Position","Photo","Player","From","To","Conference","Stars","Score","Best Scheme"))))) %>%
      arrange(desc(Score)) %>% head(20) %>%
      mutate(Rank=row_number()) %>% select(Rank, everything())
  })
  
  # ============================================================
  # SIDEBAR
  # ============================================================
  output$dynamic_sidebar <- renderUI({
    tab <- input$main_tabs
    if(is.null(tab)||tab=="QB") {
      tagList(div(class="section-label","Quarterback Weights"),
              sliderInput("w_qb_ypa",  "Yards Per Attempt",  0,60,25,step=5,post="%"),
              sliderInput("w_qb_td",   "Passing TDs",        0,60,20,step=5,post="%"),
              sliderInput("w_qb_yds",  "Passing Yards",      0,60,15,step=5,post="%"),
              sliderInput("w_qb_pct",  "Completion %",       0,60,10,step=5,post="%"),
              sliderInput("w_qb_int",  "INT Rate (inverse)", 0,60,10,step=5,post="%"),
              sliderInput("w_qb_rush", "Rushing Yards",      0,60,10,step=5,post="%"),
              sliderInput("w_qb_stars","Star Rating",        0,60,10,step=5,post="%"),
              uiOutput("qb_total"), actionButton("recalc_qb","RECALCULATE",class="apply-btn"))
    } else if(tab=="RB") {
      tagList(div(class="section-label","Running Back Weights"),
              sliderInput("w_rb_ypc",     "Yards Per Carry",  0,60,20,step=5,post="%"),
              sliderInput("w_rb_yds",     "Rushing Yards",    0,60,25,step=5,post="%"),
              sliderInput("w_rb_car",     "Carries",          0,60,15,step=5,post="%"),
              sliderInput("w_rb_td",      "Rushing TDs",      0,60,15,step=5,post="%"),
              sliderInput("w_rb_long",    "Long Rush",        0,60,5, step=5,post="%"),
              sliderInput("w_rb_rec_yds", "Receiving Yards",  0,60,10,step=5,post="%"),
              sliderInput("w_rb_rec",     "Receptions",       0,60,5, step=5,post="%"),
              sliderInput("w_rb_stars",   "Star Rating",      0,60,5, step=5,post="%"),
              uiOutput("rb_total"), actionButton("recalc_rb","RECALCULATE",class="apply-btn"))
    } else if(tab=="WR") {
      tagList(div(class="section-label","Wide Receiver Weights"),
              sliderInput("w_wr_yds",  "Receiving Yards", 0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_wr_rec",  "Receptions",      0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_wr_ypr",  "Yards Per Route", 0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_wr_td",   "Receiving TDs",   0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_wr_stars","Star Rating",     0,60,10,step=5,post="%",width="100%"),
              uiOutput("wr_total"), actionButton("recalc_wr","RECALCULATE",class="apply-btn"))
    } else if(tab=="TE") {
      tagList(div(class="section-label","Tight End Weights"),
              sliderInput("w_te_td",   "Receiving TDs",   0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_te_yds",  "Receiving Yards", 0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_te_rec",  "Receptions",      0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_te_ypr",  "Yards Per Route", 0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_te_stars","Star Rating",     0,60,10,step=5,post="%",width="100%"),
              uiOutput("te_total"), actionButton("recalc_te","RECALCULATE",class="apply-btn"))
    } else if(tab=="EDGE") {
      tagList(div(class="section-label","Edge Rusher Weights"),
              sliderInput("w_edge_sacks","Sacks",           0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_edge_tfl",  "Tackles For Loss",0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_edge_hur",  "QB Hurries",      0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_edge_solo", "Solo Tackles",    0,60,15,step=5,post="%",width="100%"),
              sliderInput("w_edge_tot",  "Total Tackles",   0,60,10,step=5,post="%",width="100%"),
              sliderInput("w_edge_stars","Star Rating",     0,60,10,step=5,post="%",width="100%"),
              uiOutput("edge_total"), actionButton("recalc_edge","RECALCULATE",class="apply-btn"))
    } else if(tab=="DL") {
      tagList(div(class="section-label","D Lineman Weights"),
              sliderInput("w_dl_sacks","Sacks",           0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_dl_tfl",  "Tackles for Loss",0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_dl_hur",  "QB Hurries",      0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_dl_solo", "Solo Tackles",    0,60,15,step=5,post="%",width="100%"),
              sliderInput("w_dl_tot",  "Total Tackles",   0,60,10,step=5,post="%",width="100%"),
              sliderInput("w_dl_stars","Star Rating",     0,60,10,step=5,post="%",width="100%"),
              uiOutput("dl_total"), actionButton("recalc_dl","RECALCULATE",class="apply-btn"))
    } else if(tab=="LB") {
      tagList(div(class="section-label","Linebacker Weights"),
              sliderInput("w_lb_sacks",  "Sacks",              0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_lb_tfl",    "Tackles for Loss",   0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_lb_hur",    "QB Hurries",         0,60,15,step=5,post="%",width="100%"),
              sliderInput("w_lb_solo",   "Solo Tackles",       0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_lb_tot",    "Total Tackles",      0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_lb_fum_rec","Fumbles Recovered",  0,60,0, step=5,post="%",width="100%"),
              sliderInput("w_lb_int",    "Interceptions",      0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_lb_pd",     "Passes Defended",    0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_lb_stars",  "Star Rating",        0,60,10,step=5,post="%",width="100%"),
              uiOutput("lb_total"), actionButton("recalc_lb","RECALCULATE",class="apply-btn"))
    } else if(tab=="CB") {
      tagList(div(class="section-label","Cornerback Weights"),
              sliderInput("w_cb_tfl",    "Tackles for Loss",    0,60,15,step=5,post="%",width="100%"),
              sliderInput("w_cb_solo",   "Solo Tackles",        0,60,10,step=5,post="%",width="100%"),
              sliderInput("w_cb_tot",    "Total Tackles",       0,60,10,step=5,post="%",width="100%"),
              sliderInput("w_cb_int",    "Interceptions",       0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_cb_pd",     "Passes Defended",     0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_cb_int_yds","Interception Yards",  0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_cb_stars",  "Star Rating",         0,60,15,step=5,post="%",width="100%"),
              uiOutput("cb_total"), actionButton("recalc_cb","RECALCULATE",class="apply-btn"))
    } else if(tab=="S") {
      tagList(div(class="section-label","Safety Weights"),
              sliderInput("w_s_tfl",  "Tackles for Loss", 0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_s_solo", "Solo Tackles",     0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_s_tot",  "Total Tackles",    0,60,25,step=5,post="%",width="100%"),
              sliderInput("w_s_int",  "Interceptions",    0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_s_pd",   "Passes Defended",  0,60,20,step=5,post="%",width="100%"),
              sliderInput("w_s_stars","Star Rating",      0,60,10,step=5,post="%",width="100%"),
              uiOutput("s_total"), actionButton("recalc_s","RECALCULATE",class="apply-btn"))
    } else if(tab=="K") {
      tagList(div(class="section-label","Kicker Weights"),
              sliderInput("w_k_fgm",    "Field Goals Made",       0,60,15,step=5,post="%",width="100%"),
              sliderInput("w_k_fga",    "Field Goals Attempted",  0,60,30,step=5,post="%",width="100%"),
              sliderInput("w_k_pct",    "Field Goal Percentage",  0,60,40,step=5,post="%",width="100%"),
              sliderInput("w_k_xpa_pct","Extra Point Percentage", 0,60,5, step=5,post="%",width="100%"),
              sliderInput("w_k_stars",  "Star Rating",            0,60,10,step=5,post="%",width="100%"),
              uiOutput("k_total"), actionButton("recalc_k","RECALCULATE",class="apply-btn"))
    } else if(tab=="VIS") {
      tagList(div(class="section-label","Visuals Controls"),
              sliderInput("top_n","Top N players",min=10,max=50,value=25,step=5),
              selectInput("vis_pos_filter","Position filter",
                          choices=c("All","QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"),selected="All"),
              div(class="overall-note","Filter applies to top-overall, market efficiency, and origin-conference views."))
    } else if(tab=="NIL") {
      tagList(div(class="section-label","NIL Controls"),
              div(class="overall-note","Use the filters in the NIL tab to slice the market."))
    } else {
      tagList(div(class="section-label", paste(tab,"controls")))
    }
  })
  
  # Weight totals
  output$qb_total   <- renderUI({ req(input$w_qb_ypa);    make_total_ui(input$w_qb_ypa+input$w_qb_td+input$w_qb_yds+input$w_qb_pct+input$w_qb_int+input$w_qb_rush+input$w_qb_stars) })
  output$rb_total   <- renderUI({ req(input$w_rb_ypc);    make_total_ui(input$w_rb_ypc+input$w_rb_yds+input$w_rb_car+input$w_rb_td+input$w_rb_long+input$w_rb_rec_yds+input$w_rb_rec+input$w_rb_stars) })
  output$wr_total   <- renderUI({ req(input$w_wr_yds);    make_total_ui(input$w_wr_yds+input$w_wr_rec+input$w_wr_ypr+input$w_wr_td+input$w_wr_stars) })
  output$te_total   <- renderUI({ req(input$w_te_td);     make_total_ui(input$w_te_td+input$w_te_yds+input$w_te_rec+input$w_te_ypr+input$w_te_stars) })
  output$edge_total <- renderUI({ req(input$w_edge_sacks);make_total_ui(input$w_edge_sacks+input$w_edge_tfl+input$w_edge_hur+input$w_edge_solo+input$w_edge_tot+input$w_edge_stars) })
  output$dl_total   <- renderUI({ req(input$w_dl_sacks);  make_total_ui(input$w_dl_sacks+input$w_dl_tfl+input$w_dl_hur+input$w_dl_solo+input$w_dl_tot+input$w_dl_stars) })
  output$lb_total   <- renderUI({ req(input$w_lb_sacks);  make_total_ui(input$w_lb_sacks+input$w_lb_tfl+input$w_lb_hur+input$w_lb_solo+input$w_lb_tot+input$w_lb_fum_rec+input$w_lb_int+input$w_lb_pd+input$w_lb_stars) })
  output$cb_total   <- renderUI({ req(input$w_cb_tfl);    make_total_ui(input$w_cb_tfl+input$w_cb_solo+input$w_cb_tot+input$w_cb_int+input$w_cb_pd+input$w_cb_int_yds+input$w_cb_stars) })
  output$s_total    <- renderUI({ req(input$w_s_tfl);     make_total_ui(input$w_s_tfl+input$w_s_solo+input$w_s_tot+input$w_s_int+input$w_s_pd+input$w_s_stars) })
  output$k_total    <- renderUI({ req(input$w_k_fgm);     make_total_ui(input$w_k_fgm+input$w_k_fga+input$w_k_pct+input$w_k_xpa_pct+input$w_k_stars) })
  
  # ============================================================
  # TABLES
  # ============================================================
  output$tbl_qb      <- renderDT({ make_dt(calc_qb()) })
  output$tbl_rb      <- renderDT({ make_dt(calc_rb()) })
  output$tbl_wr      <- renderDT({ make_dt(calc_wr()) })
  output$tbl_te      <- renderDT({ make_dt(calc_te()) })
  output$tbl_edge    <- renderDT({ make_dt(calc_edge()) })
  output$tbl_dl      <- renderDT({ make_dt(calc_dl()) })
  output$tbl_lb      <- renderDT({ make_dt(calc_lb()) })
  output$tbl_cb      <- renderDT({ make_dt(calc_cb()) })
  output$tbl_s       <- renderDT({ make_dt(s_scores()) })
  output$tbl_k       <- renderDT({ make_dt(k_scores()) })
  output$tbl_overall <- renderDT({ make_dt(overall_data()) })
  
  # ============================================================
  # PER-POSITION ANALYTICS
  # ============================================================
  make_pos_plots <- function(pos) {
    lp  <- tolower(pos)
    cfg <- viz_cfg[[pos]]
    raw_df    <- reactive({ transfer_stats %>% filter(Position == pos) })
    scored_df <- reactive({ scored_for_pos(pos) })
    
    output[[paste0("viz_",lp,"_bar")]] <- renderPlot({
      df <- raw_df(); req(nrow(df)>0); col <- cfg$bar_col; req(col %in% names(df))
      df2 <- df %>% filter(!is.na(.data[[col]])) %>% arrange(desc(.data[[col]])) %>% head(15) %>%
        mutate(lbl=coalesce(full_name, as.character(row_number())))
      ggplot(df2,aes(x=reorder(lbl,.data[[col]]),y=.data[[col]]))+geom_col(fill=CU_GOLD,width=.7)+
        geom_text(aes(label=round(.data[[col]],1)),hjust=-.1,color=CU_TEXT,size=3)+coord_flip(clip="off")+
        labs(title=paste("Top 15 —",cfg$bar_label),subtitle=paste(pos,"pool"),x=NULL,y=cfg$bar_label)+
        theme_cu()+theme(plot.margin=margin(10,50,10,10))
    })
    output[[paste0("viz_",lp,"_scat")]] <- renderPlot({
      df <- raw_df(); req(nrow(df)>0); xc <- cfg$scat_x; yc <- cfg$scat_y; req(all(c(xc,yc)%in%names(df)))
      df2 <- df %>% filter(!is.na(.data[[xc]]),!is.na(.data[[yc]])) %>% mutate(lbl=coalesce(full_name,""))
      ggplot(df2,aes(x=.data[[xc]],y=.data[[yc]]))+geom_point(color=CU_GOLD,alpha=.75,size=3)+
        geom_smooth(method="lm",se=FALSE,color=CU_TEAL,linewidth=.8,linetype="dashed")+
        geom_text_repel(aes(label=lbl),color=CU_TEXT,size=2.6,max.overlaps=10,segment.color="#444",segment.size=.3)+
        labs(title=paste(cfg$scat_xl,"vs",cfg$scat_yl),subtitle="Dashed = linear trend",x=cfg$scat_xl,y=cfg$scat_yl)+
        theme_cu()
    })
    output[[paste0("viz_",lp,"_dist")]] <- renderPlot({
      df <- scored_df(); req(!is.null(df),nrow(df)>0)
      ggplot(df,aes(x=Score))+geom_histogram(fill=CU_GOLD,bins=25,color="#000",alpha=.9)+
        geom_density(aes(y=after_stat(count)),color="#ffffff",linewidth=.7)+
        labs(title="Composite score distribution",subtitle=paste(pos,"— all portal players"),x="Score (0-99)",y="Count")+
        theme_cu()
    })
    output[[paste0("viz_",lp,"_stars")]] <- renderPlot({
      df <- raw_df(); req(nrow(df)>0)
      star_pal <- c("1"="#444441","2"="#888780","3"="#EF9F27","4"=CU_TEAL,"5"=CU_GOLD)
      df2 <- df %>% mutate(Stars=factor(coalesce(Stars,0L))) %>% count(Stars) %>% filter(Stars%in%names(star_pal))
      ggplot(df2,aes(x=Stars,y=n,fill=Stars))+geom_col(width=.6)+
        geom_text(aes(label=n),vjust=-.4,color=CU_TEXT,size=3.5)+
        scale_fill_manual(values=star_pal,guide="none")+
        labs(title="Recruiting stars breakdown",subtitle=paste(pos,"transfer pool"),x="Stars",y="Players")+
        theme_cu()
    })
    output[[paste0("sel_",lp,"_player")]] <- renderUI({
      df <- raw_df(); req(nrow(df)>0)
      selectInput(paste0("drill_",lp),"Select player",
                  choices=sort(unique(df$full_name)),selected=sort(unique(df$full_name))[1])
    })
    output[[paste0("viz_",lp,"_pctile")]] <- renderPlot({
      df <- raw_df(); req(nrow(df)>0); pid <- input[[paste0("drill_",lp)]]; req(pid)
      cols   <- cfg$pctile_cols[cfg$pctile_cols%in%names(df)]
      labels <- cfg$pctile_labels[cfg$pctile_cols%in%names(df)]; req(length(cols)>0)
      pct_df <- df %>% mutate(across(all_of(cols),~percent_rank(coalesce(.x,0))*100)) %>%
        filter(full_name==pid) %>% select(all_of(cols)) %>% slice(1); req(nrow(pct_df)>0)
      pd <- tibble(metric=factor(labels,levels=rev(labels)),pctile=as.numeric(pct_df)) %>%
        mutate(clr=case_when(pctile>=75~CU_GOLD,pctile>=50~CU_TEAL,pctile>=25~CU_BLUE,TRUE~"#888780"))
      ggplot(pd,aes(x=metric,y=pctile,fill=clr))+geom_col(width=.6)+
        geom_text(aes(label=paste0(round(pctile),"th")),hjust=-.1,color=CU_TEXT,size=3)+
        scale_fill_identity()+coord_flip(clip="off")+ylim(0,118)+
        labs(title="Percentile rank",subtitle=pid,x=NULL,y="Percentile")+
        theme_cu()+theme(plot.margin=margin(10,40,10,10))
    })
    output[[paste0("viz_",lp,"_rel")]] <- renderPlot({
      df <- raw_df(); req(nrow(df)>0); pid <- input[[paste0("drill_",lp)]]; req(pid)
      cols   <- cfg$pctile_cols[cfg$pctile_cols%in%names(df)]
      labels <- cfg$pctile_labels[cfg$pctile_cols%in%names(df)]; req(length(cols)>0)
      avgs   <- df %>% summarise(across(all_of(cols),~mean(coalesce(.x,0),na.rm=TRUE)))
      player <- df %>% filter(full_name==pid) %>% select(all_of(cols)) %>% slice(1); req(nrow(player)>0)
      rel <- as.numeric(player)/pmax(as.numeric(avgs),1e-9)
      pd  <- tibble(metric=factor(labels,levels=rev(labels)),rel=round(rel,2),
                    clr=if_else(rel>=1,CU_GOLD,"#888780"))
      ggplot(pd,aes(x=metric,y=rel,fill=clr))+geom_col(width=.6)+
        geom_hline(yintercept=1,linetype="dashed",color=CU_TEXT,linewidth=.5)+
        geom_text(aes(label=paste0(round(rel,2),"x")),hjust=-.1,color=CU_TEXT,size=3)+
        scale_fill_identity()+coord_flip(clip="off")+
        ylim(0,max(rel,1.5,na.rm=TRUE)*1.25)+
        labs(title="vs position group avg",subtitle="1.0x = exactly average",x=NULL,y="Relative to avg")+
        theme_cu()+theme(plot.margin=margin(10,40,10,10))
    })
  }
  
  for (.pos in c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K")) {
    local({ pos <- .pos; make_pos_plots(pos) })
  }
  
  # ============================================================
  # OVERVIEW VISUALS
  # ============================================================
  output$viz_top_overall <- renderPlot({
    df <- all_scores(); req(!is.null(df),nrow(df)>0)
    if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")
      df <- df %>% filter(Position==input$vis_pos_filter)
    req(nrow(df)>0)
    n   <- if(!is.null(input$top_n)) input$top_n else 25
    df2 <- df %>% arrange(desc(Score)) %>% head(n) %>%
      mutate(label=paste0(Player," · ",Position))
    ggplot(df2,aes(x=reorder(label,Score),y=Score,fill=Position))+geom_col(width=.78)+
      geom_text(aes(label=sprintf("%.1f",Score)),hjust=-.18,color=CU_TEXT,size=3)+
      scale_fill_manual(values=pos_palette,name="Position")+
      coord_flip(clip="off")+ylim(0,max(df2$Score,na.rm=TRUE)*1.10)+
      labs(title=paste("Top",n,"portal players overall"),
           subtitle=if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")
             paste("Filtered to",input$vis_pos_filter) else "Cross-position composite ranking",
           x=NULL,y="Composite score (0-99)")+
      theme_cu()+theme(plot.margin=margin(10,55,10,10),legend.position="bottom",
                       legend.box="horizontal",legend.key.size=unit(.4,"cm"))
  })
  
  nil_data_all <- reactive({
    df <- transfer_stats %>%
      filter(case_when(
        Position=="QB"           ~ !is.na(passing_att)  &passing_att>=50,
        Position=="RB"           ~ !is.na(rushing_car)  &rushing_car>=50,
        Position%in%c("WR","TE") ~ !is.na(receiving_rec)&receiving_rec>=10,
        TRUE ~ TRUE)) %>%
      mutate(composite_score = case_when(
        Position=="QB" ~ pmin((percent_rank(coalesce(passing_ypa,0))+
                                 percent_rank(coalesce(passing_td,0)*coalesce(sos_off_multiplier,1))+
                                 percent_rank(coalesce(passing_yds,0)*coalesce(sos_off_multiplier,1))+
                                 percent_rank(coalesce(passing_pct,0))+
                                 (1-percent_rank(coalesce(passing_int,0)/pmax(coalesce(passing_att,1),1)))+
                                 percent_rank(coalesce(rushing_yds,0)*coalesce(sos_off_multiplier,1)))/6*100,99),
        Position=="RB" ~ pmin((percent_rank(coalesce(rushing_ypc,0))+
                                 percent_rank(coalesce(rushing_yds,0)*coalesce(sos_off_multiplier,1))+
                                 percent_rank(coalesce(rushing_td,0)*coalesce(sos_off_multiplier,1))+
                                 percent_rank(coalesce(receiving_yds,0)*coalesce(sos_off_multiplier,1))+
                                 percent_rank(coalesce(receiving_rec,0)*coalesce(sos_off_multiplier,1)))/5*100,99),
        Position%in%c("WR","TE") ~ pmin((percent_rank(coalesce(receiving_yds,0)*coalesce(sos_off_multiplier,1))+
                                           percent_rank(coalesce(receiving_rec,0)*coalesce(sos_off_multiplier,1))+
                                           percent_rank(coalesce(receiving_ypr,0))+
                                           percent_rank(coalesce(receiving_td,0)*coalesce(sos_off_multiplier,1)))/4*100,99),
        Position%in%c("EDGE","DL") ~ pmin((percent_rank(coalesce(defensive_sacks,0)*coalesce(sos_def_multiplier,1))+
                                             percent_rank(coalesce(defensive_tfl,0)*coalesce(sos_def_multiplier,1))+
                                             percent_rank(coalesce(defensive_qb_hur,0)*coalesce(sos_def_multiplier,1)))/3*100,99),
        Position=="LB" ~ pmin((percent_rank(coalesce(defensive_tot,0)*coalesce(sos_def_multiplier,1))+
                                 percent_rank(coalesce(defensive_tfl,0)*coalesce(sos_def_multiplier,1))+
                                 percent_rank(coalesce(defensive_pd,0)*coalesce(sos_def_multiplier,1)))/3*100,99),
        Position%in%c("CB","S") ~ pmin((percent_rank(coalesce(defensive_pd,0)*coalesce(sos_def_multiplier,1))+
                                          percent_rank(coalesce(interceptions_int,0)*coalesce(sos_def_multiplier,1))+
                                          percent_rank(coalesce(defensive_tot,0)*coalesce(sos_def_multiplier,1)))/3*100,99),
        Position=="K" ~ pmin((percent_rank(coalesce(kicking_pct,0))+
                                percent_rank(coalesce(kicking_fgm,0)))/2*100,99),
        TRUE ~ 50))
    df <- estimate_nil(df)
    df %>% select(Position, Player=full_name, From=Origin, To=Destination,
                  Conference=conference, Stars, Eligibility,
                  `NIL Est.`=nil_display, nil_value,
                  `SP+ Rank`=origin_sp_ranking, Score=composite_score) %>%
      arrange(desc(nil_value))
  })
  
  score_vs_nil_data <- reactive({
    df <- nil_data_all(); req(!is.null(df),nrow(df)>0)
    df2 <- df %>% filter(!is.na(Score),!is.na(nil_value),Score>0,Position%in%names(pos_palette))
    if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")
      df2 <- df2 %>% filter(Position==input$vis_pos_filter)
    req(nrow(df2)>0)
    df2 %>% mutate(nil_value_k=nil_value/1000,
                   value_score=Score/pmax(nil_value/100000,.5))
  })
  
  output$viz_score_vs_nil <- renderPlot({
    df2 <- score_vs_nil_data()
    score_cut <- stats::quantile(df2$Score,.75,na.rm=TRUE)
    bargains  <- df2 %>% filter(Score>=score_cut) %>% arrange(desc(value_score)) %>% head(8)
    sub <- if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")
      paste0(input$vis_pos_filter," only · click any dot for details")
    else "Click a dot for details · upper-left = bargains · gold = top value targets"
    ggplot(df2,aes(x=nil_value_k,y=Score))+
      geom_point(aes(color=Position),alpha=.55,size=2.5)+
      geom_text_repel(data=bargains,aes(label=Player),color=CU_GOLD,size=3,
                      max.overlaps=20,segment.color="#888",segment.size=.3,box.padding=.45,seed=1)+
      scale_color_manual(values=pos_palette,name="Position",
                         guide=if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")"none" else "legend")+
      scale_x_continuous(labels=dollar_format(suffix="K",prefix="$",scale=1))+
      labs(title="Market efficiency — score vs NIL",subtitle=sub,
           x="Estimated annual NIL",y="Value score (0-99)")+
      theme_cu()+theme(legend.position="bottom",legend.box="horizontal",legend.key.size=unit(.4,"cm"))
  })
  
  output$viz_score_vs_nil_clicked <- renderUI({
    click <- input$viz_score_vs_nil_click
    if(is.null(click)) return(div(class="overall-note","Click any point to see player details"))
    np <- nearPoints(score_vs_nil_data(),click,xvar="nil_value_k",yvar="Score",maxpoints=1,threshold=15)
    if(nrow(np)==0) return(div(class="overall-note","No player near that click — try clicking closer to a dot"))
    p <- np[1,]
    pc <- pos_palette[[as.character(p$Position)]]; if(is.null(pc)) pc <- CU_GOLD
    div(style=paste0("background:#111;border:1px solid #222;border-left:4px solid ",pc,";border-radius:4px;padding:12px 14px;margin-top:8px"),
        div(style=paste0("font-family:'Oswald',sans-serif;font-size:16px;font-weight:700;color:",pc,";letter-spacing:1px;margin-bottom:6px"),
            paste0(p$Player," · ",p$Position)),
        div(style="display:flex;flex-wrap:wrap;gap:14px;font-size:12px;color:#ccc",
            div(tags$span(style="color:#888","Score: "),tags$span(style=paste0("color:",CU_GOLD,";font-weight:700"),sprintf("%.1f",p$Score))),
            div(tags$span(style="color:#888","NIL Est.: "),tags$span(style="color:#4caf50;font-weight:700",p$`NIL Est.`)),
            div(tags$span(style="color:#888","Stars: "),tags$span(style="color:#ddd",paste0(coalesce(as.character(p$Stars),"?"),"★"))),
            div(tags$span(style="color:#888","From: "),tags$span(style="color:#ddd",coalesce(as.character(p$From),"—"))),
            div(tags$span(style="color:#888","To: "),tags$span(style="color:#ddd",coalesce(as.character(p$To),"—"))),
            div(tags$span(style="color:#888","Conference: "),tags$span(style="color:#ddd",coalesce(as.character(p$Conference),"—")))
        )
    )
  })
  
  output$viz_pos_depth <- renderPlot({
    df <- all_scores(); req(!is.null(df),nrow(df)>0)
    df <- df %>% filter(Position%in%names(pos_palette))
    pos_order <- df %>% group_by(Position) %>%
      summarise(med=median(Score,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(med)) %>% pull(Position)
    counts <- df %>% count(Position)
    df$Position <- factor(df$Position,levels=rev(pos_order))
    ggplot(df,aes(x=Position,y=Score,fill=Position))+
      geom_violin(alpha=.45,color=NA,scale="width")+
      geom_boxplot(width=.22,alpha=.85,outlier.color=CU_GOLD,outlier.size=1.2,color="#1a1a1a")+
      stat_summary(fun=median,geom="point",color=CU_GOLD,size=2.4)+
      geom_text(data=counts,aes(x=Position,y=2,label=paste0("n=",n)),color="#888",size=2.8,inherit.aes=FALSE)+
      scale_fill_manual(values=pos_palette,guide="none")+coord_flip()+
      labs(title="Talent depth by position",
           subtitle="Wide violin = deep pool · gold dot = median · n = pool size",
           x=NULL,y="Composite score (0-99)")+theme_cu()
  })
  
  output$viz_origin_conf <- renderPlot({
    df <- all_scores(); req(!is.null(df),nrow(df)>0)
    if(!is.null(input$vis_pos_filter)&&input$vis_pos_filter!="All")
      df <- df %>% filter(Position==input$vis_pos_filter)
    req(nrow(df)>0)
    n     <- if(!is.null(input$top_n)) input$top_n else 25
    topN  <- df %>% arrange(desc(Score)) %>% head(min(n*2,nrow(df)))
    conf_summary <- topN %>% filter(!is.na(Conference),Conference!="") %>%
      group_by(Conference) %>%
      summarise(n_players=n(),avg_score=mean(Score,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(n_players)) %>% head(12)
    req(nrow(conf_summary)>0)
    ggplot(conf_summary,aes(x=reorder(Conference,n_players),y=n_players,fill=avg_score))+
      geom_col(width=.7)+
      geom_text(aes(label=paste0(n_players," (",sprintf("%.1f",avg_score),")")),
                hjust=-.15,color=CU_TEXT,size=3)+
      scale_fill_gradient(low="#1D9E75",high=CU_GOLD,name="Avg Score")+
      coord_flip(clip="off")+ylim(0,max(conf_summary$n_players)*1.30)+
      labs(title="Top player origin conferences",
           subtitle="Where elite transfer talent is coming from · (count, avg score)",
           x=NULL,y="Top players from this conference")+
      theme_cu()+theme(plot.margin=margin(10,40,10,10),legend.position="right",
                       legend.key.height=unit(.6,"cm"),legend.key.width=unit(.3,"cm"))
  })
  
  # ============================================================
  # SCHEME FIT
  # ============================================================
  output$scheme_filter_ui <- renderUI({
    pos <- req(input$scheme_pos)
    selectInput("scheme_filter","Filter by scheme",
                choices=c("All",names(scheme_weights[[pos]])),selected="All")
  })
  scheme_filtered <- reactive({
    df <- scored_for_pos(req(input$scheme_pos)); req(!is.null(df),nrow(df)>0)
    if(!is.null(input$scheme_filter)&&input$scheme_filter!="All")
      df <- df %>% filter(`Best Scheme`==input$scheme_filter)
    df
  })
  output$viz_scheme_bars <- renderPlot({
    df <- scheme_filtered(); req(nrow(df)>0)
    ggplot(df %>% head(15),aes(x=reorder(Player,Score),y=Score,fill=`Best Scheme`))+geom_col()+
      geom_text(aes(label=`Best Scheme`),hjust=-.05,size=2.8,color=CU_TEXT)+
      coord_flip(clip="off")+scale_fill_brewer(palette="Set2")+
      labs(x=NULL,y="Score",fill="Best Scheme")+
      theme_cu()+theme(plot.margin=margin(10,130,10,10))
  })
  output$tbl_scheme <- renderDT({
    df <- scheme_filtered(); req(nrow(df)>0)
    make_dt(df %>% select(any_of(c("Photo","Player","From","To","Conference","Stars","Score","Best Scheme","Scheme Breakdown"))))
  })
  
  # ============================================================
  # PLAYER RADAR
  # ============================================================
  pos_metrics <- list(
    QB   = c("passing_ypa","passing_td","passing_yds","passing_pct","passing_int","rushing_yds"),
    RB   = c("rushing_ypc","rushing_yds","rushing_car","rushing_td","rushing_long","receiving_yds","receiving_rec"),
    WR   = c("receiving_yds","receiving_rec","receiving_ypr","receiving_td","receiving_long"),
    TE   = c("receiving_yds","receiving_rec","receiving_ypr","receiving_td","receiving_long"),
    EDGE = c("defensive_sacks","defensive_tfl","defensive_qb_hur","defensive_solo","defensive_tot"),
    DL   = c("defensive_tfl","defensive_sacks","defensive_qb_hur","defensive_solo","defensive_tot"),
    LB   = c("defensive_tfl","defensive_solo","defensive_qb_hur","defensive_tot","interceptions_int"),
    CB   = c("defensive_tfl","defensive_solo","defensive_tot","interceptions_int","defensive_pd"),
    S    = c("defensive_tfl","defensive_solo","defensive_tot","interceptions_int","defensive_pd")
  )
  output$player_selector <- renderUI({
    pos <- req(input$radar_pos)
    pl  <- transfer_stats %>% filter(Position==pos) %>% pull(full_name) %>% unique() %>% sort()
    selectInput("radar_player","Player",choices=pl,selected=pl[1])
  })
  output$player_radar <- renderPlot({
    pos <- req(input$radar_pos); player <- req(input$radar_player)
    metrics <- pos_metrics[[pos]]; df_pos <- transfer_stats %>% filter(Position==pos)
    metrics <- metrics[metrics%in%names(df_pos)]; if(length(metrics)<3){plot.new();return()}
    df_n <- df_pos %>% mutate(across(all_of(metrics),~percent_rank(coalesce(.x,0)))) %>% select(full_name,all_of(metrics))
    if(pos=="QB"&&"passing_int"%in%metrics) df_n <- df_n %>% mutate(passing_int=1-passing_int)
    pr <- df_n %>% filter(full_name==player); if(!nrow(pr)){plot.new();return()}
    cd <- rbind(rep(1,length(metrics)),rep(0,length(metrics)),pr %>% select(all_of(metrics)))
    colnames(cd)<-metrics; rownames(cd)<-c("max","min",player)
    op <- par(mar=c(1,1,3,1),bg=CU_BG)
    fmsb::radarchart(cd,axistype=1,pcol=CU_GOLD,pfcol=alpha(CU_GOLD,.35),plwd=2,
                     cglcol="grey30",cglty=1,axislabcol="grey80",caxislabels=seq(0,1,.25),vlcex=.9)
    title(main=paste(player,"-",pos),col.main=CU_GOLD); par(op)
  })
  
  # ============================================================
  # COMPARE PLAYERS
  # ============================================================
  output$cmp_player_a_ui <- renderUI({
    pos <- req(input$cmp_pos)
    players <- transfer_stats %>% filter(Position==pos) %>% pull(full_name) %>% unique() %>% sort()
    selectInput("cmp_player_a","Player A",choices=players,selected=players[1])
  })
  output$cmp_player_b_ui <- renderUI({
    pos <- req(input$cmp_pos)
    players <- transfer_stats %>% filter(Position==pos) %>% pull(full_name) %>% unique() %>% sort()
    selectInput("cmp_player_b","Player B",choices=players,selected=if(length(players)>=2) players[2] else players[1])
  })
  h2h_data <- reactive({
    pos <- req(input$cmp_pos); pa <- req(input$cmp_player_a); pb <- req(input$cmp_player_b)
    req(pa!=pb)
    df <- transfer_stats %>% filter(Position==pos,full_name%in%c(pa,pb)); req(nrow(df)>=1); df
  })
  h2h_scores <- reactive({
    pos <- req(input$cmp_pos); pa <- req(input$cmp_player_a); pb <- req(input$cmp_player_b)
    sd <- scored_for_pos(pos)
    if(is.null(sd)) return(tibble(Player=character(),Score=numeric()))
    sd %>% filter(Player%in%c(pa,pb)) %>% select(Player,Score) %>% distinct(Player,.keep_all=TRUE)
  })
  output$h2h_scorecard <- renderUI({
    pos <- req(input$cmp_pos); pa <- req(input$cmp_player_a); pb <- req(input$cmp_player_b); req(pa!=pb)
    cfg <- h2h_metrics[[pos]]; if(is.null(cfg)) return(div("No scorecard config for this position."))
    df <- h2h_data()
    row_a <- df %>% filter(full_name==pa) %>% slice(1)
    row_b <- df %>% filter(full_name==pb) %>% slice(1)
    if(nrow(row_a)==0||nrow(row_b)==0) return(div("Players not found."))
    sc <- h2h_scores()
    score_a <- sc %>% filter(Player==pa) %>% pull(Score); score_a <- if(length(score_a)) round(score_a[1],1) else NA
    score_b <- sc %>% filter(Player==pb) %>% pull(Score); score_b <- if(length(score_b)) round(score_b[1],1) else NA
    hs_a <- paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/",row_a$athlete_id,".png&w=96&h=70&cb=1")
    hs_b <- paste0("https://a.espncdn.com/combiner/i?img=/i/headshots/college-football/players/full/",row_b$athlete_id,".png&w=96&h=70&cb=1")
    wins_a <- 0; wins_b <- 0
    metric_rows <- purrr::pmap(list(cfg$cols,cfg$labels,cfg$higher), function(col,label,higher_is_better) {
      val_a <- if(col%in%names(row_a)) coalesce(as.numeric(row_a[[col]]),0) else NA
      val_b <- if(col%in%names(row_b)) coalesce(as.numeric(row_b[[col]]),0) else NA
      if(is.na(val_a)&&is.na(val_b)){a_wins<-FALSE;b_wins<-FALSE
      }else if(is.na(val_a)){a_wins<-FALSE;b_wins<-TRUE
      }else if(is.na(val_b)){a_wins<-TRUE;b_wins<-FALSE
      }else if(higher_is_better){a_wins<-val_a>val_b;b_wins<-val_b>val_a
      }else{a_wins<-val_a<val_b;b_wins<-val_b<val_a}
      if(a_wins) wins_a<<-wins_a+1
      if(b_wins) wins_b<<-wins_b+1
      mx <- max(c(val_a,val_b),na.rm=TRUE)
      pct_a <- if(!is.na(val_a)&&mx>0) round(val_a/mx*100) else 0
      pct_b <- if(!is.na(val_b)&&mx>0) round(val_b/mx*100) else 0
      fmt <- function(v) if(is.na(v))"—" else if(v==round(v)) formatC(v,format="d",big.mark=",") else round(v,1)
      div(
        div(class="h2h-row",
            div(class=paste("h2h-val h2h-val-left",if(a_wins)"winner"else""),fmt(val_a)),
            div(class="h2h-metric",label),
            div(class=paste("h2h-val h2h-val-right",if(b_wins)"winner"else""),fmt(val_b))),
        div(class="h2h-bar-row",
            div(class="h2h-bar-wrap",style="text-align:right",
                div(style="display:flex;justify-content:flex-end",
                    div(class="h2h-bar-outer",style="width:100%",
                        div(class=paste("h2h-bar-inner",if(!a_wins&&!is.na(val_b)&&val_b>val_a)"loser"else""),
                            style=paste0("width:",pct_a,"%"))))),
            div(),
            div(class="h2h-bar-wrap",
                div(class="h2h-bar-outer",style="width:100%",
                    div(class=paste("h2h-bar-inner",if(!b_wins&&!is.na(val_a)&&val_a>val_b)"loser"else""),
                        style=paste0("width:",pct_b,"%")))))
      )
    })
    overall_winner <- if(!is.na(score_a)&&!is.na(score_b)){
      if(score_a>score_b) pa else if(score_b>score_a) pb else "TIE"
    } else if(wins_a>wins_b) pa else if(wins_b>wins_a) pb else "TIE"
    fa_cls <- if(overall_winner==pa)"h2h-footer-val winner" else "h2h-footer-val"
    fb_cls <- if(overall_winner==pb)"h2h-footer-val winner" else "h2h-footer-val"
    div(class="h2h-card",
        div(class="h2h-header",
            div(class="h2h-player",
                tags$img(src=hs_a,class="h2h-img",onerror="this.style.display='none'"),
                div(class="h2h-player-name",pa),
                div(class="h2h-player-sub",paste0(coalesce(row_a$conference,"")," · ",coalesce(as.character(row_a$Stars),"?"),"★"))),
            div(class="h2h-vs","VS"),
            div(class="h2h-player",
                tags$img(src=hs_b,class="h2h-img",onerror="this.style.display='none'"),
                div(class="h2h-player-name",pb),
                div(class="h2h-player-sub",paste0(coalesce(row_b$conference,"")," · ",coalesce(as.character(row_b$Stars),"?"),"★")))),
        do.call(tagList, metric_rows),
        div(class="h2h-footer",
            div(class=fa_cls,if(overall_winner==pa)"✓ EDGE  |  "else"",
                if(!is.na(score_a))paste0("Score: ",score_a)else paste0(wins_a," wins")),
            div(class="h2h-footer-mid",if(overall_winner=="TIE")"TIE"else
              paste0(if(overall_winner==pa)wins_a else wins_b," – ",if(overall_winner==pa)wins_b else wins_a)),
            div(class=fb_cls,if(!is.na(score_b))paste0("Score: ",score_b)else paste0(wins_b," wins"),
                if(overall_winner==pb)"  |  ✓ EDGE"else""))
    )
  })
  output$cmp_radar <- renderPlot({
    pos <- req(input$cmp_pos); pa <- req(input$cmp_player_a); pb <- req(input$cmp_player_b); req(pa!=pb)
    metrics <- pos_metrics[[pos]]; df_pos <- transfer_stats %>% filter(Position==pos)
    metrics <- metrics[metrics%in%names(df_pos)]; req(length(metrics)>=3)
    df_n <- df_pos %>% mutate(across(all_of(metrics),~percent_rank(coalesce(.x,0)))) %>% select(full_name,all_of(metrics))
    pr   <- df_n %>% filter(full_name%in%c(pa,pb)); if(!nrow(pr)){plot.new();return()}
    cd <- rbind(rep(1,length(metrics)),rep(0,length(metrics)),as.data.frame(pr %>% select(all_of(metrics))))
    colnames(cd)<-metrics; rownames(cd)<-c("max","min",pr$full_name)
    cols <- c(CU_GOLD,"#378ADD")
    op <- par(mar=c(1,1,3,1),bg=CU_BG)
    fmsb::radarchart(cd,axistype=1,pcol=cols,pfcol=alpha(cols,.20),plwd=2,
                     cglcol="grey30",cglty=1,axislabcol="grey80",caxislabels=seq(0,1,.25),vlcex=.9)
    legend("topright",legend=pr$full_name,col=cols,pch=16,horiz=TRUE,bty="n",cex=.85); par(op)
  })
  output$cmp_table <- renderDT({
    pos <- req(input$cmp_pos); pa <- req(input$cmp_player_a); pb <- req(input$cmp_player_b)
    metrics <- pos_metrics[[pos]]; df_pos <- transfer_stats %>% filter(Position==pos)
    metrics <- metrics[metrics%in%names(df_pos)]; req(length(metrics)>=3)
    df_n <- df_pos %>% mutate(across(all_of(metrics),~percent_rank(coalesce(.x,0)))) %>%
      select(full_name,all_of(metrics)) %>% filter(full_name%in%c(pa,pb)) %>%
      pivot_longer(all_of(metrics),names_to="metric",values_to="norm") %>%
      pivot_wider(names_from=full_name,values_from=norm)
    datatable(df_n,options=list(pageLength=20),rownames=FALSE)
  })
  
  # ============================================================
  # NIL ESTIMATOR
  # ============================================================
  nil_filtered <- reactive({
    df <- nil_data_all(); req(!is.null(df),nrow(df)>0)
    if(!is.null(input$nil_pos)&&input$nil_pos!="All")   df <- df %>% filter(Position==input$nil_pos)
    if(!is.null(input$nil_conf)&&input$nil_conf!="All") df <- df %>% filter(Conference==input$nil_conf)
    if(!is.null(input$nil_stars))  df <- df %>% filter(!is.na(Stars),Stars>=input$nil_stars)
    if(!is.null(input$nil_budget)) df <- df %>% filter(nil_value<=input$nil_budget*1000)
    df
  })
  output$viz_nil_dist <- renderPlot({
    df <- nil_filtered(); req(nrow(df)>0)
    budget_line <- if(!is.null(input$nil_budget)) input$nil_budget*1000 else NULL
    p <- ggplot(df,aes(x=nil_value/1000))+geom_histogram(fill=CU_GOLD,bins=30,color="#000",alpha=.9)+
      scale_x_continuous(labels=dollar_format(suffix="K",prefix="$",scale=1))+
      labs(title="NIL market value distribution",subtitle="All players matching current filters",
           x="Estimated annual NIL ($K)",y="Players")+theme_cu()
    if(!is.null(budget_line))
      p <- p+geom_vline(xintercept=budget_line/1000,color="#f44336",linewidth=1,linetype="dashed")+
      annotate("text",x=budget_line/1000,y=Inf,label=" Budget cap",color="#f44336",size=3,hjust=0,vjust=1.5)
    p
  })
  output$viz_nil_by_pos <- renderPlot({
    df <- nil_data_all(); req(nrow(df)>0)
    pos_order <- df %>% group_by(Position) %>%
      summarise(med=median(nil_value,na.rm=TRUE),.groups="drop") %>%
      arrange(desc(med)) %>% pull(Position)
    df <- df %>% filter(Position%in%c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"))
    df$Position <- factor(df$Position,
                          levels=rev(intersect(pos_order,c("QB","RB","WR","TE","EDGE","DL","LB","CB","S","K"))))
    ggplot(df,aes(x=Position,y=nil_value/1000,fill=Position))+
      geom_boxplot(alpha=.8,outlier.color=CU_GOLD,outlier.size=1.5)+
      scale_fill_manual(values=setNames(
        colorRampPalette(c("#1D9E75","#CFB87C","#D85A30"))(10),levels(df$Position)),guide="none")+
      scale_y_continuous(labels=dollar_format(suffix="K",prefix="$",scale=1))+coord_flip()+
      labs(title="NIL market value by position",subtitle="Median = market rate | dots = outliers",
           x=NULL,y="Estimated annual NIL ($K)")+theme_cu()
  })
  output$tbl_nil <- renderDT({
    df <- nil_filtered(); req(nrow(df)>0)
    df2 <- df %>% select(-nil_value) %>% mutate(Rank=row_number()) %>% select(Rank,everything())
    nil_idx <- which(names(df2)=="NIL Est.")-1
    datatable(df2,options=list(pageLength=30,dom="ftp",ordering=TRUE,
                               columnDefs=list(list(targets=nil_idx,width="90px"))),
              rownames=FALSE,escape=FALSE,selection="none",class="cell-border") %>%
      formatStyle("NIL Est.",color="#4caf50",fontWeight="bold") %>%
      formatStyle("Rank",color=CU_GOLD,fontWeight="bold")
  })
  
} # end server

shinyApp(ui=ui, server=server)