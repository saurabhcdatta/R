## =====================================================================
## 2_nim_reportdb.R
## Net interest margin (NIM), asset growth and insured-share growth for
## federally insured credit unions (CU_TYPE 1 = FCU, 2 = FISCU), pulled
## straight from the 5300 tables on the Report Database.
##
## Stands alone: it does not use the OCE combined panel or script 1, so it
## reaches whatever quarter the server holds.
##
##   NIM     (total interest income - total interest expense), annualized,
##           as a percent of average assets            [accounts 115, 350, 010]
##   Growth  percent change over four quarters in total assets [010] and in
##           total insured shares and deposits         [069A; 069 before it]
##
## 5300 income is reported YEAR TO DATE, so two NIM series come out:
##   nim_ytd  YTD net interest income x 4/quarter
##            / average of assets now and assets at the prior year-end.
##            NCUA's convention (FPR, Quarterly Data Summary), so it is
##            comparable with the published numbers; block [2.8] checks it.
##   nim_qtr  net interest income earned IN the quarter x 4
##            / average of assets now and assets one quarter earlier.
##            A true quarterly series: the one to use for time-series work.
##
## From the quarterly series the script builds
##   an annual series  each full year: NIM for the year, growth December to
##                     December. The current year is its latest quarter:
##                     NIM year to date, growth over the four quarters to it.
##   a forecast        of NIM (the series named in FC_VARS): the next FC_YEARS
##                     years, each one the average of the FC_WINDOW years before it
## and writes the annual table, from EXPORT_FROM on and with the forecast, to
## an Excel workbook that it then opens. The quarterly series goes to a csv.
##
## Two things differ in the older years:
##   Filing    Before 2002Q3 only credit unions over $50 million filed in
##             March and September. Those quarters stay in the table with
##             their count, but nothing is computed for them, and the
##             in-quarter NIM starts in 2002Q3. The annual series is not
##             affected: every credit union files in December.
##   Accounts  Where a total is blank on a row, the script uses what the
##             form defines it as: 110 - 119 + 120 + 124 for 115, and
##             340 + 380 + 381 for 350. Insured shares are 069A, and the
##             older code 069 where 069A is blank. Block [2.4] shows, year
##             by year, which was used.
##
## The newest quarter on the live server is not final until NCUA publishes
## it (late and amended filings can still move it). Block [2.3] saves the
## extract under the pull date so any number can be traced back.
##
## Run block by block; every object stays in the environment.
## Packages: DBI, odbc, dplyr, tidyr. The workbook is written with base R.
## =====================================================================

## ---------------------------------------------------------------------
## [2.1] Settings and connection
## ---------------------------------------------------------------------
library(DBI)
library(odbc)
library(dplyr)
library(tidyr)

DSN        <- "ReportDatabase"   # the live server, as in 0_Get_ISG_Historical_Live_Server.R
OUT_DIR    <- getwd()            # the extract, the csv and the workbook are written here
START_YEAR <- 1991               # first year reported; the pull starts one year earlier
CU_TYPES   <- c(1, 2)            # federally insured: 1 = FCU, 2 = FISCU

## The 5300 tables read, and the letter each gets in the query. Block [2.2](a) shows where the
## accounts are; if one sits in another FS220 table, add that table here under a new letter.
TBL <- c(a = "FS220", b = "FS220A", s = "FS220B")

ACCTS <- c("ACCT_010",                                                   # total assets
           "ACCT_115", "ACCT_110", "ACCT_119", "ACCT_120", "ACCT_124",   # total interest income, and its lines
           "ACCT_350", "ACCT_340", "ACCT_380", "ACCT_381",               # total interest expense, and its lines
           "ACCT_069A", "ACCT_069",                                      # total insured shares and deposits
           "ACCT_018")                                                   # total shares and deposits (a check only)

## A quarter's NIM, or its insured-share growth, is computed only if the credit unions that reported
## the item hold at least this share of the quarter's assets. Below it, the result is left empty
## instead of being biased by the missing rows; block [2.6] shows the shares.
COVER_INC <- 0.995               # interest income
COVER_INS <- 0.999               # insured shares

## Annual table, forecast and export, blocks [2.9], [2.10] and [2.12]
NIM_DECIMALS    <- 2             # decimals of NIM in the annual table (NCUA publishes two)
GROWTH_DECIMALS <- 2             # decimals of the two growth rates
EXPORT_FROM     <- 2000          # first year in the Excel workbook (in R the series still start in START_YEAR)
FC_VARS         <- "nim"         # the series that get a forecast; for all three use
                                 # c("nim", "asset_growth", "ins_growth")
FC_YEARS        <- 10            # years to forecast
FC_WINDOW       <- 5             # years averaged for each forecast
FC_ROLLING      <- TRUE          # TRUE:  each forecast year is the average of the FC_WINDOW years
                                 #        before it, so earlier forecasts feed the later ones
                                 # FALSE: every forecast year is the same number, the average of
                                 #        the last FC_WINDOW years of history

con <- dbConnect(odbc(), dsn = DSN, timeout = 10)

## ---------------------------------------------------------------------
## [2.2] Look at the server before pulling
## ---------------------------------------------------------------------
## (a) Where the accounts are
dbGetQuery(con, sprintf("
  SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE UPPER(COLUMN_NAME) IN (%s)
  ORDER BY COLUMN_NAME, TABLE_NAME", paste0("'", ACCTS, "'", collapse = ", ")))
## LOOK: every table on the server that has one of the accounts. The pull reads only the tables in TBL.

acct_map <- NULL                                     # the accounts found in TBL, as they are spelled on the server
for (al in names(TBL)) {
  cols <- names(dbGetQuery(con, sprintf("SELECT TOP 0 * FROM %s", TBL[al])))   # column names only, no rows
  hit  <- cols[toupper(cols) %in% ACCTS]
  acct_map <- rbind(acct_map, data.frame(alias = rep(al, length(hit)), table = rep(TBL[[al]], length(hit)),
                                         column = hit))
}
acct_map <- acct_map[!duplicated(toupper(acct_map$column)), ]   # an account found in two tables: the first is read
rownames(acct_map) <- NULL
acct_map
setdiff(ACCTS, toupper(acct_map$column))
## LOOK: acct_map is what the pull will read, and from which table. The second result lists the
##       accounts found in none of the TBL tables: they are treated as blank. If the first table
##       above shows one of them in another FS220 table, add that table to TBL in [2.1] and run this
##       block again. ACCT_010, ACCT_115, ACCT_350 and ACCT_069A must be found.

## (b) What the server holds for the latest quarters
coverage <- dbGetQuery(con, sprintf("
  SELECT YEAR(a.Cycle_date)   AS yr,
         MONTH(a.Cycle_date)  AS mo,
         COUNT(*)             AS filers,
         COUNT(b.join_number) AS with_b,
         COUNT(s.join_number) AS with_s,
         COUNT(t.JOIN_NUMBER) AS with_foicu,
         SUM(CASE WHEN t.CU_TYPE IN ('1', '2') THEN 1 ELSE 0 END) AS ficu_asof,
         SUM(CASE WHEN c.CU_TYPE IN ('1', '2') THEN 1 ELSE 0 END) AS ficu_now
  FROM %s a
  LEFT JOIN %s b ON b.join_number = a.join_number AND b.Cycle_date = a.Cycle_date
  LEFT JOIN %s s ON s.join_number = a.join_number AND s.Cycle_date = a.Cycle_date
  LEFT JOIN FOICU t ON t.JOIN_NUMBER = a.join_number AND t.CYCLE_DATE = a.Cycle_date
  LEFT JOIN (SELECT JOIN_NUMBER, MIN(CU_TYPE) AS CU_TYPE
             FROM CU030 GROUP BY JOIN_NUMBER) c
         ON c.JOIN_NUMBER = a.join_number
  WHERE YEAR(a.Cycle_date) >= %d
  GROUP BY YEAR(a.Cycle_date), MONTH(a.Cycle_date)
  ORDER BY YEAR(a.Cycle_date), MONTH(a.Cycle_date)",
  TBL[["a"]], TBL[["b"]], TBL[["s"]], as.integer(format(Sys.Date(), "%Y")) - 1))
coverage
## LOOK, one row per quarter:
##   filers       5300 rows on file. The newest quarter should sit just below the one before.
##   with_b       of those, rows that also have a record in the income table (should equal filers)
##   with_s       of those, rows that also have a record in the insured-shares table (same)
##   with_foicu   rows with a FOICU record for that same quarter
##   ficu_asof    type 1-2 by FOICU, i.e. as of that quarter. Published FICU counts:
##                4,287 (2025Q4), 4,250 (2026Q1), 4,214 (2026Q2).
##   ficu_now     type 1-2 by CU030, i.e. by today's type (the insured-share script's rule)
## If with_foicu is 0 for the newest quarter, FOICU has not caught up yet;
## block [2.4] then carries each credit union's type forward from the quarter before.

## (c) How far back the server goes, and who filed in the first years
dbGetQuery(con, sprintf("SELECT MIN(Cycle_date) AS first_cycle, MAX(Cycle_date) AS last_cycle FROM %s", TBL[["a"]]))
dbGetQuery(con, sprintf("
  SELECT YEAR(Cycle_date) AS yr, MONTH(Cycle_date) AS mo, COUNT(*) AS filers
  FROM %s
  WHERE YEAR(Cycle_date) BETWEEN %d AND %d
  GROUP BY YEAR(Cycle_date), MONTH(Cycle_date)
  ORDER BY YEAR(Cycle_date), MONTH(Cycle_date)", TBL[["a"]], START_YEAR - 1, START_YEAR + 1))
## LOOK: first_cycle should be December of START_YEAR - 1 or earlier. If it is later, the annual
##       series starts with the first year that has a December before it. In the second table
##       expect far fewer filers in March and September than in June and December.

## ---------------------------------------------------------------------
## [2.3] Pull one thin row per credit union per quarter, then disconnect
## ---------------------------------------------------------------------
if (!all(c("ACCT_010", "ACCT_115", "ACCT_350") %in% toupper(acct_map$column)))
  stop("ACCT_010, ACCT_115 or ACCT_350 was not found in the TBL tables: see [2.2](a).")

used  <- setdiff(unique(acct_map$alias), "a")        # the tables joined to the first one
joins <- if (length(used) == 0) "" else
  paste0("LEFT JOIN ", TBL[used], " ", used, " ON ", used, ".join_number = a.join_number AND ",
         used, ".Cycle_date = a.Cycle_date", collapse = "\n  ")
qry   <- paste0("
  SELECT CAST(a.join_number AS INT) AS join_number,
         YEAR(a.Cycle_date)         AS yr,
         MONTH(a.Cycle_date)        AS mo,
         t.CU_TYPE                  AS type_asof,
         c.CU_TYPE                  AS type_now,
         ", paste0("CAST(", acct_map$alias, ".", acct_map$column, " AS FLOAT) AS ", tolower(acct_map$column),
                   collapse = ",\n         "), "
  FROM ", TBL[["a"]], " a
  ", joins, "
  LEFT JOIN FOICU t ON t.JOIN_NUMBER = a.join_number AND t.CYCLE_DATE = a.Cycle_date
  LEFT JOIN (SELECT JOIN_NUMBER, MIN(CU_TYPE) AS CU_TYPE
             FROM CU030 GROUP BY JOIN_NUMBER) c
         ON c.JOIN_NUMBER = a.join_number
  WHERE YEAR(a.Cycle_date) >= ", START_YEAR - 1)     # one extra year: prior-year-end and year-ago values
cat(qry, "\n")                                       ## LOOK: the query as it is sent

raw <- dbGetQuery(con, qry)
dbDisconnect(con)

names(raw) <- tolower(names(raw))
for (v in setdiff(tolower(ACCTS), names(raw))) raw[[v]] <- NA_real_   # an account not on the server: blank
dim(raw)                                             ## LOOK: about a million rows from 1990; 18 columns
table(raw$yr, raw$mo)                                ## LOOK: months 3, 6, 9 and 12 only, from START_YEAR - 1

extract_file <- file.path(OUT_DIR, sprintf("nim_extract_%s.rds", format(Sys.Date(), "%Y%m%d")))
saveRDS(raw, extract_file)                           # raw <- readRDS(extract_file) restarts from here

## ---------------------------------------------------------------------
## [2.4] Shape the panel
## ---------------------------------------------------------------------
## Charter type: as of the quarter (FOICU). Where FOICU has no record for a quarter, the type comes
## from the nearest quarter of the same credit union that has one (the one before if there is one,
## otherwise the one after); a credit union with no FOICU record at all gets today's type (CU030).
cr <- raw %>%
  mutate(year      = yr,
         quarter   = (mo + 2) %/% 3,
         q_index   = year * 4 + quarter,             # consecutive quarters differ by 1
         type_asof = as.numeric(as.character(type_asof)),
         type_now  = as.numeric(as.character(type_now)),
         type_near = type_asof) %>%
  arrange(join_number, q_index) %>%
  group_by(join_number) %>%
  fill(type_near, .direction = "downup") %>%
  ungroup() %>%
  mutate(cu_type    = coalesce(type_near, type_now),
         assets_tot = acct_010,
         ## the lines that add up to total interest income and to total interest expense
         inc_lines  = if_else(is.na(acct_110) & is.na(acct_120), NA_real_,
                              coalesce(acct_110, 0) - coalesce(acct_119, 0) +
                                coalesce(acct_120, 0) + coalesce(acct_124, 0)),
         exp_lines  = if_else(is.na(acct_340) & is.na(acct_380) & is.na(acct_381), NA_real_,
                              coalesce(acct_340, 0) + coalesce(acct_380, 0) + coalesce(acct_381, 0)),
         ## each amount: the total where it is filled, otherwise the lines; "none" = nothing reported
         inc_src    = case_when(!is.na(acct_115) & acct_115 != 0 ~ "115",
                                !is.na(inc_lines)                ~ "lines",
                                !is.na(acct_115)                 ~ "115",
                                TRUE                             ~ "none"),
         exp_src    = case_when(!is.na(acct_350) & acct_350 != 0 ~ "350",
                                !is.na(exp_lines)                ~ "lines",
                                !is.na(acct_350)                 ~ "350",
                                TRUE                             ~ "none"),
         ins_src    = case_when(!is.na(acct_069a) & acct_069a != 0 ~ "069A",
                                !is.na(acct_069)  & acct_069  != 0 ~ "069",
                                TRUE                               ~ "none"),
         int_inc    = case_when(inc_src == "115"  ~ acct_115, inc_src == "lines" ~ inc_lines, TRUE ~ NA_real_),
         int_exp    = case_when(exp_src == "350"  ~ acct_350, exp_src == "lines" ~ exp_lines, TRUE ~ NA_real_),
         ins_sh     = case_when(ins_src == "069A" ~ acct_069a, ins_src == "069"  ~ acct_069,  TRUE ~ NA_real_),
         ## a row reports income if interest income is filled; a blank expense is zero
         nii_ytd    = if_else(is.na(int_inc), NA_real_, int_inc - coalesce(int_exp, 0)))

sum(duplicated(cr[, c("join_number", "q_index")]))   ## LOOK: 0 (one row per credit union per quarter)

## Where each row's charter type came from: every December, and the newest six quarters
cr %>%
  filter(quarter == 4 | q_index >= max(q_index) - 5) %>%
  group_by(year, quarter) %>%
  summarise(rows       = n(),
            from_foicu = sum(!is.na(type_asof)),                      # FOICU, same quarter
            from_near  = sum(is.na(type_asof) & !is.na(type_near)),   # FOICU, nearest quarter
            from_cu030 = sum(is.na(type_near) & !is.na(type_now)),    # no FOICU record at all
            no_type    = sum(is.na(cu_type)),
            ficu       = sum(cu_type %in% CU_TYPES),
            .groups = "drop") %>%
  as.data.frame()
## LOOK: no_type near 0 in every row. Years that are mostly from_near or from_cu030 are typed by a
##       later date, so check their ficu count against the published one in [2.8].

## Which account each amount came from, by year (credit unions of type 1-2, all quarters)
cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  group_by(year) %>%
  summarise(rows     = n(),
            inc_115  = sum(inc_src == "115"),  inc_ln = sum(inc_src == "lines"), inc_no = sum(inc_src == "none"),
            exp_350  = sum(exp_src == "350"),  exp_ln = sum(exp_src == "lines"), exp_no = sum(exp_src == "none"),
            ins_069a = sum(ins_src == "069A"), ins_069 = sum(ins_src == "069"),  ins_no = sum(ins_src == "none"),
            .groups = "drop") %>%
  as.data.frame()
## LOOK: inc_no and ins_no near 0 in every year (exp_no can be larger: some credit unions pay nothing).
##       A year with many rows under inc_no or ins_no lacks that item, and block [2.6] then leaves
##       its NIM or its insured-share growth empty.

## Do the lines add up to the totals, and are insured shares a sensible part of all shares?
cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  group_by(year) %>%
  summarise(lines_to_115  = round(sum(inc_lines[inc_src == "115" & !is.na(inc_lines)]) /
                                  sum(acct_115[inc_src == "115" & !is.na(inc_lines)]), 4),
            lines_to_350  = round(sum(exp_lines[exp_src == "350" & !is.na(exp_lines)]) /
                                  sum(acct_350[exp_src == "350" & !is.na(exp_lines)]), 4),
            ins_to_shares = round(sum(ins_sh[!is.na(ins_sh) & !is.na(acct_018)]) /
                                  sum(acct_018[!is.na(ins_sh) & !is.na(acct_018)]), 4),
            .groups = "drop") %>%
  as.data.frame()
## LOOK: lines_to_115 and lines_to_350 equal to 1 in the years next to those where the lines are used
##       (NaN = the total and the lines are never both there; the lines can drift from 1 after the
##       2022 form change, where they are not used). ins_to_shares about 0.97 in the 1990s and about
##       0.90 now, moving smoothly except for the step up when the limit rose to $250,000.

## Which quarters did every credit union file?
qcov <- cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  count(year, quarter, q_index, name = "n_cu") %>%
  arrange(q_index) %>%
  mutate(full   = coalesce(n_cu >= 0.9 * pmax(lag(n_cu), lead(n_cu), na.rm = TRUE), TRUE),
         ## in-quarter income also needs the quarter before to be on file and full
         qtr_ok = full & coalesce(lag(full) & lag(q_index) == q_index - 1, FALSE))
as.data.frame(qcov %>% filter(!full))
## LOOK: the quarters with fewer than 90 percent of the credit unions of the quarters around them.
##       Expect March and September up to 2002Q1 and nothing after. A newest quarter listed here is
##       not fully loaded on the server yet: its numbers are left out until it is.

## ---------------------------------------------------------------------
## [2.5] Year-to-date income -> income earned in the quarter
## ---------------------------------------------------------------------
## Done credit union by credit union, on ALL rows, before the charter-type
## filter. Differencing the aggregate instead would subtract the earlier
## income of every credit union that merged away during the year.
##   Q1                             quarter income = YTD
##   Q2-Q4, prior quarter on file   YTD minus the prior quarter's YTD
##   Q2-Q4, prior quarter missing   YTD / quarter (even pace; should be rare)
cr <- cr %>%
  arrange(join_number, q_index) %>%
  group_by(join_number) %>%
  mutate(prev_q   = lag(q_index),
         prev_ytd = lag(nii_ytd),
         has_prev = !is.na(prev_q) & prev_q == q_index - 1 & !is.na(prev_ytd),
         nii_qtr  = case_when(is.na(nii_ytd) ~ NA_real_,
                              quarter == 1   ~ nii_ytd,
                              has_prev       ~ nii_ytd - prev_ytd,
                              TRUE           ~ nii_ytd / quarter)) %>%
  ungroup()

cr %>%
  filter(cu_type %in% CU_TYPES, q_index %in% qcov$q_index[qcov$qtr_ok], year >= START_YEAR, !is.na(nii_ytd)) %>%
  summarise(rows        = n(),
            q1          = sum(quarter == 1),
            differenced = sum(quarter > 1 & has_prev),
            even_pace   = sum(quarter > 1 & !has_prev)) %>%
  as.data.frame()
## LOOK: the quarters that get an in-quarter NIM; even_pace is a tiny share of their rows

## ---------------------------------------------------------------------
## [2.6] Add up CU_TYPE 1 and 2 by quarter
## ---------------------------------------------------------------------
agg <- cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  group_by(year, quarter, q_index) %>%
  summarise(n_cu    = n(),
            assets  = sum(assets_tot, na.rm = TRUE),
            cov_inc = sum(assets_tot[!is.na(int_inc)], na.rm = TRUE) / assets,   # share of assets at credit unions
            cov_ins = sum(assets_tot[!is.na(ins_sh)],  na.rm = TRUE) / assets,   # that reported the item
            ins     = sum(ins_sh,     na.rm = TRUE),
            inc_ytd = sum(int_inc,    na.rm = TRUE),
            exp_ytd = sum(if_else(is.na(int_inc), 0, coalesce(int_exp, 0))),
            nii_ytd = sum(nii_ytd,    na.rm = TRUE),
            nii_qtr = sum(nii_qtr,    na.rm = TRUE),
            .groups = "drop") %>%
  arrange(q_index) %>%
  left_join(select(qcov, q_index, full, qtr_ok), by = "q_index") %>%
  mutate(inc_ok = cov_inc >= COVER_INC,
         ins_ok = cov_ins >= COVER_INS)

missing_q <- setdiff(min(agg$q_index):max(agg$q_index), agg$q_index)
if (length(missing_q)) paste0((missing_q - 1) %/% 4, "Q", (missing_q - 1) %% 4 + 1) else "none"
## LOOK: quarters with no rows at all: "none", or only March and September quarters of the early years
as.data.frame(agg %>% filter(quarter == 4 | q_index >= max(q_index) - 3) %>%
                transmute(year, quarter, n_cu, cov_inc = round(cov_inc, 4), cov_ins = round(cov_ins, 4),
                          full, qtr_ok, inc_ok, ins_ok))
## LOOK: every December, and the newest quarters. cov_inc and cov_ins should be 1 or a hair below, and
##       full, inc_ok and ins_ok TRUE. A FALSE means that item is not computed for that quarter; the
##       [2.4] tables show what is missing. qtr_ok is FALSE until 2002Q3.
as.data.frame(tail(agg, 4))

## ---------------------------------------------------------------------
## [2.7] NIM and the two growth rates, by quarter
## ---------------------------------------------------------------------
pye  <- agg %>%                                      # each year-end, keyed to the following year
  filter(quarter == 4) %>%
  transmute(year = year + 1, assets_pye = assets, full_pye = full)
prv  <- agg %>%                                      # one quarter earlier
  transmute(q_index = q_index + 1, assets_prev = assets)
yago <- agg %>%                                      # four quarters earlier
  transmute(q_index = q_index + 4, assets_yago = assets, ins_yago = ins, full_yago = full, ins_ok_yago = ins_ok)

nim <- agg %>%
  left_join(pye,  by = "year") %>%
  left_join(prv,  by = "q_index") %>%
  left_join(yago, by = "q_index") %>%
  mutate(period       = paste0(year, "Q", quarter),
         ok_ytd       = full & inc_ok & coalesce(full_pye, FALSE),
         ok_qtr       = qtr_ok & inc_ok,
         ok_ag        = full & coalesce(full_yago, FALSE),
         ok_ig        = ok_ag & ins_ok & coalesce(ins_ok_yago, FALSE),
         nim_ytd      = if_else(ok_ytd, 100 * (nii_ytd * 4 / quarter) / ((assets + assets_pye)  / 2), NA_real_),
         nim_qtr      = if_else(ok_qtr, 100 * (nii_qtr * 4)           / ((assets + assets_prev) / 2), NA_real_),
         asset_growth = if_else(ok_ag,  100 * (assets / assets_yago - 1), NA_real_),
         ins_growth   = if_else(ok_ig,  100 * (ins / ins_yago - 1),       NA_real_),
         ## why a cell is empty
         note         = if_else(!full, "Not all credit unions filed this quarter.", paste(
           if_else(!inc_ok,          "No NIM: interest income is incomplete.", ""),
           if_else(inc_ok & !ok_ytd, "No NIM year to date: the prior year-end is missing.", ""),
           if_else(inc_ok & !ok_qtr, "No in-quarter NIM: the quarter before is incomplete or missing.", ""),
           if_else(!ok_ag,           "No growth: the quarter a year earlier is incomplete or missing.", ""),
           if_else(ok_ag & !ok_ig,   "No insured share growth: insured shares are incomplete here or a year earlier.",
                   ""))),
         note         = trimws(gsub(" +", " ", note))) %>%
  filter(year >= START_YEAR)

nim_ts <- nim %>%
  transmute(period, year, quarter, n_cu,
            nim_ytd      = round(nim_ytd, 3),
            nim_qtr      = round(nim_qtr, 3),
            assets_mn    = if_else(full, round(assets / 1e6), NA_real_),          # $ millions
            asset_growth = round(asset_growth, 3),
            ins_mn       = if_else(full & ins_ok, round(ins / 1e6), NA_real_),    # $ millions
            ins_growth   = round(ins_growth, 3),
            note)
print(as.data.frame(head(nim_ts, 8)),  row.names = FALSE)
print(as.data.frame(tail(nim_ts[, names(nim_ts) != "note"], 12)), row.names = FALSE)

## ---------------------------------------------------------------------
## [2.8] Check against NCUA's published figures
## ---------------------------------------------------------------------
## Recent years. Source: NCUA Quarterly Credit Union Data Summary, 2026 Q2
## (ncua.gov/files/publications/analysis/quarterly-data-summary-2026-Q2.pdf);
## 2025Q1 from NCUA's press release of 5 June 2025. Assets, insured shares
## and net interest margin in $ billions, margin at an annual rate. Nothing
## is published for 2026Q3 yet, so the newest quarter has no row here.
pub <- data.frame(
  period     = c("2021Q4", "2022Q2", "2022Q4", "2023Q2", "2023Q4", "2024Q2", "2024Q4",
                 "2025Q1", "2025Q2", "2025Q3", "2025Q4", "2026Q1", "2026Q2"),
  pub_n_cu   = c(4942, 4853, 4760, 4686, 4604, 4533, 4455,
                 NA, 4370, 4331, 4287, 4250, 4214),
  pub_assets = c(2060.3, 2136.5, 2167.7, 2218.4, 2255.7, 2297.0, 2307.4,
                 NA, 2379.0, 2397.8, 2433.0, 2484.0, 2498.9),
  pub_ins    = c(1632.0, 1687.4, 1683.2, 1720.9, 1720.0, 1758.3, 1778.2,
                 NA, 1829.8, 1835.2, 1859.7, 1907.3, 1909.3),
  pub_nii    = c(50.6, 55.9, 60.6, 66.1, 66.6, 69.5, 71.2,
                 75.6, 77.8, 79.5, 80.4, 84.6, 86.1),
  pub_nim    = c(2.59, 2.67, 2.86, 3.02, 3.01, 3.05, 3.12,
                 3.24, 3.32, 3.38, 3.39, 3.44, 3.49))

chk <- nim %>%
  mutate(assets_bn = round(assets / 1e9, 1),
         ins_bn    = round(ins / 1e9, 1),
         nii_bn    = round(nii_ytd * 4 / quarter / 1e9, 1),
         nim_ytd   = round(nim_ytd, 2)) %>%
  inner_join(pub, by = "period") %>%
  mutate(diff = round(nim_ytd - pub_nim, 2)) %>%
  select(period, n_cu, pub_n_cu, assets_bn, pub_assets, ins_bn, pub_ins, nii_bn, pub_nii, nim_ytd, pub_nim, diff)
print(as.data.frame(chk), row.names = FALSE)
## LOOK: diff within about 0.02 (rounding, later revisions); counts, assets and insured shares equal
##       or very close. Published growth over the year to 2026Q2: assets 5.0, insured shares 4.3 percent.
##   counts or assets off             -> population: see the type columns in [2.2] and [2.4]
##   assets right, nii_bn far off     -> the income accounts are not the ones pulled in [2.3]
##   assets right, ins_bn far off     -> the insured-share account is not the one pulled in [2.3]

## The first years. Source: NCUA 1995 Annual Report, the ten-year summaries of federal and of
## federally insured state credit unions added together (espanol.ncua.gov/files/annual-reports/1995AR.pdf).
## Year-end counts; assets and insured shares in $ billions (pub_ins is "shares in insured credit
## unions" from the report's insurance fund table, about 97 percent of all shares); cost of funds
## (340 + 380 + 381) and gross income, each as a percent of average assets.
pub90 <- data.frame(
  year       = 1990:1995,
  pub_n_cu   = c(12860, 12960, 12653, 12317, 11991, 11687),
  pub_assets = c(198.2, 227.1, 261.3, 277.2, 289.5, 306.6),
  pub_ins    = c(NA, 199.8, 229.5, 240.3, 247.7, 261.4),
  pub_cost   = c(NA, 5.4, 4.0, 3.1, 3.0, 3.5),
  pub_gross  = c(NA, 10.1, 8.8, 7.7, 7.6, 8.1))

chk90 <- agg %>%
  filter(quarter == 4) %>%
  left_join(pye, by = "year") %>%
  mutate(assets_bn = round(assets / 1e9, 1),
         ins_bn    = round(ins / 1e9, 1),
         cost      = round(100 * exp_ytd / ((assets + assets_pye) / 2), 1),
         yield     = round(100 * inc_ytd / ((assets + assets_pye) / 2), 1),
         nim       = round(100 * nii_ytd / ((assets + assets_pye) / 2), 2)) %>%
  inner_join(pub90, by = "year") %>%
  select(year, n_cu, pub_n_cu, assets_bn, pub_assets, ins_bn, pub_ins, cost, pub_cost, yield, pub_gross, nim)
print(as.data.frame(chk90), row.names = FALSE)
## LOOK: counts and assets close to the published ones, ins_bn within a few percent of pub_ins, and
##       cost within about 0.1 of pub_cost. yield is interest income only, so it sits a little below
##       pub_gross, which also has fee and other operating income. nim = yield - cost: somewhat below
##       pub_gross - pub_cost, which is 4.6 to 4.8 in these years. The 1990 row has no ratios.
##   counts and assets above published  -> credit unions that became federally insured later are
##                                         counted early: FOICU does not reach back that far ([2.4])
##   cost and yield near half published -> December income in those years is not year to date
##   cost near 0, or nim near yield     -> interest expense is not on file for those years ([2.4])
##   no rows                            -> START_YEAR is after 1995, or the server starts later

## ---------------------------------------------------------------------
## [2.9] Annual series
## ---------------------------------------------------------------------
## A full year is its fourth quarter: nim_ytd is then the year's net
## interest income over the average of assets at that year-end and the one
## before, and the four-quarter growth is December to December. The current
## year is its latest full quarter: NIM year to date, annualized, and growth
## over the four quarters to that quarter.
annual <- nim %>%
  filter(full) %>%
  group_by(year) %>%
  filter(quarter == max(quarter)) %>%
  ungroup() %>%
  transmute(year,
            nim          = round(nim_ytd,      NIM_DECIMALS),
            asset_growth = round(asset_growth, GROWTH_DECIMALS),
            ins_growth   = round(ins_growth,   GROWTH_DECIMALS),
            type         = if_else(quarter == 4, "Actual", "Year to date"),
            basis        = if_else(quarter == 4, "Full year",
                                   paste0("Through ", period, ": NIM annualized, growth over four quarters"))) %>%
  filter(!(is.na(nim) & is.na(asset_growth) & is.na(ins_growth)))

print(as.data.frame(annual), row.names = FALSE)
## LOOK: one row per year from START_YEAR; only the last row can be "Year to date". An NA is a year
##       for which that item cannot be computed: [2.6] shows which test failed.

## ---------------------------------------------------------------------
## [2.10] Forecast: the average of the previous five years
## ---------------------------------------------------------------------
## The averages are taken over the rounded values in the table, so each
## forecast can be checked by hand from the years named in its basis.
vars     <- c("nim", "asset_growth", "ins_growth")
decimals <- c(nim = NIM_DECIMALS, asset_growth = GROWTH_DECIMALS, ins_growth = GROWTH_DECIMALS)

last_year <- max(annual$year)
fc_years  <- last_year + seq_len(FC_YEARS)

path <- as.data.frame(annual[, c("year", vars)])     # the history; each forecast year is added as it is made
for (y in fc_years) {
  yrs <- if (FC_ROLLING) (y - FC_WINDOW):(y - 1) else (last_year - FC_WINDOW + 1):last_year
  new <- data.frame(year = y)
  for (v in vars) {                                  # a series not in FC_VARS gets no forecast: NA
    avg      <- if (v %in% FC_VARS) mean(path[[v]][match(yrs, path$year)]) else NA_real_
    new[[v]] <- sign(avg) * round(abs(avg) + 1e-9, decimals[[v]])   # an exact half rounds away from zero, as in Excel
  }
  path <- rbind(path, new)
}

fcst <- path[path$year %in% fc_years, ]
fcst$type  <- "Forecast"
fcst$basis <- if (FC_ROLLING) sprintf("Average of %d-%d", fcst$year - FC_WINDOW, fcst$year - 1) else
                              sprintf("Average of %d-%d", last_year - FC_WINDOW + 1, last_year)

annual_all <- bind_rows(annual, fcst)
print(as.data.frame(tail(annual_all, FC_WINDOW + FC_YEARS)), row.names = FALSE)
## LOOK: the last FC_YEARS rows are the forecast; "basis" names the years behind each one. Only the
##       series in FC_VARS have a forecast, the others are NA there. An NA for a series in FC_VARS
##       means one of the years behind it has no value.

## ---------------------------------------------------------------------
## [2.11] Quarterly csv, time-series objects, and a quick look
## ---------------------------------------------------------------------
last_period <- nim$period[max(which(nim$full))]
last_period                                          ## LOOK: the quarter you expect, e.g. "2026Q3"
write.csv(nim_ts, file.path(OUT_DIR, sprintf("nim_timeseries_%s.csv", last_period)), row.names = FALSE)

annual_ts  <- ts(as.matrix(annual[, vars]), start = annual$year[1])        # the three annual series
q0         <- which(!is.na(nim$nim_qtr))[1]                                # first quarter with an in-quarter NIM
nim_qtr_ts <- ts(nim$nim_qtr[q0:nrow(nim)], start = c(nim$year[q0], nim$quarter[q0]), frequency = 4)
all(diff(nim$q_index[q0:nrow(nim)]) == 1)            ## LOOK: TRUE (no quarter missing from there on)

titles <- c(nim = "Net interest margin", asset_growth = "Asset growth", ins_growth = "Insured share growth")
op <- par(mfrow = c(3, 1), mar = c(2.5, 4.5, 2.5, 1))
for (v in vars) {                                    # history solid, forecast dashed
  plot(annual_all$year, annual_all[[v]], type = "n", xlab = "", ylab = "Percent",
       main = paste0(titles[[v]], ", federally insured credit unions"))
  lines(annual$year, annual[[v]], lwd = 2)
  lines(c(last_year, fcst$year), c(annual[[v]][annual$year == last_year], fcst[[v]]), lty = 2)
}
par(op)

## ---------------------------------------------------------------------
## [2.12] Excel workbook: annual_all from EXPORT_FROM on
## ---------------------------------------------------------------------
## openxlsx is blocked on this machine and there is no zip program on it,
## so the workbook is written with base R alone. A .xlsx file is a zip of
## a few small XML files: (a) lays out the table, (b) builds the XML,
## (c) writes the zip, (d) checks the file and opens it in Excel.

## (a) The table
xl_annual <- as.data.frame(annual_all[annual_all$year >= EXPORT_FROM, ])
names(xl_annual) <- c("Year", "NIM (% of average assets)", "Asset growth (%)", "Insured share growth (%)",
                      "Type", "Basis")
range(xl_annual$Year)                                ## LOOK: EXPORT_FROM to the last forecast year

xlsx_file <- file.path(OUT_DIR, sprintf("annual_all_%s.xlsx", last_period))
if (file.exists(xlsx_file) && !suppressWarnings(file.remove(xlsx_file)))
  stop("Close ", basename(xlsx_file), " in Excel, then run this block again.")

## (b) The XML
XML_HEAD <- '<?xml version="1.0" encoding="UTF-8" standalone="yes"?>'
NS_MAIN  <- 'xmlns="http://schemas.openxmlformats.org/spreadsheetml/2006/main"'
NS_PKG   <- 'http://schemas.openxmlformats.org/package/2006/'
REL_DOC  <- 'http://schemas.openxmlformats.org/officeDocument/2006/relationships'
TYPE_XL  <- 'application/vnd.openxmlformats-officedocument.spreadsheetml.'

xml_text <- function(x) gsub("<", "&lt;", gsub("&", "&amp;", x, fixed = TRUE), fixed = TRUE)

## One worksheet: a bold header row that stays in view, then the rows of d.
##   style  cell style of each column: 0 plain, 2 NIM, 3 growth
##   width  column widths
xl_sheet <- function(d, style, width) {
  cols  <- seq_along(d)
  cells <- lapply(cols, function(j) {
    ref <- paste0(LETTERS[j], seq_len(nrow(d)) + 1)
    v   <- d[[j]]
    if (is.numeric(v)) {                               # a missing or infinite number is left as an empty cell
      ifelse(is.finite(v), sprintf('<c r="%s" s="%d"><v>%s</v></c>', ref, style[j], sprintf("%.15g", v)), "")
    } else {                                           # so is a missing or empty text
      v <- as.character(v)
      ifelse(is.na(v) | v == "", "", sprintf('<c r="%s" t="inlineStr"><is><t>%s</t></is></c>', ref, xml_text(v)))
    }
  })
  header <- sprintf('<c r="%s1" s="1" t="inlineStr"><is><t>%s</t></is></c>', LETTERS[cols], xml_text(names(d)))
  rows   <- c(paste(header, collapse = ""), do.call(paste0, cells))
  paste0(XML_HEAD, '<worksheet ', NS_MAIN, '>',
         '<sheetViews><sheetView tabSelected="1" workbookViewId="0">',
         '<pane ySplit="1" topLeftCell="A2" activePane="bottomLeft" state="frozen"/>',
         '<selection pane="bottomLeft" activeCell="A2" sqref="A2"/></sheetView></sheetViews>',
         '<sheetFormatPr defaultRowHeight="15"/>',
         '<cols>', paste0(sprintf('<col min="%d" max="%d" width="%d" customWidth="1"/>', cols, cols, width),
                          collapse = ""), '</cols>',
         '<sheetData>', paste0(sprintf('<row r="%d">%s</row>', seq_along(rows), rows), collapse = ""),
         '</sheetData>',
         '<pageMargins left="0.7" right="0.7" top="0.75" bottom="0.75" header="0.3" footer="0.3"/>',
         '</worksheet>')
}

fmt_nim    <- paste0("0.", strrep("0", NIM_DECIMALS))            # 0.00 when NIM_DECIMALS is 2
fmt_growth <- paste0("0.", strrep("0", GROWTH_DECIMALS))

parts <- c(
  "[Content_Types].xml" = paste0(XML_HEAD, '<Types xmlns="', NS_PKG, 'content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="', TYPE_XL, 'sheet.main+xml"/>',
    '<Override PartName="/xl/styles.xml" ContentType="', TYPE_XL, 'styles+xml"/>',
    '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="', TYPE_XL, 'worksheet+xml"/>',
    '</Types>'),
  "_rels/.rels" = paste0(XML_HEAD, '<Relationships xmlns="', NS_PKG, 'relationships">',
    '<Relationship Id="rId1" Type="', REL_DOC, '/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'),
  "xl/workbook.xml" = paste0(XML_HEAD, '<workbook ', NS_MAIN, ' xmlns:r="', REL_DOC, '">',
    '<bookViews><workbookView/></bookViews>',
    '<sheets><sheet name="annual_all" sheetId="1" r:id="rId1"/></sheets></workbook>'),
  "xl/_rels/workbook.xml.rels" = paste0(XML_HEAD, '<Relationships xmlns="', NS_PKG, 'relationships">',
    '<Relationship Id="rId1" Type="', REL_DOC, '/worksheet" Target="worksheets/sheet1.xml"/>',
    '<Relationship Id="rId2" Type="', REL_DOC, '/styles" Target="styles.xml"/>',
    '</Relationships>'),
  "xl/styles.xml" = paste0(XML_HEAD, '<styleSheet ', NS_MAIN, '>',
    '<numFmts count="2"><numFmt numFmtId="164" formatCode="', fmt_nim, '"/>',
    '<numFmt numFmtId="165" formatCode="', fmt_growth, '"/></numFmts>',
    '<fonts count="2"><font><sz val="11"/><name val="Calibri"/><family val="2"/></font>',
    '<font><b/><sz val="11"/><name val="Calibri"/><family val="2"/></font></fonts>',
    '<fills count="2"><fill><patternFill patternType="none"/></fill>',
    '<fill><patternFill patternType="gray125"/></fill></fills>',
    '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>',
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
    '<cellXfs count="4">',
    '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',                           # 0 plain
    '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>',             # 1 bold header
    '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',   # 2 NIM
    '<xf numFmtId="165" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',   # 3 growth
    '</cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'),
  "xl/worksheets/sheet1.xml" = xl_sheet(xl_annual, style = c(0, 2, 3, 3, 0, 0), width = c(8, 28, 18, 26, 14, 56)))

## (c) The zip. gzfile() does the compression: the file it writes is a 10-byte header, the
## compressed bytes, then a checksum and the size, which is everything a zip entry stores.
le <- function(x, n) as.raw((x %/% 256^(0:(n - 1))) %% 256)      # x as n bytes, lowest byte first

zip_write <- function(parts, out) {
  con <- file(out, "wb"); on.exit(close(con))
  central <- raw(0)                                               # the directory, written after the files
  offset  <- 0
  for (p in names(parts)) {
    dat <- charToRaw(enc2utf8(parts[[p]]))
    tmp <- tempfile(fileext = ".gz")
    gzc <- gzfile(tmp, "wb"); writeBin(dat, gzc); close(gzc)
    gz  <- readBin(tmp, "raw", file.size(tmp)); unlink(tmp)
    n   <- length(gz)
    stopifnot(identical(gz[1:4], as.raw(c(0x1f, 0x8b, 0x08, 0x00))))
    body  <- gz[11:(n - 8)]
    name  <- charToRaw(p)
    entry <- c(le(20, 2), le(0, 2), le(8, 2), le(0, 2), le(33, 2), gz[(n - 7):(n - 4)],
               le(length(body), 4), le(length(dat), 4), le(length(name), 2), le(0, 2))
    writeBin(c(as.raw(c(0x50, 0x4b, 0x03, 0x04)), entry, name, body), con)
    central <- c(central, as.raw(c(0x50, 0x4b, 0x01, 0x02)), le(20, 2), entry,
                 le(0, 2), le(0, 2), le(0, 2), le(0, 4), le(offset, 4), name)
    offset <- offset + 30 + length(name) + length(body)
  }
  writeBin(c(central, as.raw(c(0x50, 0x4b, 0x05, 0x06)), le(0, 2), le(0, 2), le(length(parts), 2),
             le(length(parts), 2), le(length(central), 4), le(offset, 4), le(0, 2)), con)
}

zip_write(parts, xlsx_file)

## (d) Check the file, then open it
chk_dir <- tempfile()
unzip(xlsx_file, exdir = chk_dir)
all(vapply(names(parts), function(p)
  identical(readBin(file.path(chk_dir, p), "raw", 1e7), charToRaw(enc2utf8(parts[[p]]))), logical(1)))
## LOOK: TRUE (the workbook unzips back to exactly what was built)

shell.exec(normalizePath(xlsx_file))                 # opens the workbook in Excel
