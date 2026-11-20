## =====================================================================
## 2_nim_reportdb.R
## Net interest margin (NIM) time series for federally insured credit
## unions (CU_TYPE 1 = FCU, 2 = FISCU), pulled straight from the 5300
## tables on the Report Database.
##
## Stands alone: it does not use the OCE combined panel or script 1, so it
## reaches whatever quarter the server holds.
##
##   NIM = (total interest income - total interest expense), annualized,
##         as a percent of average assets        [5300 accounts 115, 350, 010]
##
## 5300 income is reported YEAR TO DATE, so two series come out:
##   nim_ytd  YTD net interest income x 4/quarter
##            / average of assets now and assets at the prior year-end.
##            NCUA's convention (FPR, Quarterly Data Summary), so it is
##            comparable with the published numbers; block [2.8] checks it.
##   nim_qtr  net interest income earned IN the quarter x 4
##            / average of assets now and assets one quarter earlier.
##            A true quarterly series: the one to use for time-series work.
##
## From these the script also builds
##   an annual series  each full year, and the current year to date
##   a forecast        the next five years, each one the average of the
##                     five years before it
## and writes an Excel workbook: annual history and forecast on the first
## tab, the quarterly series on the second.
##
## The newest quarter on the live server is not final until NCUA publishes
## it (late and amended filings can still move it). Block [2.3] saves the
## extract under the pull date so any number can be traced back.
##
## Run block by block; every object stays in the environment.
## Packages: DBI, odbc, dplyr. The workbook is written with base R.
## =====================================================================

## ---------------------------------------------------------------------
## [2.1] Settings and connection
## ---------------------------------------------------------------------
library(DBI)
library(odbc)
library(dplyr)

DSN        <- "ReportDatabase"   # the live server, as in 0_Get_ISG_Historical_Live_Server.R
OUT_DIR    <- getwd()            # the extract, the csv and the workbook are written here
START_YEAR <- 2005               # first year reported (before 2002Q3 credit unions
                                 # under $50M filed only in June and December)
CU_TYPES   <- c(1, 2)            # federally insured: 1 = FCU, 2 = FISCU

## Annual table and forecast, blocks [2.9] and [2.10]
NIM_DECIMALS <- 2                # decimals in the annual table (NCUA publishes two)
FC_YEARS     <- 5                # years to forecast
FC_WINDOW    <- 5                # years averaged for each forecast
FC_ROLLING   <- TRUE             # TRUE:  each forecast year is the average of the FC_WINDOW years
                                 #        before it, so earlier forecasts feed the later ones
                                 # FALSE: every forecast year is the same number, the average of
                                 #        the last FC_WINDOW years of history

con <- dbConnect(odbc(), dsn = DSN, timeout = 10)

## ---------------------------------------------------------------------
## [2.2] Look at the server before pulling
## ---------------------------------------------------------------------
## (a) Which tables hold the three accounts
dbGetQuery(con, "
  SELECT TABLE_NAME, COLUMN_NAME, DATA_TYPE
  FROM INFORMATION_SCHEMA.COLUMNS
  WHERE UPPER(COLUMN_NAME) IN ('ACCT_010', 'ACCT_115', 'ACCT_350')
  ORDER BY COLUMN_NAME, TABLE_NAME")
## LOOK: ACCT_010 in FS220; ACCT_115 and ACCT_350 in FS220A. If the two income
##       accounts sit in another FS220 table, replace FS220A below and in [2.3].

## (b) What the server holds for the latest quarters
coverage <- dbGetQuery(con, sprintf("
  SELECT YEAR(a.Cycle_date)   AS yr,
         MONTH(a.Cycle_date)  AS mo,
         COUNT(*)             AS filers,
         COUNT(b.join_number) AS with_income,
         COUNT(t.JOIN_NUMBER) AS with_foicu,
         SUM(CASE WHEN t.CU_TYPE IN ('1', '2') THEN 1 ELSE 0 END) AS ficu_asof,
         SUM(CASE WHEN c.CU_TYPE IN ('1', '2') THEN 1 ELSE 0 END) AS ficu_now
  FROM FS220 a
  LEFT JOIN FS220A b ON b.join_number = a.join_number AND b.Cycle_date = a.Cycle_date
  LEFT JOIN FOICU  t ON t.JOIN_NUMBER = a.join_number AND t.CYCLE_DATE = a.Cycle_date
  LEFT JOIN (SELECT JOIN_NUMBER, MIN(CU_TYPE) AS CU_TYPE
             FROM CU030 GROUP BY JOIN_NUMBER) c
         ON c.JOIN_NUMBER = a.join_number
  WHERE YEAR(a.Cycle_date) >= %d
  GROUP BY YEAR(a.Cycle_date), MONTH(a.Cycle_date)
  ORDER BY YEAR(a.Cycle_date), MONTH(a.Cycle_date)",
  as.integer(format(Sys.Date(), "%Y")) - 1))
coverage
## LOOK, one row per quarter:
##   filers       5300 rows on file. The newest quarter should sit just below the one before.
##   with_income  of those, rows that also have an FS220A record (should equal filers)
##   with_foicu   rows with a FOICU record for that same quarter
##   ficu_asof    type 1-2 by FOICU, i.e. as of that quarter. Published FICU counts:
##                4,287 (2025Q4), 4,250 (2026Q1), 4,214 (2026Q2).
##   ficu_now     type 1-2 by CU030, i.e. by today's type (the insured-share script's rule)
## If with_foicu is 0 for the newest quarter, FOICU has not caught up yet;
## block [2.4] then takes that quarter's type from CU030.

## ---------------------------------------------------------------------
## [2.3] Pull one thin row per credit union per quarter, then disconnect
## ---------------------------------------------------------------------
qry <- sprintf("
  SELECT CAST(a.join_number AS INT) AS join_number,
         YEAR(a.Cycle_date)         AS yr,
         MONTH(a.Cycle_date)        AS mo,
         t.CU_TYPE                  AS type_asof,
         c.CU_TYPE                  AS type_now,
         CAST(a.ACCT_010 AS FLOAT)  AS assets_tot,
         CAST(b.ACCT_115 AS FLOAT)  AS int_inc,
         CAST(b.ACCT_350 AS FLOAT)  AS int_exp
  FROM FS220 a
  LEFT JOIN FS220A b ON b.join_number = a.join_number AND b.Cycle_date = a.Cycle_date
  LEFT JOIN FOICU  t ON t.JOIN_NUMBER = a.join_number AND t.CYCLE_DATE = a.Cycle_date
  LEFT JOIN (SELECT JOIN_NUMBER, MIN(CU_TYPE) AS CU_TYPE
             FROM CU030 GROUP BY JOIN_NUMBER) c
         ON c.JOIN_NUMBER = a.join_number
  WHERE YEAR(a.Cycle_date) >= %d", START_YEAR - 1)   # one extra year: prior-year-end assets

raw <- dbGetQuery(con, qry)
dbDisconnect(con)

names(raw) <- tolower(names(raw))
dim(raw)                                             ## LOOK: several hundred thousand rows, 8 columns
table(raw$mo)                                        ## LOOK: only 3, 6, 9, 12

extract_file <- file.path(OUT_DIR, sprintf("nim_extract_%s.rds", format(Sys.Date(), "%Y%m%d")))
saveRDS(raw, extract_file)                           # raw <- readRDS(extract_file) restarts from here

## ---------------------------------------------------------------------
## [2.4] Shape the panel
## ---------------------------------------------------------------------
cr <- raw %>%
  mutate(year      = yr,
         quarter   = (mo + 2) %/% 3,
         type_asof = as.numeric(as.character(type_asof)),
         type_now  = as.numeric(as.character(type_now)),
         ## type as of the quarter (FOICU); where FOICU has no record, today's type (CU030)
         cu_type   = coalesce(type_asof, type_now),
         q_index   = year * 4 + quarter,   # used only to test that two rows are consecutive quarters
         ## a row reports income if interest income is filled; a blank expense is zero
         nii_ytd   = if_else(is.na(int_inc), NA_real_, int_inc - coalesce(int_exp, 0)))

sum(duplicated(cr[, c("join_number", "q_index")]))   ## LOOK: 0 (one row per credit union per quarter)

cr %>%
  filter(q_index >= max(q_index) - 5) %>%
  group_by(year, quarter) %>%
  summarise(rows       = n(),
            from_foicu = sum(!is.na(type_asof)),
            from_cu030 = sum(is.na(type_asof) & !is.na(type_now)),
            no_type    = sum(is.na(cu_type)),
            ficu       = sum(cu_type %in% CU_TYPES),
            .groups = "drop") %>%
  as.data.frame()                                    ## LOOK: where each quarter's type came from; no_type near 0

cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  group_by(year) %>%
  summarise(rows = n(), no_income = sum(is.na(int_inc)), no_expense = sum(is.na(int_exp))) %>%
  as.data.frame()                                    ## LOOK: no_income should be near zero every year

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
  filter(cu_type %in% CU_TYPES, year >= START_YEAR, !is.na(nii_ytd)) %>%
  summarise(rows        = n(),
            q1          = sum(quarter == 1),
            differenced = sum(quarter > 1 & has_prev),
            even_pace   = sum(quarter > 1 & !has_prev)) %>%
  as.data.frame()                                    ## LOOK: even_pace is a tiny share of rows

## ---------------------------------------------------------------------
## [2.6] Add up CU_TYPE 1 and 2 by quarter
## ---------------------------------------------------------------------
agg <- cr %>%
  filter(cu_type %in% CU_TYPES) %>%
  group_by(year, quarter) %>%
  summarise(n_cu    = n(),
            assets  = sum(assets_tot, na.rm = TRUE),
            nii_ytd = sum(nii_ytd,    na.rm = TRUE),
            nii_qtr = sum(nii_qtr,    na.rm = TRUE),
            .groups = "drop") %>%
  arrange(year, quarter)

all(diff(agg$year * 4 + agg$quarter) == 1)           ## LOOK: TRUE (no quarter missing; [2.7] relies on it)
as.data.frame(tail(agg, 6))

## ---------------------------------------------------------------------
## [2.7] NIM
## ---------------------------------------------------------------------
pye <- agg %>%                                       # assets at each year-end, keyed to the following year
  filter(quarter == 4) %>%
  mutate(year = year + 1) %>%
  select(year, assets_pye = assets)

nim <- agg %>%
  left_join(pye, by = "year") %>%
  mutate(assets_prev = lag(assets),                  # assets one quarter earlier
         nim_ytd = 100 * (nii_ytd * 4 / quarter) / ((assets + assets_pye)  / 2),
         nim_qtr = 100 * (nii_qtr * 4)           / ((assets + assets_prev) / 2),
         period  = paste0(year, "Q", quarter)) %>%
  filter(year >= START_YEAR)

nim_ts <- nim %>%
  select(period, year, quarter, n_cu, nim_ytd, nim_qtr) %>%
  mutate(nim_ytd = round(nim_ytd, 3),
         nim_qtr = round(nim_qtr, 3))
print(as.data.frame(tail(nim_ts, 12)), row.names = FALSE)

## ---------------------------------------------------------------------
## [2.8] Check nim_ytd against NCUA's published figures
## ---------------------------------------------------------------------
## Source: NCUA Quarterly Credit Union Data Summary, 2026 Q2, pages 1 and 3
## (ncua.gov/files/publications/analysis/quarterly-data-summary-2026-Q2.pdf);
## 2025Q1 from NCUA's press release of 5 June 2025. Assets and net interest
## margin in $ billions, margin at an annual rate. Nothing is published for
## 2026Q3 yet, so the newest quarter has no row here.
pub <- data.frame(
  period     = c("2021Q4", "2022Q2", "2022Q4", "2023Q2", "2023Q4", "2024Q2", "2024Q4",
                 "2025Q1", "2025Q2", "2025Q3", "2025Q4", "2026Q1", "2026Q2"),
  pub_n_cu   = c(4942, 4853, 4760, 4686, 4604, 4533, 4455,
                 NA, 4370, 4331, 4287, 4250, 4214),
  pub_assets = c(2060.3, 2136.5, 2167.7, 2218.4, 2255.7, 2297.0, 2307.4,
                 NA, 2379.0, 2397.8, 2433.0, 2484.0, 2498.9),
  pub_nii    = c(50.6, 55.9, 60.6, 66.1, 66.6, 69.5, 71.2,
                 75.6, 77.8, 79.5, 80.4, 84.6, 86.1),
  pub_nim    = c(2.59, 2.67, 2.86, 3.02, 3.01, 3.05, 3.12,
                 3.24, 3.32, 3.38, 3.39, 3.44, 3.49))

chk <- nim %>%
  mutate(assets_bn = round(assets / 1e9, 1),
         nii_bn    = round(nii_ytd * 4 / quarter / 1e9, 1),
         nim_ytd   = round(nim_ytd, 2)) %>%
  inner_join(pub, by = "period") %>%
  mutate(diff = round(nim_ytd - pub_nim, 2)) %>%
  select(period, n_cu, pub_n_cu, assets_bn, pub_assets, nii_bn, pub_nii, nim_ytd, pub_nim, diff)
print(as.data.frame(chk), row.names = FALSE)
## LOOK: diff within about 0.02 (rounding, later revisions); counts and assets equal or very close.
##   counts or assets off             -> population: see the type columns in [2.2] and [2.4]
##   assets right, nii_bn far off     -> the income accounts are not the ones pulled in [2.3]

## ---------------------------------------------------------------------
## [2.9] Annual NIM
## ---------------------------------------------------------------------
## A full year is the fourth-quarter nim_ytd: the year's net interest
## income over the average of assets at that year-end and the one before
## (the Q4 rows checked in [2.8]). The current year is its latest quarter:
## income year to date, annualized.
nim_annual <- nim %>%
  group_by(year) %>%
  filter(quarter == max(quarter)) %>%
  ungroup() %>%
  mutate(nim   = round(nim_ytd, NIM_DECIMALS),
         type  = if_else(quarter == 4, "Actual", "Year to date"),
         basis = if_else(quarter == 4, "Full year", paste0("Through ", period, ", annualized"))) %>%
  select(year, nim, type, basis)

print(as.data.frame(tail(nim_annual, 8)), row.names = FALSE)
## LOOK: one row per year; only the last row can be "Year to date"

## ---------------------------------------------------------------------
## [2.10] Forecast: the average of the previous five years
## ---------------------------------------------------------------------
## The averages are taken over the rounded values in the table, so each
## forecast can be checked by hand from the years named in its basis.
stopifnot(nrow(nim_annual) >= FC_WINDOW)

last_year <- max(nim_annual$year)
fc_years  <- last_year + seq_len(FC_YEARS)

nim_by_year <- setNames(nim_annual$nim, nim_annual$year)   # each forecast is added to this as it is made
fc_basis    <- character(0)
for (y in fc_years) {
  yrs <- if (FC_ROLLING) (y - FC_WINDOW):(y - 1) else (last_year - FC_WINDOW + 1):last_year
  avg <- mean(nim_by_year[as.character(yrs)])
  nim_by_year[as.character(y)] <- round(avg + 1e-9, NIM_DECIMALS)   # + 1e-9: an exact half rounds up, as in Excel
  fc_basis <- c(fc_basis, sprintf("Average of %d-%d", min(yrs), max(yrs)))
}

nim_forecast <- data.frame(year  = fc_years,
                           nim   = unname(nim_by_year[as.character(fc_years)]),
                           type  = "Forecast",
                           basis = fc_basis)

nim_annual_all <- bind_rows(nim_annual, nim_forecast)
print(as.data.frame(tail(nim_annual_all, FC_WINDOW + FC_YEARS)), row.names = FALSE)
## LOOK: the last FC_YEARS rows are the forecast; "basis" names the years behind each one

## ---------------------------------------------------------------------
## [2.11] Quarterly csv, and a quick look
## ---------------------------------------------------------------------
last_period <- nim$period[nrow(nim)]
last_period                                          ## LOOK: the quarter you expect, e.g. "2026Q3"
write.csv(nim_ts, file.path(OUT_DIR, sprintf("nim_timeseries_%s.csv", last_period)), row.names = FALSE)

nim_qtr_ts <- ts(nim$nim_qtr, start = c(nim$year[1], nim$quarter[1]), frequency = 4)
nim_ytd_ts <- ts(nim$nim_ytd, start = c(nim$year[1], nim$quarter[1]), frequency = 4)

plot(nim_qtr_ts, lwd = 2, xlab = "", ylab = "Percent of average assets",
     ylim = range(nim$nim_qtr, nim$nim_ytd, na.rm = TRUE),
     main = "Net interest margin, federally insured credit unions")
lines(nim_ytd_ts, lty = 2)
legend("top", c("In the quarter, annualized (nim_qtr)", "Year to date, NCUA basis (nim_ytd)"),
       lty = c(1, 2), lwd = c(2, 1), bty = "n")

## ---------------------------------------------------------------------
## [2.12] Excel workbook: annual history and forecast, quarterly series
## ---------------------------------------------------------------------
## openxlsx is blocked on this machine and there is no zip program on it,
## so the workbook is written with base R alone. A .xlsx file is a zip of
## a few small XML files: (a) lays out the two tabs, (b) builds the XML,
## (c) writes the zip, (d) checks the file and opens it in Excel.

## (a) The two tabs
xl_annual <- as.data.frame(nim_annual_all)
names(xl_annual) <- c("Year", "NIM (% of average assets)", "Type", "Basis")

xl_quarterly <- as.data.frame(nim_ts)
names(xl_quarterly) <- c("Period", "Year", "Quarter", "Credit unions",
                         "NIM year to date (%)", "NIM in the quarter (%)")

xlsx_file <- file.path(OUT_DIR, sprintf("nim_timeseries_%s.xlsx", last_period))
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
##   style  cell style of each column: 0 plain, 2 annual NIM, 3 three decimals, 4 thousands separator
##   width  column widths
xl_sheet <- function(d, style, width, selected = FALSE) {
  cols  <- seq_along(d)
  cells <- lapply(cols, function(j) {
    ref <- paste0(LETTERS[j], seq_len(nrow(d)) + 1)
    v   <- d[[j]]
    if (is.numeric(v)) {                               # a missing or infinite number is left as an empty cell
      ifelse(is.finite(v), sprintf('<c r="%s" s="%d"><v>%s</v></c>', ref, style[j], sprintf("%.15g", v)), "")
    } else {
      sprintf('<c r="%s" t="inlineStr"><is><t>%s</t></is></c>', ref, xml_text(as.character(v)))
    }
  })
  header <- sprintf('<c r="%s1" s="1" t="inlineStr"><is><t>%s</t></is></c>', LETTERS[cols], xml_text(names(d)))
  rows   <- c(paste(header, collapse = ""), do.call(paste0, cells))
  paste0(XML_HEAD, '<worksheet ', NS_MAIN, '>',
         '<sheetViews><sheetView', if (selected) ' tabSelected="1"' else '', ' workbookViewId="0">',
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

num_fmt <- paste0("0.", strrep("0", NIM_DECIMALS))               # 0.00 when NIM_DECIMALS is 2

parts <- c(
  "[Content_Types].xml" = paste0(XML_HEAD, '<Types xmlns="', NS_PKG, 'content-types">',
    '<Default Extension="rels" ContentType="application/vnd.openxmlformats-package.relationships+xml"/>',
    '<Default Extension="xml" ContentType="application/xml"/>',
    '<Override PartName="/xl/workbook.xml" ContentType="', TYPE_XL, 'sheet.main+xml"/>',
    '<Override PartName="/xl/styles.xml" ContentType="', TYPE_XL, 'styles+xml"/>',
    '<Override PartName="/xl/worksheets/sheet1.xml" ContentType="', TYPE_XL, 'worksheet+xml"/>',
    '<Override PartName="/xl/worksheets/sheet2.xml" ContentType="', TYPE_XL, 'worksheet+xml"/>',
    '</Types>'),
  "_rels/.rels" = paste0(XML_HEAD, '<Relationships xmlns="', NS_PKG, 'relationships">',
    '<Relationship Id="rId1" Type="', REL_DOC, '/officeDocument" Target="xl/workbook.xml"/>',
    '</Relationships>'),
  "xl/workbook.xml" = paste0(XML_HEAD, '<workbook ', NS_MAIN, ' xmlns:r="', REL_DOC, '">',
    '<bookViews><workbookView/></bookViews>',
    '<sheets><sheet name="Annual NIM" sheetId="1" r:id="rId1"/>',
    '<sheet name="Quarterly NIM" sheetId="2" r:id="rId2"/></sheets></workbook>'),
  "xl/_rels/workbook.xml.rels" = paste0(XML_HEAD, '<Relationships xmlns="', NS_PKG, 'relationships">',
    '<Relationship Id="rId1" Type="', REL_DOC, '/worksheet" Target="worksheets/sheet1.xml"/>',
    '<Relationship Id="rId2" Type="', REL_DOC, '/worksheet" Target="worksheets/sheet2.xml"/>',
    '<Relationship Id="rId3" Type="', REL_DOC, '/styles" Target="styles.xml"/>',
    '</Relationships>'),
  "xl/styles.xml" = paste0(XML_HEAD, '<styleSheet ', NS_MAIN, '>',
    '<numFmts count="2"><numFmt numFmtId="164" formatCode="', num_fmt, '"/>',
    '<numFmt numFmtId="165" formatCode="0.000"/></numFmts>',
    '<fonts count="2"><font><sz val="11"/><name val="Calibri"/><family val="2"/></font>',
    '<font><b/><sz val="11"/><name val="Calibri"/><family val="2"/></font></fonts>',
    '<fills count="2"><fill><patternFill patternType="none"/></fill>',
    '<fill><patternFill patternType="gray125"/></fill></fills>',
    '<borders count="1"><border><left/><right/><top/><bottom/><diagonal/></border></borders>',
    '<cellStyleXfs count="1"><xf numFmtId="0" fontId="0" fillId="0" borderId="0"/></cellStyleXfs>',
    '<cellXfs count="5">',
    '<xf numFmtId="0" fontId="0" fillId="0" borderId="0" xfId="0"/>',                           # 0 plain
    '<xf numFmtId="0" fontId="1" fillId="0" borderId="0" xfId="0" applyFont="1"/>',             # 1 bold header
    '<xf numFmtId="164" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',   # 2 annual NIM
    '<xf numFmtId="165" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',   # 3 three decimals
    '<xf numFmtId="3" fontId="0" fillId="0" borderId="0" xfId="0" applyNumberFormat="1"/>',     # 4 thousands
    '</cellXfs>',
    '<cellStyles count="1"><cellStyle name="Normal" xfId="0" builtinId="0"/></cellStyles>',
    '</styleSheet>'),
  "xl/worksheets/sheet1.xml" = xl_sheet(xl_annual,    style = c(0, 2, 0, 0),       width = c(8, 28, 14, 30),
                                        selected = TRUE),
  "xl/worksheets/sheet2.xml" = xl_sheet(xl_quarterly, style = c(0, 0, 0, 4, 3, 3), width = c(10, 8, 10, 14, 22, 24)))

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
