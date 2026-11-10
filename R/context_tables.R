# ---------------------------------------------------------------------------
# Context tables used by the gas reports. Edit these by hand.
#
#   political_control : one row every time the President, House, or Senate
#                       majority changed. Each row lasts until the next row.
#   key_events        : events worth labeling on the price charts.
# ---------------------------------------------------------------------------

library(tibble)

# D = Democrat, R = Republican.
# Senate notes: a 50-50 Senate goes to the Vice President's party
# (2001 and 2021), and Sen. Jeffords' party switch on 2001-06-06 flipped it to D.
political_control <- tribble(
  ~start,        ~president,          ~pres_party, ~house, ~senate,
  "1993-01-20",  "Bill Clinton",      "D",         "D",    "D",
  "1995-01-03",  "Bill Clinton",      "D",         "R",    "R",
  "2001-01-03",  "Bill Clinton",      "D",         "R",    "D",
  "2001-01-20",  "George W. Bush",    "R",         "R",    "R",
  "2001-06-06",  "George W. Bush",    "R",         "R",    "D",
  "2003-01-03",  "George W. Bush",    "R",         "R",    "R",
  "2007-01-03",  "George W. Bush",    "R",         "D",    "D",
  "2009-01-20",  "Barack Obama",      "D",         "D",    "D",
  "2011-01-03",  "Barack Obama",      "D",         "R",    "D",
  "2015-01-03",  "Barack Obama",      "D",         "R",    "R",
  "2017-01-20",  "Donald Trump",      "R",         "R",    "R",
  "2019-01-03",  "Donald Trump",      "R",         "D",    "R",
  "2021-01-20",  "Joe Biden",         "D",         "D",    "D",
  "2023-01-03",  "Joe Biden",         "D",         "R",    "D",
  "2025-01-03",  "Joe Biden",         "D",         "R",    "R",
  "2025-01-20",  "Donald Trump",      "R",         "R",    "R"
)
political_control$start <- as.Date(political_control$start)

key_events <- tribble(
  ~date,         ~event,
  "2003-03-20",  "Iraq War begins",
  "2005-08-29",  "Hurricane Katrina hits Gulf refineries",
  "2008-07-11",  "Crude oil peaks near $147/barrel",
  "2008-09-15",  "Financial crisis (Lehman collapse)",
  "2011-02-15",  "Arab Spring / Libya supply cut",
  "2014-11-27",  "OPEC refuses to cut output; oil glut",
  "2020-03-11",  "COVID-19 declared a pandemic",
  "2022-02-24",  "Russia invades Ukraine",
  "2022-06-13",  "National average tops $5",
  "2026-02-28",  "U.S./Israel strike Iran; Strait of Hormuz disrupted",
  "2026-09-15",  "Brent crude spikes to ~$131/barrel"
)
key_events$date <- as.Date(key_events$date)
