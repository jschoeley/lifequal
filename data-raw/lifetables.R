library(HMDHFDplus)
library(dplyr)

# Swedish period life-tables 1x1 ------------------------------------------

readHMDweb(CNTRY = "SWE", item = "fltper_1x1",
           username = "jona.s@gmx.de", password = "1320145677",
           fixup = TRUE) -> sweden_f_1x1

readHMDweb(CNTRY = "SWE", item = "mltper_1x1",
           username = "jona.s@gmx.de", password = "1320145677",
           fixup = TRUE) -> sweden_m_1x1

# bind female & male lifetables
bind_rows(female = sweden_f_1x1,
          male = sweden_m_1x1,
          .id = "sex") %>%
  # don't include OpenInterval column
  select(-OpenInterval) %>%
  # set column names to lowercase
  rename(period = Year, x = Age) -> sweden1x1

# save to /data
devtools::use_data(sweden1x1, overwrite = TRUE, compress = "xz")

# Swedish period life-tables 5x5 ------------------------------------------

readHMDweb(CNTRY = "SWE", item = "fltper_5x5",
           username = "jona.s@gmx.de", password = "1320145677",
           fixup = TRUE) -> sweden_f_5x5

readHMDweb(CNTRY = "SWE", item = "mltper_5x5",
           username = "jona.s@gmx.de", password = "1320145677",
           fixup = TRUE) -> sweden_m_5x5

# bind female & male lifetables
bind_rows(female = sweden_f_5x5,
          male = sweden_m_5x5,
          .id = "sex") %>%
  # don't include OpenInterval column
  select(-OpenInterval) %>%
  # set column names to lowercase
  rename(period = Year, x = Age) %>%
  # leave out 1751 because it does not adhere to 5-year period aggregation
  filter(period != 1751) %>%
  # convert years to year intervals
  mutate(period = paste(period, period+4, sep = "-")) %>%
  # add interval width for age groups
  # (last group is open so interval width is NA)
  group_by(sex, period) %>%
  mutate(wx = c(1, 4, rep(5, 21), NA)) %>%
  select(sex, period, x, wx, everything()) %>%
  ungroup() -> sweden5x5

# save to /data
devtools::use_data(sweden5x5, overwrite = TRUE, compress = "xz")
