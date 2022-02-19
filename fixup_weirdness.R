# fixup dates.

# On 23 Nov MoH moved ~7,000 or so first and second doses into the "Overseas/Unknown" category.
# They were shifted back on 25 Nov.

round_mean <- function(x) {
  if (all(is.na(x))) {
    return(x)
  }
#  cat("round_mean on ", length(x), "\n")
#  print(x)
  ans <- rep(floor(mean(x)), length(x))
  num_extra <- sum(x) - sum(ans)
  ans[seq_len(num_extra)] <- ans[seq_len(num_extra)] + 1
  stopifnot(sum(ans) == sum(x))
  ans
}

my_cumsum <- function(x) {
  # any NA at the start of x gets ignored...
  first_non_na = match(FALSE, is.na(x))
  if (all(is.na(first_non_na))) {
    return(rep(NA, length(x)))
  }
  if (first_non_na == 1)
    return(cumsum(x))
  c(x[seq_len(first_non_na-1)], cumsum(x[-seq_len(first_non_na-1)]))
}

fixup_weirdness <- function(dat) {
  # Now update the counts accordingly. We want to take the average of the 23rd and 25th
  fixup <- dat %>% filter(Date %in% c(ymd("2021-11-23", "2021-11-25"))) %>%
    group_by(DHB, Dose) %>%
    mutate(Number = round_mean(Number))

  fixed <- bind_rows(fixup, dat %>% anti_join(fixup %>% select(-Number)))

  # OK, now fixup the main counts
  fixed_up <- fixed %>%
    group_by(DHB, Dose) %>%
    arrange(Date) %>%
    mutate(FixedVacc = if_else(is.na(Number), Vacc, Number)) %>%
    mutate(FixedVacc = my_cumsum(FixedVacc))

  out <- fixed_up %>% select(-Vacc) %>% rename(Vacc = FixedVacc) %>%
    ungroup()

  # fixup Northland for 1 and 2 Dec
  fixup <- out %>% filter(DHB == "Northland",
                 Date %in% c(ymd("2021-12-01", "2021-12-02"))) %>%
    group_by(Dose) %>%
    mutate(Number = round_mean(Number))

  fixed <- bind_rows(fixup, out %>% anti_join(fixup %>% select(-Number)))

  fixed_up <- fixed %>%
    group_by(DHB, Dose) %>%
    arrange(Date) %>%
    mutate(FixedVacc = if_else(is.na(Number), Vacc, Number)) %>%
    mutate(FixedVacc = my_cumsum(FixedVacc))

  out <- fixed_up %>% select(-Vacc) %>% rename(Vacc = FixedVacc) %>%
    ungroup()
}

