
# LISS household income: identifying and resolving within-household outliers,
# written before I knew what tidyverse was.

### load packages
library(haven)
library(dplyr)
library(zoo)
library(stats)
library(sjlabelled)
library(imputeTS)

options(scipen = 9)

### helper functions
# get mode excluding NA, largest value is returned if 50/50
Mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- sort(unique(rna(x)), decreasing = FALSE)
  return(ux[which.max(tabulate(match(x, ux)))])
}

# clean way to rename column names
rename <- function(df, old, new) {
  for (i in 1:length(old)) {
    x <- which(names(df) == old[i])
    names(df)[x] <- new[i]
  }
  return(df)
}

# shorter and more inclusive version of 'na.omit()'
rna <- function(x, flat = FALSE) {
  if (flat == TRUE) {
    x <- as.numeric(unlist(x))
  }
  ifelse(is.list(x),
    x <- na.omit(x),
    x <- x[is.finite(x)]
  )
  return(x)
}

# get rows that satisfy a condition
rows <- function(argument, ind = FALSE) {
  arg <- substitute(argument)
  x <- as.character(arg)
  obj <- x[grep("[$|:]", x)]
  i <- which(eval(arg))
  val <- eval(parse(text = paste0(obj, "[", deparse(arg), "]")))
  ifelse(ind == FALSE, return(val), return(i))
}

# replace value ('ind' of 'col') with NA with NA while simultaneously copying its original value to dummy variable
dummy_na <- function(df, ind, col, dummy, copy_value = TRUE, print_length = FALSE) {
  ifelse(copy_value == TRUE,
    d_value <- df[[col]][ind],
    d_value <- 1
  )
  df[[dummy]][ind] <- d_value
  df[[col]][ind] <- NA
  if (print_length == TRUE) {
    cat(length(ind), "\n")
  }
  return(df)
}

# moving average without missing values
mav <- function(...) {
  rollmean(rna(...), k = 1)
}

# get power of 10, either truncate, ceiling or nearest based on cut-off point
get_power10 <- function(x, type = "nearest", cutoff = 0.75) {
  x <- rna(unlist(x))
  l <- list(10^trunc(log10(x)), 10^ceiling(log10(x)))
  if (type == "trunc") {
    p <- ls[[1]]
  } else if (type == "ceiling") {
    p <- ls[[2]]
  } else if (type == "nearest") {
    p <- sapply(seq_along(x), function(i) {
      ifelse(x[i] / l[[1]][i] > (cutoff * 10), l[[2]][i], l[[1]][i])
    })
  }
  return(p)
}

# calculate percent log difference over valid scores
lag_diff <- function(df, power10 = TRUE) {
  x <- rna(df$nethh, TRUE)
  change <- function(k) {
    delt <- log(k / lag(k, 1))
    trim <- signif(mean(abs(rna(delt))), 2)
    return(trim)
  }
  d <- sapply(seq_along(x), function(i) change(x[(i - 1):(i + 1)]))
  df$diff[!is.na(df$nethh)] <- d
  if (power10 == TRUE) {
    df$power10[!is.na(df$nethh)] <- get_power10(x)
  }
  return(df)
}

# get row mean across defined column(s)
row_by_col <- function(x = 1:nrow(income), col = c("nethh_min", "nethh_max"), value = "median") {
  value <- eval(parse(text = value))
  ifelse(length(col) > 1,
    val <- value(rowMeans(rna(income[x, col]))),
    val <- value(rna(income[x, col]))
  )
  return(val)
}

# reference values based on computations with within and between values to asses best fitting substitute value
ref <- function(x) {
  rr <- which(df$nethh %in% rna(df$nethh[x]))
  rr <- rna(c(df$nethh[-rr], mav(df$nethh), mav(rowMeans(rna(df[, 5:6]))), median(rowMeans(rna(income[, 5:6])))))
  return(rr)
}

# median (or mean,...) of 'key' across households 'y' which have similar characteristics as household 'x'
similar_cases <- function(x, y, ref, key, method = "max", value = "median", min_n = 1) {
  m <- function(i) getFunction(method)(rna(i))
  base <- which(!is.na(y[ref]))
  if (is.integer(key)) {
    key <- names(x)[key]
  }
  key <- names(which(colSums(is.na(x[key])) != nrow(x)))
  for (i in seq_along(key)) {
    k <- which(y[key[i]] == m(x[key[i]]))
    ifelse(length(intersect(base, k)) >= min_n,
      base <- intersect(base, k),
      next
    )
  }
  p <- getFunction(value)(y[base, ref])
  return(p)
}

### prepare data
# load merged data
# background <- haven::read_spss("/Users/siard/Documents/RU/CRP/income/background.sav", user_na = TRUE)
# income <- haven::read_spss("/Users/siard/Documents/RU/CRP/income/income.sav", user_na = TRUE)
#
# # add background variables to income: household id + wave nr from module
# background_ <- inner_join(background, income[, 1:3], by = c("nomem_encr", "wave"))
# income_ <- full_join(background_, income, by = c("nomem_encr", "wave", "wavenr"))

# rename items and select relevant ones for analysis
# ci00a339 = "What was total net income of your household over the period from 1 January 20XX to 31 December 20XX?"
# ci00a229 = "Can you perhaps indicate, then, within what limits the total net income of your household lay in the period from 1 January 20XX to 31 December 20XX?
# ci00a001 = "Position in the household"

income <-
  rename(
    income_, c("ci00a339", "ci00a229", "ci00a001", "wave"),
    c("nethh", "nethh_min", "positiehh", "wavenr")
  )
income$nethh_max <- income$nethh_min
income <- income[, c("nomem_encr", "nohouse_encr", "wavenr", "nethh", "nethh_min", "nethh_max", "brutoink", "nettoink", "aantalhh", "positiehh", "belbezig", "leeftijd", "oplmet", "geslacht")]

# make all income items numeric
i <- grep("net|brut", names(income))
income[, i] <- lapply(income[, i], function(x) abs(as.numeric(x)))

### make invalid scores NA: either user NA or values falling outside valid range
income$user_na <- income$is_na <- NA # default is NA because 0 could be a 'valid' score
i <- which(income$nethh_min < 1 | income$nethh_min > 7)
income <- dummy_na(income, i, "nethh_min", "user_na")
income <- dummy_na(income, i, "nethh_max", "user_na")

# find and replace labeled NA & 9999999 (not labeled but still a missing)
na <- unique(unlist(sapply(income, function(x) c(attr(x[[1]], "na_values", attr(x[[1]], "na_range")))))) # get user NA
for (c in 1:ncol(income)) {
  i <- which(is.element(unlist(income[, c]), na) | abs(unlist(income[, c])) > 9999999)
  if (length(i)) {
    income[i, c] <- NA
    income$user_na[i] <- 1
  }
}

# create data frame without labels to improve runtime
income <- data.frame(remove_all_labels(income))

# change yearly household income to NA when <= 100 and when ≈ monthly personal income (only when the former < 10.000, otherwise the latter could be the former due to input error)
x <- which(with(income, nethh < 10 | (!is.na(brutoink) & nethh <= 100) | (nethh < 10000 & ((abs(nethh - nettoink) < 100) | (abs(nethh - brutoink) < 100))))) # 100 is based on data screening, yet still arbitrary
income$is_na[x] <- income$nethh[x]
income$nethh[x] <- NA

# transform income category indicator to its lower and upper value, 120.000 represents category "60.000 euros or more"
upper <- c(8000, 16000, 24000, 36000, 48000, 60000, 120000)
lower <- c(0, 8000, 16000, 24000, 36000, 48000, 60000)
income$nethh_min <- lower[match(income$nethh_min, 1:7)]
income$nethh_max <- upper[match(income$nethh_max, 1:7)]

# reverse some ordinal values to make highest score the most relevant one for outlier analysis
i <- which(income$oplmet > 6) # higher score = more education
income$oplmet[i] <- -as.numeric(income$oplmet[i])
i <- which(income$belbezig > 1) # make "paid employment" highest score
income$belbezig[i] <- -as.numeric(income$belbezig[i])
income$positiehh <- (8 - income$positiehh) # reverse all values so higher values = adult (higher probability of knowing household income)

# get all unique household ids n = 9.540
hh <- unique(income$nohouse_encr)
income$valid_hh <- income$power10 <- income$outlier <- income$diff <- 0 # new variables
rest_hh <- c() # empty list for households with insufficient data for reliable estimates
income_copy <- income

tryCatch(
  {
    for (h in 1:length(hh)) {
      cat("\r ", hh[h], " ", format(round(h / length(hh) * 100, 2), nsmall = 2), "%") # progress}

      df <- income[which(income$nohouse_encr == hh[h]), ]
      # df <- income[which(income$nohouse_encr == 500277), ]
      # df$valid_hh <- length(which(rowSums(!is.na(income[which(income$nohouse_encr == hh[h]), 4:5])) != 0))  # cases without missing values for household income (or its indication)
      df[5:8][df[5:8] < 10] <- NA # remove invalid scores which bias analysis

      if (length(rna(df$nethh)) > 1) {
        loop <- rep <- err <- 0
        df <- lag_diff(df)
        mode_power10 <- Mode(rows(df$power10 > 0))

        # preliminary checks
        if (all(df$power10 < 10000)) {
          i <- which(df$power10 > 0)
          df$outlier[i] <- df$nethh[i]
          ifelse(all(df$diff[i] < 0.6),
            x <- (10000 / df$power10[i]),
            x <- min(rna(df$power10[i]))
          )
          df$nethh[i] <- df$nethh[i] * x
        }
        if (Mode(df$power10) == 1000) {
          i <- which(df$power10 == 1000)
          m <- mean(df$nethh[i])
          if (which.min(abs(row_by_col() - c(m, m * 10))) == 2) {
            df$nethh[i] <- df$nethh[i] * 10
          }
        }
        wave_cases <- Filter(nrow, lapply(unique(df$wavenr), function(w) {
          rna(unique(df[df$wavenr == w, c("wavenr", "nethh")]))
        }))
        cases <- sapply(wave_cases, `[[`, 1)
        if (any(lengths(cases)) > 1) {
          for (i in seq_along(cases)) {
            w <- which(df$wavenr == 4)
            if (any(df$diff[w] >= 0.5)) {
              k <- w[which.min(df$diff[w])]
              w <- setdiff(w, k)
              for (i in 1:nrow(w)) {
                if (k$outlier[w] == 0) {
                  df$outlier[w[i]] <- df$nethh[w[i]]
                  df$nethh[w[i]] <- df$nethh[k]
                }
              }
            }
          }
        }

        # iteratively analyze and impute outliers
        while (length(err)) {

          ## locate outlier(s)
          df <- lag_diff(df)
          if (length(unique(rows(df$power10 > 0))) > 2) {
            mode_power10 <- 10000
            skip_imp <- TRUE
          } else {
            mode_power10 <- Mode(rows(df$power10 > 1000))
            skip_imp <- FALSE
          }

          # primary conditions: asses difference between neighboring values
          err <- which(df$diff >= 0.6 & df$power10 != mode_power10)
          if (!length(err) & any(!is.na(df$nethh_max))) {
            d <- unlist(lapply(
              unique(df$wavenr),
              function(i) {
                w <- which(df$wavenr == i)
                return(df$nethh[w] - ifelse(any(!is.na(df$nethh_max[w])),
                  max(rna(df$nethh_max[w])), df$nethh[w]
                ))
              }
            ))
            if (any(!is.na(d)) && max(rna(d)) >= 1000) {
              if (df$diff[which.max(d)] >= 0.6) {
                err <- which.max(d)
              }
            }
          }

          # secondary condition: asses difference with reference values
          if (!length(err)) {
            m <- mean(ref(err))
            diff <- sapply(df$nethh, function(x) abs(log1p((x - m) / m)))
            if (any(rna(diff) > 0.9)) {
              err <- which.max(diff)
            }
          }
          if (!length(err)) {
            break
          }

          # get most extreme outlier if there are multiple suspects
          i <- ifelse(all(abs(diff(trunc(sapply(err, function(e) mean(abs(ref(err) - e))) / 100)) < 10)),
            err[which.max(df$diff[err])],
            err[which.max(sapply(err, function(e) mean(abs(ref(err) - e))))]
          )

          # check if index already passed through loop
          # same value can be analyzed twice, but not more to avoid bias towards null model
          r <- which(rep == i)
          if (any(r)) {
            if (length(err) > 1) {
              err_ <- setdiff(err, i)
              i <- err_[which.max(df$diff[err_])]
            } else if (length(r) > 2) {
              break
            }
          }
          rep <- c(rep, i)

          ## compute substitute values
          # asses conditions
          if (skip_imp == FALSE) {
            df_ <- df[, 4:9]
            df_[which((. <- df$power10) == df$power10[i] & . != mode_power10), 1] <- NA
            x <- sapply(df_, function(i) list(length(rna(i)), length(unique(rna(i)))))
            x_ <- which(x[1, ] > 2 & x[2, ] > 2)
            if ("nethh" %in% names(df_[x_])) {
              imp <- na_kalman(df_[x_], model = "StructTS", smooth = TRUE)[i, 1]
            } else if (length(rna(df_$nethh[-i])) > 1) {
              imp <- na_ma(df_$nethh)[i]
            }
          } else {
            imp <- 0
          }
          wave_cluster <- which(df$wavenr == df$wavenr[i])

          # generate substitutes, each with its own rationale
          sub <- c(
            rna(df$nethh[setdiff(wave_cluster, i)]), # other values in same wave
            df$nethh[i] * mode_power10 / df$power10[i], # computations to cover typo's and misplaced digits
            df$nethh[i] + mode_power10, df$nethh[i] - mode_power10,
            imp, # estimated value from imputation when missing (if applicable)
            mean(rna(df$nethh_max)), # mean of upper value income category
            similar_cases(df, income, "nethh", 9:13, value = "mean")
          ) # mean income of households with shared characteristics
          sub <- sub[sub >= 8000] # filter out inappropriate substitutes
          if (df$outlier[i] == 0 & is.na(df$outlier[i])) {
            df$outlier[i] <- df$nethh[i]
          }

          # asses best fitting substitute based on smallest mean of absolute difference with all reference values
          df$nethh[i] <- sub[which.min(sapply(sub, function(e) mean(abs(ref(err) - e))))]

          loop <- loop + 1
          if (loop == nrow(df)) { # avoid inf
            break
          }
        }
        # add changes to source
        if (any(df$outlier > 0)) {
          income[which(income$nohouse_encr == hh[h]), c(4, 15:20)] <- df[, c(4, 15:20)]
        } else {
          rest_hh <- c(rest_hh, hh[h])
        }
      }
    }
  },
  error = function(error_message) {
    message(error_message)
  }
)

haven::write_sav(income, "~/Documents/RU/CRP/income/output.sav")
