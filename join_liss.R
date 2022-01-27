# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("haven")
# install.packages("stringdist")

library(tidyverse)
library(magrittr)
library(haven)
library(stringdist)

# download LISS data ------------------------------------------------------

# Part of more user-friendly download feature under development
make_dir <-
  compose(~ path.expand(paste0("~/", .x)) %>%
    list(
      list.dirs(., recursive = FALSE),
      paste0(., "/LISS data")
    ) %>%
    unlist() %>%
    make.unique(sep = " ") %>%
    last() %>%
    assign(x = "save_to", envir = .GlobalEnv) %T>%
    dir.create())

get_liss <- function(.folder = "Downloads", .quiet = FALSE) {
  login <- askYesNo("Are you logged in to the LISS data archive?", prompts = c("yes", "no", "login"))
  liss_url <- "https://www.dataarchive.lissdata.nl/hosted_files/download/"
  if (login) {
   # make_dir(.folder)
    view <- c(
      1091, 485, 1089, 3284, 1436, 2074, 3298, 3120, 3302, 3384, 3807, 5233, 5285,
      6429, 188, 584, 636, 1252, 1635, 5137, 3055, 3767, 3775, 3783, 3785, 3823,
      5203, 5191, 6373, 182, 656, 1637, 3406, 3325, 3889, 5647, 209, 211, 391, 846,
      1284, 1681, 2454, 3157, 3286, 3741, 4063, 4917, 6043, 221, 349, 588, 967,
      1354, 2194, 2929, 3146, 3340, 3851, 4139, 4867, 6209, 5053, 684, 680, 1204,
      2559, 2306, 2620, 6091, 6115, 6107, 6123, 6131, 6257, 6419, 243, 2939, 1142,
      2551, 2563, 2569, 3209, 3211, 3611, 3697, 5151, 5315, 6241, 129, 180, 457,
      1206, 1338, 3865, 3869, 3189, 3365, 3589, 4549, 5143, 5717, 5753, 5711, 4073,
      3707, 3292, 3293, 3139, 2685, 2272, 1290, 958, 443, 141, 43, 5543, 120, 1054,
      1056, 777, 1342, 1678, 2494, 3258, 3260, 3435, 3949, 5173, 1490, 1489, 1492,
      1491, 1493, 1494, 1496, 1495, 1497, 1498, 1499, 1500, 1501, 1505, 1506, 1507,
      1508, 1509, 1510, 1511, 3119, 1513, 1514, 1515, 1516, 1517, 1518, 1519, 1520,
      1521, 1522, 1523, 1524, 1525, 1526, 1527, 1528, 1529, 1530, 1531, 1532, 1533,
      1534, 1535, 1536, 1538, 1537, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546,
      1580, 1581, 1582, 1643, 1644, 1645, 1866, 1865, 2025, 2157, 2160, 2162, 2435,
      2436, 2437, 2438, 2439, 2449, 2682, 2683, 2684, 2802, 2803, 2804, 2886, 2887,
      2888, 2943, 2945, 2944, 3093, 3094, 3133, 3134, 3148, 3135, 3149, 3171, 3150,
      3172, 3201, 3202, 3203, 3204, 3205, 3206, 3232, 3233, 3234, 3308, 3309, 3311,
      3310, 3331, 3332, 3422, 3423, 3424, 3425, 3487, 3485, 3489, 3491, 3721, 3719,
      3723, 3725, 3841, 3843, 3879, 3845, 3881, 3883, 3989, 3991, 3993, 4101, 4103,
      4105, 4257, 4255, 4435, 4437, 4439, 4539, 4541, 4543, 4629, 4631, 4633, 4985,
      4987, 5373, 5375, 5377, 5679, 5681, 5683, 5923, 5925, 5927, 6357, 6359, 6361,
      6619, 6623, 6621, 6807, 6805
    )
    # progress bar is part of new download feature under development
    iwalk(view, ~ .x %T>%
      {
        cat("\rProgress: ", round(which(equals(view, .)) / length(view) * 100, 2), "%\t\t", sep = "")
      } %>%
      c(liss_url, .) %>%
      rev() %>%
      browseURL(browser = getOption("browser")))
  } else {
    browseURL("https://www.dataarchive.lissdata.nl/users/login")
  }
}


# helper functions --------------------------------------------------------

extract <- magrittr::extract

# Creating a function that will return the most common value in a vector.
form <-
  compose(~ table(.x, exclude = "") %>%
    sort() %>%
    tail(1) %>%
    names())

# Taking two data frames and joining them together based on the intersection of the column names.
full_join_by <-
  compose(~ full_join(.x, .y, by = intersect(names(.x), names(.y))))

# Get the na_values and na_range attributes of each column,
# and keep only those that are finite.
# If the length of the resulting list is greater than 0,
# return the range of the values, otherwise return NA.
user_na_range <-
  compose(~ attributes(.x) %>%
    .[c("na_values", "na_range")] %>%
    unlist() %>%
    keep(~ is.finite(.)) %>%
    {
      ifelse(`>`(length(.), 0),
        range(., na.rm = TRUE) %>% list(), NA
      )
    } %>%
    simplify())

user_na_values <-
  compose(~ list(attr(.x, "labels"), user_na_range(.x)) %$%
    list(
      map(., ~ as.character(.)) %>%
        as.list() %>%
        set_names(letters[seq_along(.)]) %>%
        cross_df() %>%
        distinct() %>%
        rbind(data.frame(a = NA_character_, b = NA_character_)) %>%
        mutate(c = stringdist(a, b) %>% `<=`(0.5)) %>%
        mutate(a = as.numeric(a)) %>%
        .[-c(nrow(.)), ],
      data.frame(a = .[[1]], d = between(.[[1]], .[[2]][1], .[[2]][2]))
    ) %>%
    reduce(full_join_by) %>%
    mutate(e = c + d >= 0.5) %>%
    select(a, e) %>%
    distinct(.) %$%
    .[[1]][which(.[[2]], useNames = TRUE)])

# If the length of the vector is greater than or equal to 2, return a vector of the first and last
# elements of the vector, with the vector's name appended to each element. Otherwise, return an empty string.
first_last_label <-
  compose(~ if (length(.x) >= 2) {
    list(as.numeric(.x), names(.x)) %>%
      map(~ c(first(., 1), tail(., 1))) %>%
      transpose() %>%
      map_chr(~ reduce(., paste)) %>%
      paste0(collapse = " ")
  } else {
    ""
  })

# Taking the data frame and selecting the columns that match the pattern "_encr" and "wave" and then
# sorting the names of the columns.
rearrange <-
  compose(~ select(.x, sort(names(.x))) %>% select(matches(c("_encr", "wave")), everything()))

# The function takes a string as input and returns a string as output.
# First, it removes all non-alphanumeric characters from the input string,
# then converts all alphanumeric characters to lowercase,
# then removes all consecutive whitespace from the string, and
# then returns the modified string.
str_alnum <- function(.x, .lower = TRUE, .str = FALSE) {
  gsub("[^[:alnum:]\\s_]", " ", .x) %>%
    when(., .lower ~ tolower(.), .) %>%
    when(., .str ~ paste0(., collapse = " "), .) %>%
    str_squish()
}

# load, prep, group and join modules --------------------------------------

merge_liss <- function(.path, .all_bck = FALSE) {

  # Read all the .sav files in the current directory and all subdirectories,
  # and split them into two lists, one for models and one for backgrounds.
  liss <-
    list.files(.path, pattern = ".sav$", full.names = TRUE, recursive = TRUE) %>%
    map(~ read_sav(., user_na = TRUE)) %>%
    split(map_lgl(., ~ grepl("[0-9]", names(.)) %>% any())) %>%
    set_names(names(.) %>% str_replace_all(c("TRUE" = "mdl", "FALSE" = "bck")))

  # Extracts the two-letter module key from the variable names,
  # create a new column with the wave number,
  # remove wave-specific elements from the variables names to allow join operation
  mdl <-
    extract(liss, "mdl") %>%
    flatten() %>%
    list(map(., ~ str_extract_all(names(.), "^[a-z]{2}", simplify = TRUE) %>%
      form())) %>%
    transpose() %>%
    map(~ set_names(.[1], .[[2]])) %>%
    flatten() %>%
    map(~ mutate(., wave = str_replace_all(names(.), "^[a-z]{2}[0-9]{2}([a-z]{1})[0-9]{3}$", "\\1") %>%
      keep(~ nchar(.) == 1) %>%
      match(letters) %>% form() %>% as.numeric()) %>%
      set_names(str_replace_all(
        names(.), c(
          "^([a-z]{2})[0-9]{2}[a-z]{1}([0-9]{3})" = "\\100a\\2",
          "^([a-z]{2})[0-9]{2}[a-z]{1}_m" = "wave_date"
        )
      )) %>%
      rearrange()) %>%
    split(names(.))

  # Temporarily append all variables names with their respective first & last label and class
  # to ensure only similar variables are merged
  mdl %<>%
    map_depth(3, ~ paste(
      list(attr(., "labels"), user_na_values(.)) %>%
        {
          .[[1]][-which(.[[1]] %in% .[[2]])]
        } %>%
        first_last_label(),
      class(.) %>% discard(grepl("_", .)) %>% paste0(collapse = " ")
    )) %>%
    map_depth(2, ~ unlist(.) %>%
      {
        paste(names(.), unname(.)) %>%
          str_alnum() %>%
          str_replace_all(" ", ".")
      }) %>%
    list(mdl) %$%
    map2(.[[1]], .[[2]], ~ map2(.x, .y, ~ set_names(.y, .x))) %>%
    map(~ reduce(., full_join_by)) %>%
    map(~ set_names(., names(.) %>%
      str_replace("^(.*?)\\..*?$", "\\1") %>%
      make.unique(sep = "_"))) %>%
    map(~ rearrange(.)) %>%
    reduce(full_join_by)

  # Taking the data from the liss data frame and extracting the bck column.
  bck <-
    extract(liss, "bck") %>%
    flatten() %>%
    reduce(full_join_by)

  # Return either a data frame including all background data or only the instances occurring in the module data.
  list(mdl, bck) %>%
    {
      if (isTRUE(all_bck)) {
        reduce(., full_join_by)
      } else {
        reduce(., left_join, by = reduce(map(., ~ names(.)), intersect))
      }
    }
}
