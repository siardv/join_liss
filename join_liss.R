# install.packages("tidyverse")
# install.packages("magrittr")
# install.packages("haven")
# install.packages("stringdist")

library(tidyverse)
library(magrittr)
library(haven)
library(stringdist)

# download LISS data ------------------------------------------------------

download_ids <- c(
  43,   120,  129,  141,  180,  182,  188,  209,  211,  221,  243,  349,  391,  443,  457,  485,  584,  588,  636,  656,  680,  684,  777,  846,  958,
  967,  1054, 1056, 1089, 1091, 1142, 1204, 1206, 1252, 1284, 1290, 1338, 1342, 1354, 1436, 1489, 1490, 1491, 1492, 1493, 1494, 1495, 1496, 1497, 1498,
  1499, 1500, 1501, 1505, 1506, 1507, 1508, 1509, 1510, 1511, 1513, 1514, 1515, 1516, 1517, 1518, 1519, 1520, 1521, 1522, 1523, 1524, 1525, 1526, 1527,
  1528, 1529, 1530, 1531, 1532, 1533, 1534, 1535, 1536, 1537, 1538, 1539, 1540, 1541, 1542, 1543, 1544, 1545, 1546, 1580, 1581, 1582, 1635, 1637, 1643,
  1644, 1645, 1678, 1681, 1865, 1866, 2025, 2074, 2157, 2160, 2162, 2194, 2272, 2306, 2435, 2436, 2437, 2438, 2439, 2449, 2454, 2494, 2551, 2559, 2563,
  2569, 2620, 2682, 2683, 2684, 2685, 2802, 2803, 2804, 2886, 2887, 2888, 2929, 2939, 2943, 2944, 2945, 3055, 3093, 3094, 3119, 3120, 3133, 3134, 3135,
  3139, 3146, 3148, 3149, 3150, 3157, 3171, 3172, 3189, 3201, 3202, 3203, 3204, 3205, 3206, 3209, 3211, 3232, 3233, 3234, 3258, 3260, 3284, 3286, 3292,
  3293, 3298, 3302, 3308, 3309, 3310, 3311, 3325, 3331, 3332, 3340, 3365, 3384, 3406, 3422, 3423, 3424, 3425, 3435, 3485, 3487, 3489, 3491, 3589, 3611,
  3697, 3707, 3719, 3721, 3723, 3725, 3741, 3767, 3775, 3783, 3785, 3807, 3823, 3841, 3843, 3845, 3851, 3865, 3869, 3879, 3881, 3883, 3889, 3949, 3989,
  3991, 3993, 4063, 4073, 4101, 4103, 4105, 4139, 4255, 4257, 4435, 4437, 4439, 4539, 4541, 4543, 4549, 4629, 4631, 4633, 4867, 4917, 4985, 4987, 5053,
  5137, 5143, 5151, 5173, 5191, 5203, 5233, 5285, 5315, 5373, 5375, 5377, 5543, 5647, 5679, 5681, 5683, 5711, 5717, 5753, 5923, 5925, 5927, 6043, 6091,
  6107, 6115, 6123, 6131, 6209, 6241, 6257, 6357, 6359, 6361, 6373, 6419, 6429, 6619, 6621, 6623, 6805, 6807
)

get_liss <- function(.wait = NULL) {
  login <- askYesNo("Are you logged in to the LISS data archive?", prompts = c("yes", "no", "login"))
  if (login) {
    iwalk(download_ids, ~ paste0("https://www.dataarchive.lissdata.nl/hosted_files/download/", .) %>%
      browseURL(.) %$% Sys.sleep(sum(0, .wait)))
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
