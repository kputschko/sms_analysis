

# As Formula --------------------------------------------------------------

.fx_as_formula <- function(y, x) {

  require(dplyr)
  require(stringr)

  work_table <-
    tibble(y, x = list(x)) %>%
    mutate(x_list = map2(x, y, ~ setdiff(.x, .y)),
           x_string = map_chr(x_list, ~ str_c(.x, collapse = "+")),
           formula = str_glue("{y} ~ {x_string}"))

  if (nrow(work_table) == 1) {

    work_table %>%
      pull(formula) %>%
      map(as.formula) %>%
      pluck(1)

  } else {

    work_table %>%
      pull(formula) %>%
      map(as.formula)

  }

}


# Top Percent -------------------------------------------------------------

.fx_top_p <- function(.data, .p, .wt) {

  library(tidyverse)
  library(rlang)

  n <- nrow(.data)
  p <- ceiling(n * .p)

  top_n(x = .data, n = p, wt = !!sym(.wt))

}


# Nesting -----------------------------------------------------------------

.fx_nest <- function(data) {

# Use: To prepare for creating many models
# Assumes: Input data is ready for subsetting, and groups have been applied


  require(tidyr)
  require(tibble)

  .groups <-
    data %>%
    group_vars %>%
    length()

  if (.groups == 0) {

    tribble(~group, ~data,
            "NA",   data) %>%
      rowid_to_column("index")

  } else {

    data %>% nest() %>% rowid_to_column("index")

  }

}



# PMAS Manipulate ---------------------------------------------------------

.fx_manipulate <- function(.data,
                           .group = NULL,
                           .mutate = NULL,
                           .filter = NULL,
                           .binary = NULL,
                           .exclude = NULL,
                           .response = NULL,
                           .sample_frac = 1,
                           .strings_to_factor = TRUE) {

  require(dplyr)
  require(purrr)
  require(rlang)
  require(stringr)

  # === Validate Input ===
  if (is.numeric(.sample_frac)) {stopifnot(.sample_frac %>% between(0, 1))}

  if (!is.null(.mutate)) {stopifnot(is_named(.mutate),
                                    is_list(.mutate))}

  if (!is.null(.group))    {stopifnot(is.character(.group))}
  if (!is.null(.binary))   {stopifnot(is.character(.binary))}
  if (!is.null(.filter))   {stopifnot(is.character(.filter))}
  if (!is.null(.response)) {stopifnot(is.character(.response))}


  # === Sample Logic ===
  .sample_frac <- if (is.null(.sample_frac)) { 1L } else {.sample_frac}

  # === Mutate Logic ===
  .data_mutate <- if (is.null(.mutate)) {NULL} else {.mutate %>% map(~parse_quo(.x, env = caller_env()))}

  # === Filter Logic ===
  .data_filter <- if (is.null(.filter)) {NULL} else {.filter %>% map(~parse_quo(.x, env = caller_env()))}

  # === Group Logic ===
  .data_group <- if (is.null(.group)) {NULL} else {syms(.group)}

  # === Exclusion Logic ===
  .data_include <-
    if (is.null(.exclude)) {

      colnames(.data) %>% syms()

    } else {

      colnames(.data) %>% setdiff(.exclude) %>% union(names(.mutate)) %>% syms()

    }

  # === Binary Logic ===
  .data_binary <-
    if (is.null(.binary)) {NULL
    } else {

      binary_values <- str_glue("ifelse({.binary} > 0, 1, 0)") %>% as_list()
      binary_names  <- str_c("bin", .binary, sep = "_")
      binary_list   <- set_names(x = binary_values, nm = binary_names)
      binary_logic  <- map(binary_list, ~parse_quo(.x, env = caller_env()))

    }

  # === Respone na.rm ===
  .data_response_na.rm <-
    if (is.null(.response)) {NULL
    } else {

      str_glue("!is.na({.response})") %>%
        str_c(collapse = " | ") %>%
        map(~parse_quo(.x, env = caller_env()))

    }


  # === Apply Logic ===
  .data_output <-
    .data %>%
    mutate(!!! .data_mutate) %>%
    mutate(!!! .data_binary) %>%
    filter(!!! .data_filter) %>%
    filter(!!! .data_response_na.rm) %>%
    group_by(!!! .data_group) %>%
    select(!!! .data_include)


  # === Sample ===
  if (.sample_frac < 1) {

    .data_output <-
      .data_output %>%
      sample_frac(size = .sample_frac)

  }


  # === Strings to Factor ===
  if (.strings_to_factor) {

    cols_factor <-
      .data_output %>%
      select_if(is.character) %>%
      colnames() %>%
      union(names(.data_binary))

    .data_output <-
      .data_output %>%
      ungroup %>%
      mutate_at(cols_factor, as.factor) %>%
      group_by(!!! .data_group)

  }


  return(.data_output)

}






# Describe ----------------------------------------------------------------


.fx_describe <- function(data) {

  require(foreach)
  require(stringr)
  require(rlang)

  stopifnot(is.data.frame(data))


  foreach(i = 1:ncol(data), .combine = "rbind") %do% {

    column_name  <- data %>% colnames() %>% pluck(i)
    column_type  <- data[[column_name]] %>% class()
    column_rlang <- sym(column_name)
    filter_rlang <- parse_quo(x = str_glue("is.na({column_name})"), env = caller_env())

    # n_missing <- data %>% filter(!!! filter_rlang) %>% nrow()

    if (column_type %in% c("factor", "character")) {

      data %>%
        summarise(n = n(),
                  n_distinct = n_distinct(!! column_rlang, na.rm = TRUE),
                  n_missing  = sum(is.na(!! column_rlang))) %>%
        mutate(column_name = !! column_name,
               column_type = !! column_type,
               min = NA,
               med = NA,
               max = NA,
               mean = NA,
               sd = NA
               ) %>%
        select(column_name, column_type,
               n, n_missing, n_distinct,
               min, max, mean, sd)

    } else {

      data %>%
        summarise(n = n(),
                  n_distinct = n_distinct(!! column_rlang, na.rm = TRUE),
                  n_missing  = sum(is.na(!! column_rlang)),
                  mean = mean(!! column_rlang, na.rm = TRUE),
                  sd = sd(!! column_rlang, na.rm = TRUE),
                  min = min(!! column_rlang, na.rm = TRUE),
                  med = median(!! column_rlang, na.rm = TRUE),
                  max = max(!! column_rlang, na.rm = TRUE)) %>%
        mutate(column_name = !! column_name,
               column_type = !! column_type) %>%
        mutate_if(.predicate = is.numeric,
                  .funs = function(x) {format(x, scientific = FALSE, digits = 4)}) %>%
        select(column_name, column_type,
               n, n_missing, n_distinct,
               min, max, mean, sd)


    }



  }

}
