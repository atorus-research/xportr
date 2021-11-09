#' Vectorized Abbreviation
#'
#' Makes the `abbreviate()` function (from base R) vectorized to accommodate a
#' vector of differing minLength values. Cannot use `Vectorize()` here, as it
#' will lead to dupes. This method generates the abbreviations with the largest
#' minLengths first and leaves those intact as it continues onto the terms with
#' the next highest minLength values. This continues iteratively until the
#' lowest minlength is reached.
#'
#' @param names.arg a character vector of names to be abbreviated, or an object
#'   to be coerced to a character vector by as.character.
#' @param minlength the minimum length of the abbreviations, as a numeric
#'   vector. Must be same lenth as names.arg argument's vector.
#'
#' @export
#' @noRd
#'
#' @examples
#' abbreviate_v(c("Fruit Flies", "Tubular Dude"),c(4,6))
#' 
abbreviate_v <- function(names.arg, minlength){
  if(!is.numeric(minlength)) stop("minlength arg must be numeric")
  if(length(minlength) != length(names.arg)) stop("names.arg & minlength arg must be same length")
  
  # grab each unique minlength value
  abbr_lengths <- unique(minlength) %>% sort(T)
  
  # Initiate first iteration, where entire vector is a abbreviated to same length
  abbr <- abbreviate(
    names.arg,
    minlength = abbr_lengths[1],
    method = "both.sides"
  )
  
  # For any other abbreviation lengths, continue to abbreviate the abbreviations
  for(ab_len in abbr_lengths[-1]){
    abbr[minlength <= ab_len] <- abbreviate(
      abbr[minlength <= ab_len],
      minlength = ab_len,
      method = "both.sides"
    )
  }
  return(abbr)
}


#' Find and replace \%'s with "Pct"
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_grapes("Johnny owes me 90% of his sandwich")
words_grapes <- function(x) {
  gsub("\\%","Pct", x)
}


#' Find and replace _'s (underscores) with a blank space " "
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_("I_Ate")
words_ <- function(x) {
  gsub("\\_"," ",x)
}


#' Find when there are two capital letters next to each other, followed by a
#' lower case letter, then add a space between the two capital letters to
#' separate the assumed 'words'
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_ABb("IAte")
words_ABb <- function(x) {
  gsub("([[:upper:]])([[:upper:]][[:lower:]])","\\1 \\2",x)
}


#' Insert a space between a lowercase character and an uppercase to
#' separate a character string into assumed 'words'
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_aB("iAte")
words_aB <- function(x) {
  gsub("([[:lower:]])([[:upper:]])","\\1 \\2",x)
}


#' Insert a space between a number followed by a character to create the assumed
#' "words" in the character vector
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_1a(x = "iAte1grape")
words_1a <- function(x) {
  gsub("([0-9])([[:alpha:]])","\\1 \\2",x)
}


#' Insert a space between a character followed by a number to create the assumed
#' "words" in the character vector
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'
#' @noRd
#' @export
#' @examples
#' words_a1("iAte1grape")
words_a1 <- function(x) {
  gsub("([[:alpha:]])([0-9])","\\1 \\2",x)
}


#' Find and replace special characters or symbols
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param replace character to replace the special characters or symbols with;
#'   Defaults to blank space.
#'
#' @noRd
#' @export
#' @examples
#' replace_sym("i.Ate-1grape?")
replace_sym <- function(x, replace = " ") {
  gsub("[^[:alnum:]]", replace, x)
}


#' Uses a smattering of string functions to separate words that may be smashed
#' together so that the the character element is more human readable
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' 
#' @noRd
#' @export
#' @examples
#' read_words("iAteABunch_of_grapesUntil99%Full")
read_words <- function(x){
  # x %>% words_grapes() %>% words_() %>% words_ABb() %>% words_aB()
  words_aB(words_ABb(words_(words_grapes(x)))) # with no pipes
}


#' Uses a smattering of string functions to separate words and/or numbers that
#' may be smashed together so that the the character element is more human
#' readable
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' 
#' @noRd
#' @export
#' @examples
#' read_words_and_nums("iAteABunch_of_grapesUntil99%Full")
read_words_and_nums <- function(x){
  # x %>% read_words() %>% words_1a() %>% words_a1()
  words_a1(words_1a(read_words(x))) # with no pipes
}


#' Uses a smattering of string functions to separate words and/or numbers that
#' may be smashed together so that the the character element is more human
#' readable
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#'   
#' @noRd
#' @export
#' @examples
#' read_words_nums_no_sym(x = "iAteABunch_of_grapesUntil99%Full")
read_words_nums_no_sym <- function(x){
  # x %>% replace_sym() %>% read_words_and_nums()
  # read_words_and_nums(replace_sym(x)) # with no pipes
}


#' Indicate 1 if a string starts with a number, 0 otherwise
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' 
#' @noRd
#' @export
#' @examples
#' starts_with_number("iAteABunch_of_grapesUntil99%Full")
#' starts_with_number("1a.How Was Brunch?")
starts_with_number <- function(x){
  suppressWarnings(ifelse((!is.na(as.numeric(substr(x,1,1)))),1,0))
}


#' Extract Starting Prefix number
#' 
#' Return "[NUM]" if a string starts with a number, "" otherwise
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' 
#' @noRd
#' @export
#' @examples
#' prefix_num(x = "iAteABunch_of_grapesUntil99%Full")
#' prefix_num(x = "1a.How Was Brunch? Still $2.99?")
prefix_num <- function(x){
  suppressWarnings(ifelse(starts_with_number(x) == 1, readr::parse_number(x), ""))
}


#' Prefix Bundle Extraction Vessel
#'
#' Function used to simplify the extraction of certain prefix bundles, used in
#' `prefix_num_bundle().`
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param num a number, as numeric or character, that matches the first
#'   occurrence of a number in the prefix bundle. Determined automatically in
#'   `prefix_num_bundle()`
#' @param srch_patt The regular expression search pattern, expressed as a string
#'
#' @noRd
#' @export
#' @examples
#' extrct_vssl(x = "1a. How Was Brunch?", 1, "[^[:alnum:]]([0-9]){1,3}")
extrct_vssl <- function(x, num, srch_patt){
  x %>% 
    read_words() %>% 
    stringr::str_extract(paste0("(",num, srch_patt, ")"))
}


#' Extract 'prefix bundle'
#'
#' A 'prefix bundle' is a string that starts with a numeric digit and is
#' followed by 1 or 2 alpha characters. The string is passed to internal
#' `read_words()` function to parse out 'bundle' alphas from other alphas found
#' in the term. Returns "[NUM] bundle" prefix with no underscore to later be
#' appended on the back side of suggested variable name, upon user discretion
#' (it can be left on front end with a "_" prefix)
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param input_relo_2_end logical, if `TRUE` then the prefix bundle, if it
#'   exists will be scooted to the end of the string x, else it will receive the
#'   necessary "_" prefix
#'
#' @noRd
#' @export
#' @examples
#' prefix_num_bundle(x = "iAteABunch_of_grapesUntil99%Full")
#' prefix_num_bundle(x = "1a. How Was Brunch?")
#' prefix_num_bundle(x = "2aa How Was Brunch2day?")
#' prefix_num_bundle(x = "30abHow Was Brunch2day?")
#' prefix_num_bundle(x = c("30ab.How Was Brunch2day?", "40AHow Was Brunch2day?"))
#' prefix_num_bundle(x = c("iAteABunch_of_grapesUntil99%Full","30ab.How Was Brunch2day?", "40AHow Was Brunch2day?"))
prefix_num_bundle <- function(x, input_relo_2_end = T, sep = "_"){
  
  # x <- original_varname[309]
  full_bundle <- purrr::map_chr(x, function(x){
    pfix_st_num <- prefix_num(x) # grab prefix num, if it exists
    if(pfix_st_num != ""){
      fb <- case_when(
        # pfix_st_num = 3
        # Example x = "3a. hey"
        !(extrct_vssl(x, pfix_st_num, "[[:alpha:]]{1,2}[^[:alnum:]]") %>% is.na()) ~
          extrct_vssl(x, pfix_st_num, "[[:alpha:]]{1,2}[^[:alnum:]]"),
        
        # Example x = "3_17a_hey" (beatpd col pos 310 - 318)
        !(extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}[[:alpha:]][^[:alnum:]]") %>% is.na()) ~
          extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}[[:alpha:]][^[:alnum:]]"),
        
        # Example x = "3_17_hey"
        !(extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}[^[:alnum:]]") %>% is.na()) ~
          extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}[^[:alnum:]]"),
        
        # Example x = "3_17hey"
        !(extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}") %>% is.na()) ~
          extrct_vssl(x, pfix_st_num, "[^[:alnum:]]([0-9]){1,3}"),
        
        # Example x = "3hey"
        TRUE ~ extrct_vssl(x, pfix_st_num, "")
      )
      
    } else {
      fb <- ""
    }
    return(fb)
  })
  
  pfix_st_num_v <- prefix_num(x)
  
  fb_clean <- full_bundle %>%
    trimws(which = "both") %>%
    stringr::str_replace_all(" ", "_") 
  
  bundle <- ifelse(is.na(fb_clean), as.character(pfix_st_num_v), # no bundle, just digit
                   stringr::str_replace(fb_clean, "([^[:alnum:]])", "")) # remove spcl chars
  
  # if relocating bundle to end, then move it, else do nothing
  fixed <-  purrr::map2_chr(x, fb_clean, function(x, fb){
    if(input_relo_2_end){
      trimws(ifelse(fb == "", x, paste0(stringr::str_replace(x, fb, ""), sep,
                                        stringr::str_replace(fb, "([^[:alnum:]])",""))),
             which = "both")
    } else {
      x
      # ifelse(prefix_num(x) == "", x, paste0(sep, x))
    }
  })
  return(list(viable = fixed, bundle = bundle, full_bundle = fb_clean #, bundle_nchr = fb_nchar
  ))
}



#' Change Case upper or lower
#'
#' Supply a character string or vector, returns the same vector with changed
#' case. Notice, this function just uses `toupper()` or `tolower()` but the
#' string, `x`, may have been altered by the janitor package's use of
#' snakcase::to_any_case later on, when suggesting a rename
#'
#' @param x a character vector, or an object which can be coerced by
#'   as.character to a character vector. Long vectors are supported.
#' @param case character string, either "UPPER_SNAKE_CASE", "lower_snake_case",
#'   "lowerCamel" or "UpperCamel"
#'
#' @noRd
#' @export
#' @examples
#' change_case(c("hello darkness","My Old FRIEND"), "lower_snake_case" )
#' change_case(c("hello darkness","My Old FRIEND"), "UPPER_SNAKE_CASE" )
change_case <- function(x, case){
  if(case == "UPPER_SNAKE_CASE")  {toupper(x)}
  else if(case == "lower_snake_case") {tolower(x)}
  else {x} # don't amend, default to what janitor uses (via snakecase::to_any_case)
}


#' Suggest a new renamed / abbreviated term
#' 
#' Take multiple character vectors as inputs and return the final suggestion or the 
#'   transfermation method used.
#'
#' @param input_char_len the maximum char length the final suggestion can be
#' @param original_varname a character vector
#' @param dict_varname a character vector to use first if a dictionary term
#'   exists
#' @param adj_orig the original name, adjusted for any percent symbols (%) or
#'   grapes
#' @param stem the stem or root word(s), char
#' @param abbr_stem the character abbreviation of the stem or root
#' @param abbr_parsed the character abbreviation of the adj_orig
#'   
#' @examples
#' suggest_rename(
#'   input_char_len = 8,
#'   original_varname = c("", "subject id", "1c. ENT", "1b. Eyes", "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc"),
#'   dict_varname = c(NA, "SUBJID", NA, NA, NA, NA, NA, NA),
#'   use_bundle = c("","","","","","","",""),
#'   adj_orig = c("", "subject id", "1c. ENT", "1b. Eyes", "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc"),
#'   stem = c("", "subject id", "c ent", "b eye", "d lung", "e heart", "year number", "a skin desc"),
#'   abbrev = c("", "subjctid", "_1c. ENT", "_1b.Eyes", "_1d lung", "_1eheart", "yearnmbr", "_1asknds")
#'   abbr_transf = c("","","","","","","")
#'   )
suggest_rename <- function(input_char_len,
                           input_relo_2_end = T,
                           input_sep = '_',
                           original_varname,
                           dict_varname,
                           use_bundle, 
                           adj_orig, 
                           stem,
                           abbr_stem,
                           abbr_parsed
  ){

  col_pos <- seq.int(length(original_varname))
  case_when(
    
    # Name blank cols
    original_varname == "" | is.na(original_varname) ~ paste0("V",col_pos),

    # Dictionary
    !(is.na(dict_varname)) ~ sugg = dict_varname, 

    # Minor cleaning
    nchar(paste0(use_bundle, replace_sym(adj_orig, input_sep))) <= input_char_len ~
      paste0(use_bundle, replace_sym(adj_orig, input_sep)),

    nchar(paste0(use_bundle, replace_sym(adj_orig, ""))) <= input_char_len ~
      paste0(use_bundle, replace_sym(adj_orig, "")),

    # Stemming 
    nchar(paste0(use_bundle, replace_sym(stem, input_sep))) <= input_char_len ~
      paste0(use_bundle, replace_sym(stem, input_sep)),

    nchar(paste0(use_bundle, replace_sym(stem, ""))) <= input_char_len ~
      paste0(use_bundle, replace_sym(stem, "")),

    # Abbreviation
    TRUE ~ 
      case_when(
        nchar(paste0(use_bundle, abbr_parsed)) <= input_char_len & 
          nchar(abbr_parsed) >= nchar(abbr_stem) ~ paste0(use_bundle, abbr_parsed)
        
        , nchar(paste0(use_bundle, abbr_stem)) <= input_char_len ~ paste0(use_bundle, abbr_stem)
        
        , TRUE ~ "NoAbbrev"
      )
  )
}


# Test
# # my_vars01 %>%
# # mutate(
# # sugg_varname =
#       suggest_rename(
#         input_char_len = 8,
#         input_relo_2_end = T,
#         input_sep = '_',
#         original_varname = my_vars01$original_varname,
#         dict_varname = my_vars01$dict_varname,
#         use_bundle = my_vars01$use_bundle,
#         adj_orig = my_vars01$adj_orig,
#         stem = my_vars01$stem,
#         abbr_stem = my_vars01$abbr_stem,
#         abbr_parsed = my_vars01$abbr_parsed,
#       )
#   # )


#' Build Suggestion Data Frame
#'
#' Take a single character vector, x, as in input and use it to build a data
#' frame used to make a final suggestion
#'
#' @param return_df logical, defaults to TRUE where entire dataset is returned
#'   from suggestion process, else just the suggestion column itself
#' @param input_char_len numeric, the maximum length of the final suggestion
#' @param original_varname character vector
#' @param replace_vec A named character vector where the name is replaced by the
#'   value.
#' @param dict_dat a data frame
#'
#' @noRd
#' @export
#' @examples
#' tidy_rename(return_df = F, input_char_len = 8, original_varname = c("", "subject id", "1c. ENT", "1b. Eyes", "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc"), dict_dat = data.frame(original_varname = "subject id", dict_varname = "subjid", label = "Subject ID"))
#' 
tidy_rename <- function(return_df = T,
                        input_char_len = 8,
                        input_relo_2_end = F,
                        input_sep = '_',
                        original_varname,
                        replace_vec = c(
                            "'"="",
                            "\""="",
                            "%"="_pct_",
                            "#"="_nmbr_"
                          ),
                        dict_dat = data.frame(original_varname = character(),
                                              dict_varname = character(),
                                              label = character()),
                        input_changeCase = c(
                          "UPPER_SNAKE_CASE", "lower_snake_case", "lowerCamel", "UpperCamel"
                        )
){
  # decode case arg for janitor
  case_decoded <- case_when(input_changeCase %in% c("UPPER_SNAKE_CASE", "lower_snake_case") ~ "snake",
                            input_changeCase == "lowerCamel" ~ "lower_camel",
                            input_changeCase == "UpperCamel" ~ "upper_camel",
                            TRUE ~ input_changeCase)
  
  # initialize data.frame to track all moving parts when suggesting new var names
  # original_varname = original_varname[1:30]
  d <- data.frame(original_varname = original_varname, 
                  col_pos = seq.int(length(original_varname)), 
                  stringsAsFactors = F)
  
  # checkout prefix bundle, if applicable
  pb <- prefix_num_bundle(original_varname,
                          input_relo_2_end= input_relo_2_end,
                          sep = input_sep)
  
  # Create initial columns
  my_vars01 <-
    d %>%
    mutate(lower_original_varname = tolower(original_varname)) %>% # for join w/ dict. Don't want join to be case sensitive
    left_join(
      dict_dat %>%
        mutate(lower_original_varname = tolower(original_varname)) %>% 
        select(lower_original_varname, dict_varname, dict_label = label) %>%
        distinct(lower_original_varname, .keep_all = T)
      , by = "lower_original_varname"
    ) %>%
    select(-lower_original_varname) %>%
    mutate(
      num_st_ind = starts_with_number(original_varname)
      , prefix_bundle = pb$bundle
      , prefix_full_bundle = pb$full_bundle
      , use_bundle = case_when(input_relo_2_end ~ "",
                               input_relo_2_end == FALSE & prefix_bundle != "" ~ #paste0(
                                 input_sep, #prefix_bundle),
                               TRUE ~ ""
      )
      
      , viable_start = pb$viable
      
      
      , my_minlength = ifelse(input_relo_2_end == F & num_st_ind == 1,
                              input_char_len - 1, input_char_len)
      
      # apply small adjustments before transformation: trim white space, replace %'s, etc
      , adj_orig = 
          viable_start %>% 
          trimws(which = "both") %>% 
          janitor::make_clean_names(case = case_decoded,
                                    use_make_names = FALSE, # get rid of x prefix
                                    replace = replace_vec,
          )
          # If not using janitor but still want to replace stuff
          # stringr::str_replace_all(pattern = replace_vec) %>%
      
      # 1st, convert special chars to spaces, then separate any uppercase
      # words from lowercase words, then separate proper words, then
      # separates numerals from alpha chars
      , adj_parsed = adj_orig %>% read_words_nums_no_sym() %>% trimws(which = "both")
      
      , abbr_parsed = abbreviate_v(adj_parsed, minlength = my_minlength)
      
      # Before stemming, ditch special characters for spaces and is set to lowercase
      , stem = adj_parsed %>% tm::stemDocument() 
      , abbr_stem = abbreviate_v(stem, minlength = my_minlength)
      
    ) %>%
    mutate(
      sugg_varname = 
        suggest_rename(
          return_val = "sugg",
          input_char_len = input_char_len,
          original_varname = original_varname,
          dict_varname = dict_varname,
          use_bundle = use_bundle,
          adj_orig = adj_orig,
          sas_varname = sas_varname,
          stem = stem,
          abbr_stem = abbr_stem,
          abbr_parsed = abbr_parsed,
          # abbr_no_parse = abbr_no_parse
        ) %>% 
        change_case(input_changeCase) # applying desired case: upper or lower
      
    )
  
  # print(my_vars01[320,])
  
  # add a new var, keeping track of dups
  my_vars <-
    my_vars01 %>%
    left_join(
      my_vars01 %>%
        group_by(sugg_varname) %>%
        summarize(sugg_n = n()) %>%
        ungroup() 
      , by = "sugg_varname") 
  
  if(return_df == T){
    return(my_vars)
  } else {
    my_vars$sugg_varname
  }
}
