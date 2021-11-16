#' Vectorized Abbreviation
#'
#' Makes the `abbreviate` function (from base R) vectorized to
#' accommodate a vector of differing minLength values. Cannot use
#' `Vectorize` here, as it will lead to dupes. This method
#' generates the abbreviations with the largest minLengths first and leaves
#' those intact as it continues onto the terms with the next highest minLength
#' values. This continues iteratively until the lowest minlength is reached.
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
  read_words_and_nums(replace_sym(x)) # with no pipes
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
#' A helper function used to simplify the extraction of certain prefix bundles, used in
#' \code\link{gather_n_move_prefix_num_bundle}}.
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param num a number, as numeric or character, that matches the first
#'   occurrence of a number in the prefix bundle. Determined automatically in
#'   `gather_n_move_prefix_num_bundle()`
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
#' \code{\link{read_words}} function to parse out 'bundle' alphas from
#' 'non-bundle' alphas found in the term. Returns the "bundle prefix" with no
#' underscore prefix.
#'
#' @param x a character vector where matches are sought, or an object which can
#'   be coerced by as.character to a character vector. Long vectors are
#'   supported.
#' @param relo_2_end logical, if `TRUE` then the prefix bundle, if it exists
#'   will be scooted to the end of the string x, else it will receive the
#'   necessary "_" prefix
#' @param sep string of only one character long, intended to separate words from
#'   one another, not a starting character if original term has a numeric prefix!
#'
#' @details Currently, this function accounts for the following types of prefix
#'   bundles, where 3 is used as an example starting digit:
#'   * x = "3a. hey" will return "3a"
#'   * x = "3_17a_hey" will return "3_17a"
#'   * x = "3_17_hey" will return "3_17"
#'   * x = "3_17hey" will return "3_17"
#'   * x = "3hey" will return "3"
#'   
#' @noRd
#' @export
#' @examples
#' gather_n_move_prefix_num_bundle(x = "iAteABunch_of_grapesUntil99%Full")
#' gather_n_move_prefix_num_bundle(x = "1a. How Was Brunch?")
#' gather_n_move_prefix_num_bundle(x = "2aa How Was Brunch2day?")
#' gather_n_move_prefix_num_bundle(x = "30abHow Was Brunch2day?")
#' gather_n_move_prefix_num_bundle(x = c("30ab.How Was Brunch2day?", "40AHow Was Brunch2day?"))
#' gather_n_move_prefix_num_bundle(x = c("iAteABunch_of_grapesUntil99%Full","30ab.How Was Brunch2day?", "40AHow Was Brunch2day?"))
gather_n_move_prefix_num_bundle <- function(x, relo_2_end = T, sep = "_"){
  
  full_bundle <- purrr::map_chr(x, function(x){
    pfix_st_num <- prefix_num(x) # grab prefix num
    if(pfix_st_num != ""){ # if it exists...
      fb <- case_when(
        # Example x = "3a. hey"
        !(extrct_vssl(x, pfix_st_num, "[[:alpha:]]{1,2}[^[:alnum:]]") %>% is.na()) ~
          extrct_vssl(x, pfix_st_num, "[[:alpha:]]{1,2}[^[:alnum:]]"),
        
        # Example x = "3_17a_hey"
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
    } else { # if prefix num doesn't exist...
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
    if(relo_2_end){
      trimws(ifelse(fb == "", x, paste0(stringr::str_replace(x, fb, ""), sep,
                                        stringr::str_replace(fb, "([^[:alnum:]])",""))),
             which = "both")
    } else { x }
  })
  return(list(viable = fixed, bundle = bundle, full_bundle = fb_clean))
}



#' Change letter case - upper, lower, or leave as-is
#'
#' Supply a character string or vector, returns the same vector with changed
#' case. Notice, this function just uses `toupper()` or `tolower()` but the
#' string, `x`, may have been altered by the janitor package's use of
#' \code{\link[snakecase]{to_any_case}} later on, when suggesting a rename
#'
#' @param x a character vector, or an object which can be coerced by
#'   as.character to a character vector. Long vectors are supported.
#' @param letter_case character string, either "lower", "upper", or "asis"
#'
#' @noRd
#' @export
#' @examples
#' chg_letter_case(c("hello darkness","My Old FRIEND"), "lower" )
#' chg_letter_case(c("hello darkness","My Old FRIEND"), "upper" )
#' chg_letter_case(c("hello darkness","My Old FRIEND"), "asis" )
chg_letter_case <- function(x, letter_case = "asis"){
  if(tolower(letter_case) == "upper")  {toupper(x)}
  else if(tolower(letter_case) == "lower") {tolower(x)}
  else {x} # leave 'else {x}', so  janitor can edit the case (via snakecase::to_any_case)
}


#' Suggest a new renamed / abbreviated term
#' 
#' Take multiple character vectors as inputs and return the final suggestion or the 
#'   transfermation method used.
#'
#' @param char_len the maximum char length the final suggestion can be
#' @param original_varname a character vector
#' @param dict_varname a character vector to use first if a dictionary term
#'   exists
#' @param adj_orig the original name, adjusted for any percent symbols (%) or
#'   grapes
#' @param stem the stem or root word(s), char
#' @param abbr_stem the character abbreviation of the stem or root
#' @param abbr_parsed the character abbreviation of the adj_orig
#'  
#' @noRd
#' @examples
#' least_pushy_rename_method(
#'   char_len = 8,
#'   original_varname = c("", "subject id", "1c. ENT", "1b. Eyes", "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc"),
#'   dict_varname = c(NA, "SUBJID", NA, NA, NA, NA, NA, NA),
#'   use_bundle = c("","","","","","","",""),
#'   adj_orig = c("", "subject id", "1c. ENT", "1b. Eyes", "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc"),
#'   stem = c("", "subject id", "c ent", "b eye", "d lung", "e heart", "year number", "a skin desc"),
#'   abbrev = c("", "subjctid", "_1c. ENT", "_1b.Eyes", "_1d lung", "_1eheart", "yearnmbr", "_1asknds")
#'   abbr_transf = c("","","","","","","")
#'   )
least_pushy_rename_method <- function(char_len,
                           relo_2_end = TRUE,
                           sep = '_',
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
    !(is.na(dict_varname)) ~ dict_varname, 

    # Minor cleaning
    nchar(paste0(use_bundle, replace_sym(adj_orig, sep))) <= char_len ~
      paste0(use_bundle, replace_sym(adj_orig, sep)),

    nchar(paste0(use_bundle, replace_sym(adj_orig, ""))) <= char_len ~
      paste0(use_bundle, replace_sym(adj_orig, "")),

    # Stemming 
    nchar(paste0(use_bundle, replace_sym(stem, sep))) <= char_len ~
      paste0(use_bundle, replace_sym(stem, sep)),

    nchar(paste0(use_bundle, replace_sym(stem, ""))) <= char_len ~
      paste0(use_bundle, replace_sym(stem, "")),

    # Abbreviation
    TRUE ~ 
      case_when(
        nchar(paste0(use_bundle, abbr_parsed)) <= char_len & 
          nchar(abbr_parsed) >= nchar(abbr_stem) ~ paste0(use_bundle, abbr_parsed)
        
        , nchar(paste0(use_bundle, abbr_stem)) <= char_len ~ paste0(use_bundle, abbr_stem)
        
        , TRUE ~ "NoAbbrev"
      )
  )
}


#' Rename terms for submission compliance from vector
#'
#' The function takes a single character vector as input and converts each
#' string to be submission compliant. This means the variable must be a maximum
#' of 8 characters, ASCII only, and should contain only uppercase letters,
#' numbers and must start with a letter. No other symbols or special characters
#' should be included in these names. However, legacy studies started on or
#' before December 17, 2016, may use the underscore character "_". This function
#' is slightly more flexible than the submission criteria would allow, so use
#' the arguments wisely. \code{\link{xportr_varnames}} performs the same logic,
#' but directly renames the columns of a data.frame plus enforces more strict
#' adherence to the regulatory guidelines mentioned above.
#'
#' @param original_varname a character vector needing renaming
#' @param char_len integer, the max number of characters allowed in the renamed
#'   strings
#' @param relo_2_end logical, if TRUE: numerical prefix bundles will be
#'   relocated to the end of the string. A prefix bundle is determined as a
#'   number or "grouping of numbers separated by special characters and other
#'   punctuation". See details section for more info on prefix bundles.
#' @param letter_for_num_prefix character. Will be ignored if `relo_2_end =
#'   TRUE`. Per CDISC & regulatory body requirements, variable names cannot
#'   start with a number, so if you want to leave the number of the left-hand
#'   side, use this argument to insert a starting letter before the numeric
#'   prefix.
#' @param sep string of only one character long, intended to separate words from
#'   one another. In general, only "" and "_" are possible, but the user can
#'   insert a letter if desired. Note that "_" is discouraged because it is only
#'   permissible for legacy studies started on or before Dec 17, 2016.
#' @param replace_vec A named character vector where the name is replaced by the
#'   value.
#' @param dict_dat a data frame containing two variables: the `original_varname`
#'   and the `dict_varname` to find and replace
#' @param letter_case character, with choices c("upper", "lower", "asis")
#'   allowing user to make the final renamed terms uppercase (the default),
#'   lowercase, or leave them as-is, respectively. Note, lowercase is discourage
#'   as it is not submission compliant.
#' @param case character, see \code{\link[snakecase]{to_any_case}} for more info
#'   on alternate cases but some popular choices are "snake", "lower_camel", or
#'   "parsed" (the default). From the documentation, the "parsed" case parses
#'   out substrings and surrounds them with an underscore. Underscores at the
#'   start and end are trimmed. No lower or upper case pattern from the input
#'   string are changed.
#' @param return_df logical, defaults to TRUE where entire dataset is returned
#'   from suggestion process, else just the suggestion column itself
#'
#' @details Abbreviating variable names in the `xportr` pkg uses a step-by-step
#'   process detailed below. Each original variable name will be renamed based
#'   on the method performing the least amount of work to achieve the min
#'   character length requirement (8 chars), thus maintaining maximum
#'   originality. Therefore, the function only moves on to the next method on an
#'   as-needed basis; namely, when the term is still greater than 8 characters,
#'   even after lite modification.
#'
#'   (1) Blanks: Any columns that are missing a variable name (i.e., the header
#'   was blank in the source file) will be renamed to 'V' + the column position.
#'   So if the 2nd column is blank, it will receive a 'V2' rename.
#'
#'   (2) Use dictionary of controlled terminology: For example: 'Subject ID' may
#'   better be suited as 'SUBJID' within your organization. Note, that
#'   dictionary terms are expected to be submission compliant and will not be
#'   further abbreviated. They will, however, undergo a check for
#'   non-compliance.
#'
#'   (3) Do nothing! Or at the very least, mimic what SAS does automatically
#'   when cleaning up variable names during a PROC IMPORT. Namely, replace any
#'   special characters with underscores ('_'), capitalize everything, and if
#'   the value starts with a digit, add the '_' prefix. If the 'SASified' name
#'   is <= 8 chars, then the function will use that rename. However, if its
#'   still too long, the function will try removing any extra special characters
#'   or spaces to help reduce to 8 chars.
#'
#'   (4) Find the STEM or ROOT word of each original variable name. For example,
#'   if the original contains the word 'resting', the 'ing' will be dropped and
#'   only the root word 'rest' will be considered. If less than 8 chars, the
#'   algorithm suggests that result. If its still too long, the function will,
#'   again, remove any special characters or spaces from the stemmed word(s).
#'
#'   (5) Apply an abbreviation algorithm who's primary goal is readability, such
#'   that the results remain unique. The methods described below are a bit more
#'   'involved', but the results are very robust. First, you should know that
#'   characters are always stripped from the end of the strings first (i.e. from
#'   right to left). If an element of the variable name contains more than one
#'   word (words are separated by spaces) then at least one letter from each
#'   word will be retained.
#'
#'   Method: First spaces at the ends of the string are stripped. Then (if
#'   necessary) any other spaces are stripped. Next, lower case vowels are
#'   removed followed by lower case consonants. Finally if the abbreviation is
#'   still longer than 8 chars, upper case letters and symbols are stripped.
#'
#'   When identifying 'words', the app performs some pre-processing steps in the
#'   following order:
#'
#'   * Certain symbols are replaced. Like the '%' symbol is replaced with 'PCT'.
#'   Ex: 'I_Ate_%' becomes 'I_Ate_PCT'
#'
#'   * Replace any symbols with a blank space. Ex: 'I_Ate' becomes 'I Ate'
#'
#'   * Find when there are two capital letters next to each other, followed by a
#'   lower case letter then adds a space between the two capital letters to
#'   separate the assumed 'words'. Ex: 'IAte' becomes 'I Ate'
#'
#'   * Insert a space between a number followed by a character. Ex: iAte1meal'
#'   becomes 'iAte1 meal'
#'
#'   * Insert a space between a character followed by a number. Ex: 'iAte1meal'
#'   becomes 'iAte 1meal'
#'
#'   What do we abbreviate when? If a stemmed word exists, the app will apply
#'   the abbreviation algorithm described above on the stemmed version of the
#'   variable name, else the original variable name.
#'
#'   Since submission guidelines indicate variables may not start with a number,
#'   when found, the algorithm will either add and maintain a '_' "prefix
#'   bundle" through any transformations detailed above, or it will relocate the
#'   "prefix bundle" to the end of the term (the default behavior). What if a
#'   term starts with non-standard numerical prefix? Currently, the function
#'   accounts for the following types of prefix bundles, where 3 is used as an
#'   example starting digit:
#'
#'   * x = "3a. hey" will return "3a"
#'
#'   * x = "3_17a_hey" will return "3_17a"
#'
#'   * x = "3_17_hey" will return "3_17"
#'
#'   * x = "3_17hey" will return "3_17"
#'
#'   * x = "3hey" will return "3"
#'
#' @family var_name functions
#' @export
#' @examples
#' vars <- c("", "studyid", "STUDYID", "subject id", "1c. ENT", "1b. Eyes",
#'          "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc")
#'
#' # Default behavior
#' xportr_tidy_rename(vars)
#' xportr_tidy_rename(vars, letter_case = "asis")
#'
#' # Leave numerical prefix on left-hand side, but add a starting letter
#' xportr_tidy_rename(vars, relo_2_end = FALSE, letter_for_num_prefix = "x")
#'
#' # Add a dictionary and remove underscores
#' xportr_tidy_rename(vars, sep = "",
#'  dict_dat = data.frame(original_varname = "subject id", dict_varname = "subjid"))
#'  
xportr_tidy_rename <- function(
      original_varname,
      char_len = 8,
      relo_2_end = TRUE,
      letter_for_num_prefix = "x",
      sep = '',
      replace_vec = c("'"="",
                      "\""="",
                      "%"="_pct_",
                      "#"="_nmbr_"),
      dict_dat = data.frame(original_varname = character(),
                            dict_varname = character()),
      letter_case = "upper",
      case = "parsed",
      return_df = FALSE,
      verbose = getOption('xportr.type_verbose', 'none')
){
  # Check to make sure letter_for_num_prefix is actually a letter
  st_letter <- substr(letter_for_num_prefix, 1, 1)
  if(relo_2_end == FALSE & !stringr::str_detect(st_letter, '[A-Za-z]')) {
    stop(paste0("Argument 'letter_for_num_prefix' must start with a letter, not '",
                st_letter, "'", ifelse(st_letter == '', " (an empty string)", "")))
  }
  # The larger the letter_for_num_prefix gets, the less space there is for the
  # rest of the term, so we'll put a hard stop with a max of 3 characters
  if(relo_2_end == FALSE & nchar(letter_for_num_prefix) > 3) {
    stop(paste0("Argument 'letter_for_num_prefix' cannot be longer than 3 characters. '",
                letter_for_num_prefix, "' has ", nchar(letter_for_num_prefix),
                " chars; please adjust."))
  }
  
  # Check the strings for compliance before performing any renaming
  init_checks <- xpt_validate_var_names(original_varname, list_vars_first = FALSE)
  if (length(init_checks) > 0) {
    message(c("\nThe following variable name validation checks failed:\n",
              paste("*", init_checks, collapse = "\n")))
  } else {
  message("\nVariable Name Validation passed! No renaming necessary.\n")
  }
  
  
  # initialize data.frame to track all moving parts when suggesting new var names
  d <- data.frame(original_varname = original_varname, 
                  col_pos = seq.int(length(original_varname)), 
                  stringsAsFactors = F)
  
  # parse prefix bundle information (if applicable)
  pb <- gather_n_move_prefix_num_bundle(original_varname,
                                        relo_2_end = relo_2_end,
                                        sep = sep)
  
  # Borrowed from janitor, but adjusted to handle non-case sensitive duplicated
  # names since they can introduce dups because we don't adjust letter case
  # until after the algorithm has been executed... so here we are proactively
  # searching for potential dups that may exist after we change case to upper
  # (for ex) by making the strings unique in the same letter case before feeding
  # to the "renaming engine". Example edge case: a variable called "studyid" and
  # 2nd one called "STUDYID" would result in a dups without the following code:
  viable <- pb$viable
  case_viable <- viable %>% chg_letter_case(letter_case)
  if(tolower(letter_case) %in% c("upper","lower") &
                  any(duplicated(case_viable)) ) {
    dupe_count <-
      vapply(
        seq_along(case_viable), function(i) {
          sum(case_viable[i] == case_viable[1:i])
        },
        1L)
    
    viable[dupe_count > 1] <-
      paste(
        viable[dupe_count > 1],
        dupe_count[dupe_count > 1],
        sep = "_"
      )
  }
  
  # Generate all the rename methods needed to perform the least intrusive rename
  # method for each term
  my_vars01 <-
    d %>%
    mutate(lower_original_varname = tolower(original_varname)) %>% # for join w/ dict. Don't want join to be case sensitive
    left_join(
      dict_dat %>%
        mutate(lower_original_varname = tolower(original_varname)) %>% 
        select(lower_original_varname, dict_varname) %>%
        distinct(lower_original_varname, .keep_all = T)
      , by = "lower_original_varname"
    ) %>%
    select(-lower_original_varname) %>%
    mutate(
        num_st_ind = starts_with_number(original_varname)
      , prefix_bundle = pb$bundle
      , use_bundle = case_when(relo_2_end ~ "",
                               relo_2_end == FALSE & prefix_bundle != "" ~ letter_for_num_prefix, 
                               TRUE ~ "")
      , viable_start = viable
      , my_minlength = ifelse(relo_2_end == F & num_st_ind == 1,
                              char_len - nchar(letter_for_num_prefix),
                              char_len)
      # apply small adjustments before transformation: trim white space, replace
      # %'s, use janitor (does a ton of work, like removing non-ASCII), etc
      , adj_orig = 
          viable_start %>% 
          trimws(which = "both") %>%
          janitor::make_clean_names(case = case,
                                    use_make_names = FALSE, # get rid of x prefix
                                    replace = replace_vec,
                                    sep_out = sep
          )
          # If we want to pivot away from using janitor but still want to
          # replace stuff and translate to ASCII, use this code:
          # stringr::str_replace_all(pattern = replace_vec) %>%
          # stringi::stri_trans_general(id="Any-Latin;Greek-Latin;Latin-ASCII")
      
      # 1st, convert special chars to spaces, then parse any uppercase words
      # from lowercase words, then parse proper words, then parse numerals from
      # alpha chars
      , adj_parsed = adj_orig %>% read_words_nums_no_sym() %>% trimws(which = "both")
      
      , abbr_parsed = abbreviate_v(adj_parsed, minlength = my_minlength)
      
      # Before stemming, ditch special characters for spaces and is set to
      # lowercase per the tm pkg
      , stem = adj_parsed %>% tm::stemDocument() 
      , abbr_stem = abbreviate_v(stem, minlength = my_minlength)
      
    ) %>%
    mutate(
      renamed_var = 
        least_pushy_rename_method(
          char_len = char_len,
          original_varname = original_varname,
          dict_varname = dict_varname,
          use_bundle = use_bundle,
          adj_orig = adj_orig,
          stem = stem,
          abbr_stem = abbr_stem,
          abbr_parsed = abbr_parsed
        ) %>% 
        chg_letter_case(letter_case) # upper, lower, or asis
      
    )
  
  # add a new var, keeping track of dups
  my_vars <-
    my_vars01 %>%
    left_join(
      my_vars01 %>%
        group_by(renamed_var) %>%
        summarize(renamed_n = n()) %>%
        ungroup() 
      , by = "renamed_var") 
  
  
  # These are just a few other "nice to know" checks/msgs regarding the var renaming
  # See messages.R for details
  var_names_log(my_vars, verbose)
  
  # Perform official xpt checks now that strings have been renamed
  final_checks <- xpt_validate_var_names(my_vars$renamed_var, list_vars_first = FALSE)
  if (length(final_checks) > 0) {
    message(c("\nThe following variable name validation checks still failed:\n",
              paste0(paste(final_checks, collapse = "\n"), "\n")))
  } else if(length(init_checks) > 0) {
    message("\nAll renamed variables passed validation.\n\n")
  }
  
  
  if(return_df == T){
    return(my_vars)
  } else {
    my_vars$renamed_var
  }
}
