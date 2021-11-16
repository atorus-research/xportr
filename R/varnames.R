#' Rename data.frame variables for submission compliance
#'
#' Change the variable names in .df (only) and return that object to the user.
#' This function simply calls \code{\link{xportr_tidy_rename}} on colnames(.df).
#' That function takes a single character vector as input and converts each
#' string to be submission compliant. This means the variable must be a maximum
#' of 8 characters, ASCII only, and should contain only uppercase letters,
#' numbers and must start with a letter. No other symbols or special characters
#' should be included in these names. However, legacy studies started on or
#' before December 17, 2016, may use the underscore character "_".
#'
#' @param .df An R object with columns that can be coerced
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#' @inheritParams xportr_tidy_rename
#'
#' @details Abbreviating variable names in the `xportr` pkg uses a step-by-step
#'   process detailed below. Each original variable name will be renamed based
#'   on the method performing the least amount of work to achieve the min
#'   character length requirement (8 chars), thus maintaining maximum
#'   originality. Therefore, the function only moves on to the next method on an
#'   as-needed basis; namely, when the term is still greater than 8 characters,
#'   even after lite modification.
#'
#'   \enumerate{
#'   \item \strong{Blanks}: Any columns that are missing a variable
#'   name (i.e., the header was blank in the source file) will be renamed to 'V'
#'   plus the column position. So if the 2nd column is blank, it will receive a
#'   'V2' rename.
#'
#'   \item \strong{Use dictionary of controlled terminology}: For example:
#'   'Subject ID' may better be suited as 'SUBJID' within your organization.
#'   Note, that dictionary terms are expected to be submission compliant and
#'   will not be further abbreviated. They will, however, undergo a check for
#'   non-compliance.
#'
#'   \item \strong{Do nothing!} Or at the very least, mimic what SAS does
#'   automatically when cleaning up variable names during a PROC IMPORT. Namely,
#'   replace any special characters & capitalize everything, etc. If the
#'   'SASified' name is <= 8 chars, then the function will use that rename.
#'   However, if its still too long, the function will try removing any extra
#'   special characters or spaces to help reduce to 8 chars.
#'
#'   \item \strong{Find the STEM or ROOT word} of each original variable name.
#'   For example, if the original contains the word 'resting', the 'ing' will be
#'   dropped and only the root word 'rest' will be considered. If less than 8
#'   chars, the algorithm suggests that result. If its still too long, the
#'   function will, again, remove any special characters or spaces from the
#'   stemmed word(s).
#'
#'   \item \strong{Apply an abbreviation algorithm} who's primary goal is
#'   readability, such that the results remain unique. The methods described
#'   below are a bit more 'involved', but the results are very robust. First,
#'   you should know that characters are always stripped from the end of the
#'   strings first (i.e. from right to left). If an element of the variable name
#'   contains more than one word (words are separated by spaces) then at least
#'   one letter from each word will be retained.
#'
#'   \strong{Method}: First spaces at the ends of the string are stripped. Then
#'   (if necessary) any other spaces are stripped. Next, lower case vowels are
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
#' }
#'
#' @return Returns the modified table.
#'
#' @family var_name functions
#' @export
#'
#' @examples
#' vars <- c("", "STUDYID", "studyid", "subject id", "1c. ENT", "1b. Eyes",
#'      "1d. Lungs", "1e. Heart", "year number", "1a. Skin_Desc")
#' adxx <- data.frame(matrix(0, ncol = 10, nrow = 3))
#' colnames(adxx) <- vars
#'
#' my_dictionary <- data.frame(original_varname = "subject id", dict_varname = "subjid")
#'
#' xportr_varnames(adxx) # default
#' xportr_varnames(adxx, relo_2_end = FALSE) # prefix numbers on left-hand side
#' xportr_varnames(adxx, dict_dat  = my_dictionary) # 'SUBJID' used
#' xportr_varnames(adxx, sep = "_") # permissible for legacy studies
#' 
xportr_varnames <- function(
                      .df,
                      verbose = getOption('xportr.type_verbose', 'none'),
                      relo_2_end = TRUE,
                      letter_for_num_prefix = "x",
                      sep = "",
                      replace_vec = c("'"="",
                                      "\""="",
                                      "%"="_pct_",
                                      "#"="_nmbr_"),
                      dict_dat = data.frame(original_varname = character(),
                                            dict_varname = character())){


  tidy_names_df <- xportr_tidy_rename(
                       original_varname =  colnames(.df),
                       relo_2_end = relo_2_end,
                       letter_for_num_prefix = letter_for_num_prefix,
                       sep = sep,
                       replace_vec = replace_vec,
                       dict_dat = dict_dat,
                       char_len = 8,          # hardcoded for compliance
                       letter_case = "upper", # hardcoded for compliance
                       case = "parsed",       # hardcoded for compliance
                       return_df = TRUE,
                       verbose = verbose
                      )
  
  colnames(.df) <- tidy_names_df$renamed_var
  
  # should I output a message stating whether a variable name now differs from
  # the metacore object variable names?
  
  .df
}
