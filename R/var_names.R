#' Rename variable names into compliance
#'
#' Current assumptions: columns_meta is a data.frame with column "Variable"
#'
#' 
#' @param .df An R object with columns that can be coerced
#' @param verbose The action the function takes when a variable isn't typed
#'   properly. Options are 'stop', 'warn', 'message', and 'none'
#' @inheritParams tidy_rename
#'
#' @return Returns the modified table.
#' @export
#'
#' @examples
#' adsl <- data.frame(
#'  Subj = as.character(123, 456, 789),
#'  Different = c("a", "b", "c"),
#'  Val = c("1", "2", "3"),
#'  BIGLongVAR1 = c("var val1", "var val2", "var val3"),
#'  x = c("x1", "x2", "x3")
#' ) # no "NoteUsed"
#' colnames(adsl)[ colnames(adsl)== "x"] <- "1ofthebiggestNames"
#' .df = adsl
#' rm(adsl)
#' 
#' metacore <- data.frame(
#'   dataset = "adsl",              # fix both here and in metacore?
#'   variable = c("Subj", "Val", "NotUsed", "BIGLongVAR1") 
#' ) %>% # no "1ofthebiggestNames"
#' dplyr::union(
#'    data.frame( dataset = "advs", variable = c("carrot", "cake"))
#' )
#'
#' df2 <- xportr_var_names(.df)
xportr_var_names <- function(
                      .df,
                      # metacore,
                      # rename_metacore_vars = FALSE,
                      # domain = NULL,
                      verbose = getOption('xportr.type_verbose', 'none'),
                      ...){
  # I don't think these params are needed
  # @param metacore Either a data.frame that has the names of all possible columns
  #   and their types, or a `Metacore` object from the `Metacore` package. Required
  #   column names are dataset, variables, type
  # @param domain Name of the dataset. Ex ADAE/DM. This will be used to subset
  #   the metacore object. If none is passed it is assumed to be the name of the
  #   dataset passed in `.df`.
  # domain = "adsl"
  
  # Do we want to rename the variable names in both .df and metacore? I think
  # the answer could sometimes be "yes". So, should there be one function that
  # does it all? Well, the metacore object is something that is managed in a
  # separate workflow so maybe it would be better suited to have a function
  # available to modify in a separate workflow if needed. xportr's main focus is
  # trying to get the .df ready for export. A renaming mechanism should be
  # implemented as the last step before export. Perhaps when the final checks
  # are being made so there is no conflict with the variable's name in the
  # metacore object for other function calls. So, that would happen in
  # xpt_validate() function. Args included will need to be called somethinig
  # like `var_rename` with possible values such as "None" (the default), a named
  # vector where the name replaces the value, or "Auto" which ports tidyRename,
  # where additional options can be used from xportr_tidy_names or
  # xportr_var_rename using ... inheritsParams.
  
  # However, if the user wants to actually change the variable names BEFORE
  # then, perhaps we export a tidyRename function that users can use on their
  # own to modify their spec files or the dataset directly. For that function,
  # we'll need a arg called `rename_metacore_vars` that is logical (T/F). If
  # TRUE, then all the variables needs to be considered between both list of var
  # names (in .df and in metacore$variable if just a data.frame and something
  # else if it's an actual metacore objct (ds_spec and var_spec)) but then puts
  # them back together in the correct locations. Will need a data.frame object
  # to keep track of what goes where. That data.frame will have the
  # original_varname, df_pos, metacore_pos, and then the sugg_varname. Why do we
  # need to join both? Because the algorithm might spit out something that is
  # not unique if all variables aren't considered during the renaming process.
  # Should there also be a arg that applies to all domains? No. xportr is domain
  # specific.
  
  # Also, don't want to always change case... we need an option to leave case
  # alone.
  
  
  # # Name of the columns for working with metadata
  # domain_name <- getOption("xportr.domain_name")
  # variable_name <- getOption("xportr.variable_name")
  # 
  # if (!is.null(domain) && !is.character(domain)) {
  #   abort(c("`domain` must be a vector with type <character>.",
  #           x = glue("Instead, it has type <{typeof(domain)}>."))
  #   )
  # }
  # 
  # df_arg <- as_name(enexpr(.df)) 
  #     # what does this even do?
  #     # isn't this trying to convert a df to a string?
  # 
  # if (!is.null(attr(.df, "_xportr.df_arg_"))) {
  #   df_arg <- attr(.df, "_xportr.df_arg_")
  # } else if(identical(df_arg, ".")){
  #   attr(.df, "_xportr.df_arg_") <- get_pipe_call()
  #   df_arg <- attr(.df, "_xportr.df_arg_") 
  # }
  # 
  # domain <- domain %||% df_arg
  # 
  # if(!is.null(domain)) attr(.df, "_xportr.df_arg_") <- domain
  # 
  # ## Pull out correct metadata
  # if("Metacore" %in% class(metacore)) {
  #   # metacore_ds <- metacore$ds_vars # probably not needed. Var spec variable list should match that found in ds_vars
  #   metacore <- metacore$var_spec # ac ammended
  # }
  # 
  # # filter metacore object to just the domain specified
  # if(domain_name %in% names(metacore)){
  #   metacore <- metacore %>%
  #     filter(!!sym(domain_name) == domain)
  # }
  # metacore <- metacore %>%
  #   select(!!sym(variable_name)) 
    
  
  
  # if(rename_metacore_vars){
  #   
  #   # hmmm... to do return a metacore object? Nothing in this package
  #   # is designed that way
  #   all_vars <- data.frame(
  #     variable = colnames(.df),
  #     df_pos = 1:ncol(.df)
  #   ) %>%
  #   full_join(
  #     metacore %>% mutate(mc_pos = 1:nrow(metacore))
  #   ) %>%
  #   mutate(variable = tidy_rename(variable))
  #   
  # } else { # just change the variable names in the .df only
    colnames(.df) <- tidy_rename(original_varname =  colnames(.df), ...)
  # }
  
  .df
  
  # should I output a message stating whether a variable name now differs
  # from the metacore object variable names?
}
