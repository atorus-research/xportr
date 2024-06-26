% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/label.R
\name{xportr_label}
\alias{xportr_label}
\title{Assign Variable Label}
\usage{
xportr_label(
  .df,
  metadata = NULL,
  domain = NULL,
  verbose = NULL,
  metacore = deprecated()
)
}
\arguments{
\item{.df}{A data frame of CDISC standard.}

\item{metadata}{A data frame containing variable level metadata. See
'Metadata' section for details.}

\item{domain}{Appropriate CDISC dataset name, e.g. ADAE, DM. Used to subset
the metadata object.}

\item{verbose}{The action this function takes when an action is taken on the
dataset or function validation finds an issue. See 'Messaging' section for
details. Options are 'stop', 'warn', 'message', and 'none'}

\item{metacore}{\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}} Previously used to pass
metadata now renamed with \code{metadata}}
}
\value{
Data frame with label attributes for each variable.
}
\description{
Assigns variable label from a variable level metadata to a given data frame.
This function will give detect if a label is greater than
40 characters which isn't allowed in XPT v5. If labels aren't present for the
variable it will be assigned an empty character value. Labels are stored in
the 'label' attribute of the column.
}
\section{Messaging}{
 \code{label_log()} is the primary messaging tool for
\code{xportr_label()}. If there are any columns present in the '.df' that are not
noted in the metadata, they cannot be assigned a label and a message will
be generated noting the number or variables that have not been assigned a
label.

If variables were not found in the metadata and the value passed to the
'verbose' argument is 'stop', 'warn', or 'message', a message will be
generated detailing the variables that were missing in metadata.
}

\section{Metadata}{
 The argument passed in the 'metadata' argument can either
be a metacore object, or a data.frame containing the data listed below. If
metacore is used, no changes to options are required.

For data.frame 'metadata' arguments three columns must be present:
\enumerate{
\item Domain Name - passed as the 'xportr.domain_name' option. Default:
"dataset". This is the column subset by the 'domain' argument in the
function.
\item Variable Name - passed as the 'xportr.variable_name' option.
Default: "variable". This is used to match columns in '.df' argument and
the metadata.
\item Variable Label - passed as the 'xportr.label' option.
Default: "label". These character values to update the 'label' attribute of
the column. This is passed to \code{haven::write} to note the label.
}
}

\examples{
adsl <- data.frame(
  USUBJID = c(1001, 1002, 1003),
  SITEID = c(001, 002, 003),
  AGE = c(63, 35, 27),
  SEX = c("M", "F", "M")
)

metadata <- data.frame(
  dataset = "adsl",
  variable = c("USUBJID", "SITEID", "AGE", "SEX"),
  label = c("Unique Subject Identifier", "Study Site Identifier", "Age", "Sex")
)

adsl <- xportr_label(adsl, metadata, domain = "adsl")
}
