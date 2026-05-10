# Standards in Different Regulatory Agencies

## Motivation

The `xportr` package is designed to help clinical programmers create
`CDISC` compliant `xpt` files. It provides the functionality to
associate metadata information to a local R data frame, perform data set
level validation checks, and convert into a transport v5 file (`xpt`).
However, technical requirements related to the `xpt` files can change
across different regulatory agencies. This vignette aims to start to
provide a clear and concise summary of the differences between the
agencies for the `xpt` files. Further updates will come with later
package releases.

The following section will delve into various technical specifications
as per [FDA](https://www.fda.gov/media/153632/download),
[NMPA](https://www.nmpa.gov.cn/directory/web/nmpa/images/obbSqc7vwdm0ssrU0enKb7dtd29u9a4tbzUrdTyo6jK1NDQo6mhty5wZGY=.pdf),
and [PMDA](https://www.pmda.go.jp/files/000247157.pdf) guidelines.

#### File name - character

##### XPT ![](../reference/figures/xpt.png)

The first character must be an English letter (A, B, C, . . ., Z) or
underscore (\_). Subsequent characters can be letters, numeric digits
(0, 1, . . ., 9), or underscores. You can use uppercase or lowercase
letters. Blanks cannot appear in SAS names. Special characters, except
for the underscore, are not allowed.

##### FDA ![](../reference/figures/fda.jpg)

Dataset in the transport file should be named the same as the transport
file. Variable names, as well as variable and dataset labels should
include American Standard Code for Information Interchange (ASCII) text
codes only. Dataset names should contain only lowercase letters,
numbers, and must start with a letter.

##### NMPA ![](../reference/figures/nmpa.png)

The file name and the dataset name must be the same for the SDTM and
ADaM datasets. The Japanese dataset and alphanumeric dataset must be
identical in structure, except for the data lengths of the Japanese
items and the corresponding alphanumeric character sequence

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### File name - length

##### XPT ![](../reference/figures/xpt.png)

maximum length of 8 bytes

##### FDA ![](../reference/figures/fda.jpg)

8 characters

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Variable name

##### XPT ![](../reference/figures/xpt.png)

The name can contain letters of the Latin alphabet, numerals, or
underscores. The name cannot contain blanks or special characters except
for the underscore. The name must begin with a letter of the Latin
alphabet (A–Z, a–z) or the underscore.

##### FDA ![](../reference/figures/fda.jpg)

Variable names, as well as variable and dataset labels should include
American Standard Code for Information Interchange (ASCII) text codes
only. Variable names should contain only uppercase letters, numbers, and
must start with a letter

##### NMPA ![](../reference/figures/nmpa.png)

The Japanese dataset and alphanumeric dataset must be identical in
structure, except for the data lengths of the Japanese items and the
corresponding alphanumeric character sequence

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Variable length

##### XPT ![](../reference/figures/xpt.png)

8 bytes

##### FDA ![](../reference/figures/fda.jpg)

8 characters

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Label character

##### XPT ![](../reference/figures/xpt.png)

\-

##### FDA ![](../reference/figures/fda.jpg)

Variable names, as well as variable and dataset labels should include
American Standard Code for Information Interchange (ASCII) text codes
only. Do not submit study data with the following special characters in
variable and dataset labels: 1. Unbalanced apostrophe, e.g.,
“Parkinson’s” 2. Unbalanced single and double quotation marks 3.
Unbalanced parentheses, braces or brackets, e.g.,`(`, `{`and `[`

##### NMPA ![](../reference/figures/nmpa.png)

The Japanese dataset and alphanumeric dataset must be identical in
structure, except for the data lengths of the Japanese items and the
corresponding alphanumeric character sequence

##### PMDA ![](../reference/figures/pmda.png)

For eSubmission in China, one of the requirements is to translate the
foreign language data package (e.g., English) to Chinese. Variable
labels, dataset labels, MedDRA, WHO Drug terms, primary endpoint-related
code lists, etc., need to be translated from English to Chinese.

------------------------------------------------------------------------

#### Label length

##### XPT ![](../reference/figures/xpt.png)

40 bytes

##### FDA ![](../reference/figures/fda.jpg)

40 characters

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Values character

##### XPT ![](../reference/figures/xpt.png)

\-

##### FDA ![](../reference/figures/fda.jpg)

Variable values are the most broadly compatible with software and
operating systems when they are restricted to ASCII text codes
(printable values below 128). Use UTF-8 for extending character sets;
however, the use of extended mappings is not recommended. Transcoding
errors, variable length errors, and lack of software support for multi
byte UTF-8 encodings can result in incorrect character display and
variable value truncation.

##### NMPA ![](../reference/figures/nmpa.png)

If variables had been collected in Japanese and there is a risk of
losing certain information by translating it into English, the
descriptions in Japanese are necessary and appropriate, and data written
in Japanese (hereinafter referred to as Japanese data) may be submitted.
In the Japanese dataset, only the Japanese items should be Japanese and
the rest should be alphanumeric(=ASCII) data, similar to that in the
alphanumeric dataset.

##### PMDA ![](../reference/figures/pmda.png)

For eSubmission in China, one of the requirements is to translate the
foreign language data package (e.g., English) to Chinese. Variable
labels, dataset labels, MedDRA, WHO Drug terms, primary endpoint-related
code lists, etc., need to be translated from English to Chinese.

------------------------------------------------------------------------

#### Values length

##### XPT ![](../reference/figures/xpt.png)

200 bytes

##### FDA ![](../reference/figures/fda.jpg)

The allotted length for each column containing character (text) data
should be set to the maximum length of the variable used across all
datasets in the study except for supplementary qualification datasets.

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Format

##### XPT ![](../reference/figures/xpt.png)

SAS format

##### FDA ![](../reference/figures/fda.jpg)

SAS format

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.

------------------------------------------------------------------------

#### Type

##### XPT ![](../reference/figures/xpt.png)

Numeric and character

##### FDA ![](../reference/figures/fda.jpg)

\-

##### NMPA ![](../reference/figures/nmpa.png)

\-

##### PMDA ![](../reference/figures/pmda.png)

\-

------------------------------------------------------------------------

#### File size

##### XPT ![](../reference/figures/xpt.png)

\-

##### FDA ![](../reference/figures/fda.jpg)

5 GB

##### NMPA ![](../reference/figures/nmpa.png)

To be consulted if sponsors have datasets \>= 5 GB No requirement to
split datasets

##### PMDA ![](../reference/figures/pmda.png)

Information has not yet been collected.
