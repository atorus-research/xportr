### Thank you for your Pull Request! 

We have developed a Pull Request template to aid you and our reviewers.  Completing the below tasks helps to ensure our reviewers can maximize their time on your code as well as making sure the xportr codebase remains robust and consistent.  

### The scope of `{xportr}`

`{xportr}`'s scope is to enable R users to write out submission compliant `xpt` files that can be delivered to a Health Authority or to downstream validation software programs.  We see labels, lengths, types, ordering and formats from a dataset specification object (SDTM and ADaM) as being our primary focus. We also see messaging and warnings to users around applying information from the specification file as a primary focus. Please make sure your Pull Request meets this **scope of {xportr}**. If your Pull Request moves beyond this scope, please get in touch with the `{xportr}` team on [slack](https://pharmaverse.slack.com/archives/C030EB2M4GM) or create an issue to discuss.

Please check off each task box as an acknowledgment that you completed the task. This checklist is part of the Github Action workflows and the Pull Request will not be merged into the `devel` branch until you have checked off each task.

### Changes Description

_(descriptions of changes)_ 

### Task List

- [ ] The spirit of xportr is met in your Pull Request
- [ ] Place Closes #<insert_issue_number> into the beginning of your Pull Request Title (Use Edit button in top-right if you need to update)
- [ ] Summary of changes filled out in the above Changes Description.  Can be removed or left blank if changes are minor/self-explanatory.
- [ ] Code is formatted according to the [tidyverse style guide](https://style.tidyverse.org/).  Use `styler` package and functions to style files accordingly.
- [ ] Updated relevant unit tests or have written new unit tests. See our [Wiki](https://github.com/atorus-research/xportr/wiki/Style-Guide-for-Unit-Tests) for conventions used in this package.
- [ ] Creation/updated relevant roxygen headers and examples. See our [Wiki](https://github.com/atorus-research/xportr/wiki/Style-Guide-for-Roxygen-Headers) for conventions used in this package.
- [ ] Run `devtools::document()` so all `.Rd` files in the `man` folder and the `NAMESPACE` file in the project root are updated appropriately
- [ ] Run `pkgdown::build_site()` and check that all affected examples are displayed correctly and that all new/updated functions occur on the "Reference" page.
- [ ] Update NEWS.md if the changes pertain to a user-facing function (i.e. it has an @export tag) or documentation aimed at users (rather than developers)
- [ ] Address any updates needed for vignettes and/or templates
- [ ] Link the issue Development Panel so that it closes after successful merging.
- [ ] Fix merge conflicts 
- [ ] Pat yourself on the back for a job well done!  Much love to your accomplishment!
