# Release Description
<!--- Summarize what is being released.  -->

## Milestone
<!--- Link to the milestone for the release. ---> 
<!--- Make sure all relevant issues are included on the linked pages. --->
Milestone: 

# Release Checklist
<!--- Fill out the following Release checklist -->

- [ ] DESCRIPTION File version number has been updated
- [ ] DESCRIPTION file updated with New Developers (if applicable)
- [ ] NEWS.md has been updated and issues numbers linked
- [ ] README.md has been updated (if applicable)
- [ ] Vignettes have been updated (if applicable)
- [ ] Ensure all unit tests are passing
- [ ] Review https://r-pkgs.org/release.html for additional checks and guidance
- [ ] Use `rhub::check_for_cran()` for checking CRAN flavors before submission
- [ ] Use `usethis::use_revdep()` to check for any reverse dependencies
- [ ] GitHub actions on this PR are all passing
- [ ] Draft GitHub release created using automatic template and updated with additional details. Remember to click "release" after PR is merged.