## crunchtabs 1.4.2

- Updates to vignettes
- INNOV-326 writeCodeBookLatex should not open by default. Add function argument open = FALSE. This avoids a situation in non-interactive sessions where the process would otherwise be stuck as it attempts to open a GTK program from a bash session. 
- Small change in how appendices are automatically identified. Previous iterations prepared long tables with the longtabu package. However, this tex package overwrote numeric presentation options from siunitx. This has been changed to longtable, which respects siunitx. Now appendix long tables are identified by the length of lines in their tex (20 or more).

## crunchtabs 1.4.1

- Multiple adjustments for tracking report edge cases
- In a tracking report, if an alias appears only once we now specify that by appending the wave label to the question's note.
- In a tracking report, we now have the option to show a variable only once even if it appears in multiple datasets.

Fixes:

- In some cases there are duplicated row names in different positions when we use cbindFill, we did not account for this possibility.
- When creating a tracking report for a multiple response question, it's possible that one or more of the responses is not included in one or more of the waves, we have added a tryCatch to accomodate this possibility.
- themeNew documentation was missing a closing } that silently broke the display of documentation via ?themeNew.
- When converting MR variables we rename subVars using numbers. If a researcher included an alias with the same number they would collide. Now we assign random strings instead of sequential numbers to the subVars.


## crunchtabs 1.4.0

Features:

- Relabeling for items, options, descriptions and notes via relabel #247. Allows for arbitrary question text adjustment #138 #55
- Tracking reports #232
- Recontact reports #232
- Now get a warning when you attempt to make a codebook from a weighted dataset #226
- Major updates to vignettes for tracking/recontacts #218
- Option to show only the first appearance of an alias in a tracking report as a single categorical #241
- Added ability to generate a codebook from a generic dataset using generated metadata #227
- Added ability to generate codebooks from large arrow datasets #227
- Automatically align variables, by adding "-" where category has no responses #245, #243

Fixes:

- Fix for absolutelynopagebreak duplication on wrapping tex that is a character vector instead of simple string #246
- Fix bogus warnings for format_label_column_exceptions #245
- Fix missing images from vignettes #242
- Added linting #228
- Removing legacy features from the repository #225

## crunchtabs 1.3.1

- Allows variable pass through in tracking reports such that questions that appear in one or more waves (but less than all waves) are displayed appropriately and in the order specified. 

## crunchtabs 1.3.0

- Fixes problems with enforce_onehundred (#195)
- Adds option to remove page numbers from toplines/crosstabs (#200)
- Add toplines() function as an alias to crosstabs() (#201)
- Add functionality for flipping grids and presenting recontact questions (#103 )
- Fixes two new issues with codebooks where kableExtra added breaking elements and an issue with the basename of a dataaset containing special characters (#204, #205 )
- Adds support for custom weights (#209)
- Adds support for recontact_toplines (#199)
- Adds functionality for flipping grids on an exceptional basis (#212)
- Fixes a bug with include_q_number = FALSE (#207)
- Vignettes for recontact toplines, custom weights and other (#218)
- Enforces unweighted dataset for codebook creation (#226)
- Replaces with_mock with mockery (#224)

## crunchtabs 1.2.9

- Codebook question descriptions now appropriately escape special characters (#187)
- Added option enforce_onehundred which allows one to avoid rounding errors in totals rows (#189)
- Codebook table of contents overruns, cutting text and adding "..." (#186)
- Codebook generation now supports a filepath (#185)
- Added vertical space before append_text (#182)
- Bugfix: append_text that is multiple lines du0lpicated vertical space. Collapsing. (#191)
- Remove requirement for dev version of kableExtra (#184)

## crunchtabs 1.2.8

- Documentation for generating codebooks (#180)
- Tests and fixes for broken tests (#176)
- Spacing and font type for codebooks (#178)
- Additional refinements for codebooks such as title, subtitle, sample description (#152)
- Fixes for warnings in existing code (#158)
- Bugfix for logo assignment in excel (#155)
- Added pagebreak_in_banner which stops multi-banner crosstabs from being spread across pages using "to be continued" (#161, #169)
- Major rework of codebooks (#165, #172, #170, #160, #159)

## crunchtabs 1.2.7

- Initial support for latex codebooks (#122 and #152)
- Adjustments for categorical array presentation
- Fixes for problems related to logos (#155)

## crunchtabs 1.2.6
 
- tinytex enforcement does not respect logging = FALSE (#132)
- sorting for categoricals / multiple response in toplines (#104)
- sorting for crosstabs objects (#143)
- updated FAQ with sorting examples (#104)

## crunchtabs 1.2.5

* Add summarization for continuous variables (NumericVariable) in toplines (#80)
* Add summarization for date time variable (DatetimeVariable) in toplines (#120)
* Add summarization for text data (TextVariable) in toplines (#121)
* Add amsmath package to latex preamble (#123)
* Bugfix for open=TRUE, PDF doc now opens appropriately (#125)
* Update FAQ: question alias numbering example (#128)
* Update FAQ: customizing stub widths (#118)
* Update FAQ: specifying logos in excel (#115)
* Update FAQ: how to weight by question (#106)
* Internal proposal for new summarizations and codebook (#117)
* Fix codecov checks (#111)

## crunchtabs 1.2.3

* New introductory and FAQ vignettes (#96)
* tableHeader.CrossTabVar respects global stub width setting (#93)
* Updated vignette (#65)
* Adding more tests (#84)
* Warn users of missing logo in pdf (#88)
* Fix warnings (#47, unmerged!)
* Adding pkgdown (#96)

## crunchtabs 1.2.2

* Make categorical arrays in toplines smarter (#67)
* Add ability to specify column width of stub in crosstab and categorical array via format_label_column_exceptions (#77)
* Adding significant documentation (#66)
* Adding more tests (#11)
* Updates to CI and development setup (#68)
* Added CI for internal examples

## crunchtabs 1.2.1

* Added new theme settings: latex_max_lines_for_tabular and latex_round_percentages_exception options.

## crunchtabs 1.2.0

* Fix .tex/.pdf reports when variable names (table headers) contain newline characters (#36)
* Fix .tex/.pdf topline reports containing categorical array variables with subtotals
* Fix .tex/.pdf reports containing non-uniform base multiple-response variables

## crunchtabs 1.1.0

* Assorted bug fixes

## crunchtabs 1.0.0

* Introduces "themes" for customizing report style.

## crunchtabs 0.1.0

* Initial addition of functions and tests
