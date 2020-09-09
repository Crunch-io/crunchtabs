## crunchtabs 1.2.9

- Codebook question descriptions now appropriately escape special characters
- Added option enforce_onehundred which allows one to avoid rounding errors in totals rows
- Codebook table of contents overruns, cutting text and adding "..."
- Codebook generation now supports a filepath
- Added vertical space before append_text
- Bugfix: append_text that is multiple lines du0lpicated vertical space. Collapsing.

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
