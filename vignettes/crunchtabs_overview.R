## ---- eval = FALSE-------------------------------------------------------
#  toplines_summary <- crosstabs(dataset = ds, weight = 'weight')

## ---- eval = FALSE-------------------------------------------------------
#  writeLatex(toplines_summary, filename = filename)
#  writeExcel(toplines_summary, filename = filename)

## ---- eval = FALSE-------------------------------------------------------
#  banner_data <- banner(ds, vars = list(`banner 1` = c('gender', 'age4')))

## ---- eval = FALSE-------------------------------------------------------
#  crosstabs_summary <- crosstabs(ds, banner = banner_data)

## ---- eval = FALSE-------------------------------------------------------
#  writeLatex(crosstabs_summary, filename = filename)
#  writeExcel(crosstabs_summary, filename = filename)

## ---- eval = FALSE-------------------------------------------------------
#  cb_summary <- crosstabs(ds, codebook = TRUE)

## ---- eval = FALSE-------------------------------------------------------
#  writeCodebook(cb_summary, filename = filename)

