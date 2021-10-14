library(pdftools)
library(fs)

pdfs <- dir_ls('paper/figures', glob = '*.pdf')
sapply(pdfs, \(x) pdf_convert(pdf = x,
                              filenames = path('paper/figures/png', path_ext_remove(path_file(x)), ext = 'png'),
                              dpi = 300))
