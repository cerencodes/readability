# *****************************************************************************
# COMBINE APPENDIX DOCX FILES ----
# *****************************************************************************
library(officer)

project_dir <- if (dir.exists("results")) "." else if (dir.exists(file.path("..", "results"))) ".." else "."
appendix_dir <- file.path(project_dir, "results", "appendices")
combined_file <- file.path(appendix_dir, "All_Appendices_Combined.docx")

appendix_files <- list.files(
  appendix_dir,
  pattern = "^Appendix_.*\\.docx$",
  full.names = TRUE
)

appendix_files <- sort(appendix_files)

if (length(appendix_files) == 0) {
  stop("No Appendix_*.docx files found in: ", appendix_dir)
}

doc <- read_docx()
doc <- body_add_par(doc, "Combined Appendices", style = "heading 1")
doc <- body_add_par(
  doc,
  paste("Generated from", length(appendix_files), "appendix files in results/appendices."),
  style = "Normal"
)

for (i in seq_along(appendix_files)) {
  doc <- body_add_break(doc)
  doc <- body_add_docx(doc, src = appendix_files[[i]])
}

print(doc, target = combined_file)
message("Wrote combined appendix file: ", combined_file)
