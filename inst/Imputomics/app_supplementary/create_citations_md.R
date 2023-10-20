

res <- sapply(list_imputations(), function(ith_object_name) {
  filename <- utils::getSrcFilename(get(ith_object_name), full.names = TRUE)
  all_lines <- readLines(filename)
  
  last_line_id <- grep(pattern = paste0("^", ith_object_name), x = all_lines)
  gsub(pattern = "}", replacement = "", fixed = TRUE,
       x = strsplit(all_lines[max(grep("#' @references", all_lines[1L:last_line_id])) + 1], "{", fixed = TRUE)[[1]][2])
})

all_refs <- unique(na.omit(res))
    
all_refs_txt <- capture.output(Rdpack::get_bibentries(bibfile = "./inst/REFERENCES.bib"))
 
refs_starts <- c(1, which(all_refs_txt == "") + 1)

all_refs_txt[refs_starts] <- paste0(1L:length(refs_starts), ". ", all_refs_txt[refs_starts])

cat(c("### References of missing value imputation algorithms", 
      "The following references are the source references for missing value imputation algorithms included in the web server.",
      all_refs_txt), sep = "\n", 
    file = "inst/Imputomics/citations.md")

             