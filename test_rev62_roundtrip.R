# Round-trip validation of SWATreadR against rev.62 reference data
# Reads each file, writes it, reads again, compares values (type-agnostic)

library(SWATreadR)

ref_path <- "path/to/refdata/Ames_sub1/"  # set to your rev.62 dataset
out_path <- tempdir()

values_equal <- function(t1, t2) {
  if(ncol(t1) != ncol(t2)) return(FALSE)
  for(i in seq_len(ncol(t1))) {
    v1 <- t1[[i]]; v2 <- t2[[i]]
    if(is.numeric(v1) && is.numeric(v2)) {
      if(!isTRUE(all.equal(as.numeric(v1), as.numeric(v2)))) return(FALSE)
    } else if(!identical(as.character(v1), as.character(v2))) return(FALSE)
  }
  TRUE
}

files <- list.files(ref_path)
for (f in files) {
  t1 <- tryCatch(read_swat(file.path(ref_path, f)), error=function(e) NULL)
  if(is.null(t1) || nrow(t1)==0) next
  of <- file.path(out_path, f)
  if(file.exists(of)) file.remove(of)
  r <- tryCatch(write_swat(t1, of), error=function(e) e)
  if(inherits(r,"error")) { cat("WRITE FAIL:", f, "\n"); next }
  t2 <- tryCatch(read_swat(of), error=function(e) NULL)
  if(is.null(t2)) { cat("REREAD FAIL:", f, "\n"); next }
  cat(if(values_equal(t1,t2)) "PASS" else "DIFF", f, "\n")
}
