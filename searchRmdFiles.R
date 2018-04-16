#### Works on OSX, using "locate"

Rmdfiles = system("locate Rmd", intern = T)
Rmdfiles = grep("Rmd$", Rmdfiles, value=T)
length(Rmdfiles)
saveOptions = options(warn=-1)
pattern = "^date"
for(f in Rmdfiles[1:20]) {
  result = system(paste0("grep -i ", "'", pattern, "' ", "'", f, "'"), intern=T)
  if(length(result) > 0) {
    cat(f, "==> \n", result, "\n")
  }
}
options(saveOptions)