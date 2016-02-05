findDups <- function(folder, remove.files=FALSE) {
     # This script is used to find and optionally remove duplicated files in a folder.
     # Please be AWARE THAT DELETIONS AREN'T UNDOABLE!! If they're gone, they're gone for good!
     
     # load required libraries
     library(digest)
     
     # get the filenames
     b <- dir(path = folder,full.names = TRUE,recursive = TRUE,no.. = TRUE,include.dirs = TRUE)
     
     # obtain md5s of each file. Cut it to 5000 to mantain time within reasonable
     md5s <- sapply(b, digest, file = TRUE, algo = "md5", length = 5000)
     
     # now split the list by md5s
     duplicate_files <- split(b,md5s)
     
     # get those md5s that have more than one file
     z2 <- sapply(duplicate_files, function(x) length(x)>1)
     z3 <- split(duplicate_files,z2)
     realDup <- z3$"TRUE"
     
     # For a reason I don't yet understand, there are differing files with same md5s, so let's see
     #   if there are some potentially false duplicates, by analizing their size
     falseDup <- which(!sapply(realDup, function(x) sum(diff(file.info(x)$size)^2)==0))
     
     # Candidates to deletion are those in realDup that weren't in falseDup
     delCandidate <- setdiff(1:length(realDup),falseDup)
     rmdup <- realDup[delCandidate]
     
     # If user choose to remove the files, then this is what happens
     if (isTRUE(remove.files)) {
          len <- sapply(rmdup, length)  # We get the number of files for each md5
          for (i in 1:length(rmdup)){
               file.remove(rmdup[[i]][1:(len[i]-1)]) # and we will only let the last one remain
          }
          print(paste(sum(len)-length(len), " files removed")) # Inform how many files were removed
     }

}