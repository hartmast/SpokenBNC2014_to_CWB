
# This script converts the files in the "spoken/tagged"
# folder of SpokenBNC2014 to VRT format.



# list of files -----------------------------------------------------------

# change path to directory if needed
f <- list.files("../bnc2014spoken-xml/spoken/tagged/", full.names = T)



# get speaker metadata ----------------------------------------------------

# again, change path to directory if needed
mt <- readLines("../bnc2014spoken-xml/spoken/metadata/speakerInfo.xml")

# split up speaker metadata
mt_start <- grep("<speaker id=", mt)
mt_end   <- grep("</speaker>", mt)

# as list with speaker IDs as name of list elements
mt <- lapply(1:length(mt_start), function(i) mt[(mt_start[i]:mt_end[i])])
names(mt) <- sapply(1:length(mt), function(i) gsub(".*id=\"|\">", "", mt[[i]][1]))



# transform nested xml elements to attributes
# that will then be inserted in the u tag

mt2 <- list() #empty list for collecting the results

for(i in 1:length(mt)) {
  # list of names of speaker attributes:
  spk_attr <- gsub("/|>", "", gsub(".*<", "", mt[[1]][2:25]))
  
  # list of values of speaker attributes:
  spk_vlu  <- gsub("<.*?>|\t", "", mt[[1]][2:25])
  
  # paste together, separated by space
  mt2[[i]] <- paste(sapply(1:length(spk_attr), function(i) paste(spk_attr[i], "=\"", spk_vlu[i], "\"", sep="", collapse="")), sep="", collapse = " ")
  
}


names(mt2) <- names(mt)



for(fl in 1:length(f)) {
  
  # read data ---------------------------------------------------------------
  
  t <- readLines(f[fl])
  
  
  # collect XML attributes
  if(fl == 1) {
    attr_list <- unique(gsub(" .*", "", t))
  } else {
    attr_list_cur <- unique(gsub(" .*", "", t))
    attr_list <- unique(c(attr_list, attr_list_cur))
  }
  
  

  # add speaker metadata ----------------------------------------------------

  # get "who" attribute
  who <- grep("who=", t)  
  
  # add speaker metadata to each line that contains "who"
  for(j in 1:length(who)) {
    who_id <- gsub("\".*", "", gsub(".*who=\"", "", t[who[j]]))
    
    if(who_id %in% names(mt2)) {
      
      # add metadata from metadata list
      mt_number <- which(names(mt2)==who_id)
      
      # add metadata to the "u" attributes
      t[who[j]] <- gsub("who=", 
                        paste(mt2[mt_number], " who="),
                        t[who[j]])
    }
    
  }
  
  
  
  
  
  
  # verticalize "w" lines ---------------------------------------------------
  
  w <- grep("^<w", t)
  
  for(i in 1:length(w)) {
    
    # replace respective line with verticalized text
    t[w[i]] <-  paste(
      c(gsub("<.*?>", "", t[w[i]]),
        gsub("\".*", "", gsub(".*pos=\"", "", t[w[i]])),
        gsub("\".*", "", gsub(".*lemma=\"", "", t[w[i]])),
        gsub("\".*", "", gsub(".*class=\"", "", t[w[i]])),
        gsub("\".*", "", gsub(".*usas=\"", "", t[w[i]]))
      ), collapse = "\t"
    )
    
    # print(i)
    
  }
  
  
  
  
  # export vrt --------------------------------------------------------------
  
  if(fl == 1) {
    write.table(t, "spokenbnc2014.vrt", row.names = F, col.names = F, quote = F)
  } else {
    write.table(t, "spokenbnc2014.vrt", row.names = F, col.names = F, quote = F, 
                append = T)
  }
  
  print(paste(fl, " of ", length(f), " files processed", sep="", collapse=""))
  
  
}

