getFilingsLimited <- function (cik.no = cik, form.type, 
                              filing.year, 
                              filing.date,
                              quarter = c(1, 2, 3, 4), 
                              downl.permit = "y") 
{
  options(warn = -1)
  if (!is.numeric(filing.year)) {
    cat("Error: Input year(s) is not numeric.")
    return()
  }
  filing.year <- filing.year[filing.year >= 1994]
  if (length(filing.year) == 0) {
    cat("Please provide filing years after 1993.")
    return()
  }
  getdownCompat <- function() {
    if (nzchar(Sys.which("libcurl"))) {
      dmethod <- "libcurl"
    }
    else if (nzchar(Sys.which("wget"))) {
      dmethod <- "wget"
    }
    else if (nzchar(Sys.which("curl"))) {
      dmethod <- "curl"
    }
    else if (nzchar(Sys.which("lynx"))) {
      dmethod <- "lynx"
    }
    else if (nzchar(Sys.which("wininet"))) {
      dmethod <- "wininet"
    }
    else {
      dmethod <- "auto"
    }
    return(dmethod)
  }
  DownloadSECFile <- function(link, dfile, dmethod) {
    tryCatch({
      utils::download.file(link, dfile, method = dmethod, 
                           quiet = TRUE)
      return(TRUE)
    }, error = function(e) {
      return(FALSE)
    })
  }
  dmethod <- getdownCompat()
  index.df <- data.frame()
  for (year in filing.year) {
    yr.master <- paste0(year, "master.Rda")
    filepath <- paste0("Master Indexes/", yr.master)
    if (!file.exists(filepath)) {
      getMasterIndex(year)
    }
    load(filepath)
    if (form.type == "ALL") {
      form.type <- unique(year.master$form.type)
    }
    if (cik.no == "ALL") {
      year.master <- year.master[which(year.master$form.type %in% form.type & year.master$quarter %in% quarter),]
    }
    else {
      year.master <- year.master[which(year.master$cik %in% cik.no & 
                                         year.master$form.type %in% form.type & 
                                         year.master$quarter %in% quarter), ]
    }
    if (nrow(year.master) > 0) {
      year.master$filing.year <- year
      index.df <- rbind(index.df, year.master)
    }
  }
  if (nrow(index.df) == 0) {
    cat("No filing information found for given CIK(s) and Form Type in the mentioned year(s)/quarter(s).\n")
    return()
  }
  index.df <- index.df[order(index.df$cik, index.df$filing.year), ]
  index.df <- index.df %>% filter(date.filed == as.character(filing.date)) # dropping everything but the data I care about
  total.files <- nrow(index.df)
  msg3 <- paste0("Total number of filings to be downloaded = ", 
                 total.files, ". Do you want to download (y/n)? ")
  if (as.character(downl.permit) == "n") {
    downl.permit <- readline(prompt = msg3)
  }
  if (as.character(downl.permit) == "y") {
    dir.create("Edgar filings_full text")
    cat("Downloading fillings. Please wait...", "\n")
    progress.bar <- txtProgressBar(min = 0, max = total.files, 
                                   style = 3)
    index.df$edgar.link <- as.character(index.df$edgar.link)
    accessions <- do.call(rbind.data.frame, strsplit(index.df$edgar.link, "\\/"))[4]
    index.df$accession.number <- gsub("\\.txt", "", accessions[,1])
    row.names(index.df) <- c(1:nrow(index.df))
    index.df$status <- NA
    for (i in 1:total.files) {
      edgar.link <- paste0("https://www.sec.gov/Archives/", 
                           index.df$edgar.link[i])
      f.type <- gsub("/", "", index.df$form.type[i])
      year <- index.df$filing.year[i]
      cik <- index.df$cik[i]
      new.dir <- paste0("Edgar filings_full text/Form ", 
                        f.type)
      dir.create(new.dir)
      new.dir2 <- paste0(new.dir, "/", cik)
      dir.create(new.dir2)
      dest.filename <- paste0(new.dir2, "/", cik, "_", 
                              f.type, "_", index.df$date.filed[i], "_", index.df$accession.number[i], 
                              ".txt")
      if (file.exists(dest.filename)) {
        res <- TRUE
      }
      else {
        res <- DownloadSECFile(edgar.link, dest.filename, 
                               dmethod)
      }
      if (res == FALSE) {
        Sys.sleep(5)
        res <- DownloadSECFile(edgar.link, dest.filename, 
                               dmethod)
      }
      if (res) {
        index.df$status[i] <- "Download success"
      }
      else {
        index.df$status[i] <- "Download Error"
      }
      setTxtProgressBar(progress.bar, i)
    }
    index.df$edgar.link <- NULL
    close(progress.bar)
    return(index.df)
  }
}
