### Functions for retrieving climate data

getPRISMnorms <- function(var = c("tmean", "ppt"), focal.months = 1:12,
                          focal.region = extent(-180, 180, -180, 180),
                          dnld.dir = paste0(getwd(), "/"), import = TRUE,
                          metadata = TRUE,quiet = TRUE){
  
  ## load required packages
  require(raster)
  
  ## create file system for data
  workdir <- paste0(dnld.dir, ifelse(var == "tmean", "Temperature", "Precipitation"),
                    "/PRISM normals 800m ", var, "/")
  if(!file.exists(workdir)){
    dir.create(workdir, recursive = TRUE)
  }
  
  ## fetch metadata & documentation
  if(metadata == TRUE & !file.exists(paste0(workdir, "PRISM_datasets.pdf"))){
    if(quiet == FALSE){print("Fetching metadata...")}
    download.file(url = "http://prism.nacse.org/documents/Daly2008_PhysiographicMapping_IntJnlClim.pdf",
                  destfile = paste0(workdir, "Daly2008.pdf"), quiet = quiet)
    download.file(url = "http://prism.nacse.org/documents/PRISM_datasets.pdf",
                  destfile = paste0(workdir, "PRISM_datasets.pdf"), quiet = quiet)
  }
  
  ## fetch data
  if(quiet == FALSE){print("Fetching data...")}
  zipfiles <- vector(mode = "character")
  urls <- vector(mode = "character")
  for(i in 1:length(focal.months)){
    # Specify file names
    zipfiles[i] <- paste0("PRISM_", var, "_30yr_normal_800mM2_",
                          ifelse(focal.months[i] < 10, paste0(0, focal.months[i]), focal.months[i]),
                          "_bil.zip")
    # Download files and unzip only if file doesn't already exist
    if(!file.exists(paste0(workdir, substr(zipfiles[i], 1, nchar(zipfiles[i])-3), "bil"))){
      download.file(paste0("ftp://anonymous@prism.nacse.org:21/normals_800m/", var, "/",
                           zipfiles[i]), destfile = paste0(workdir, zipfiles[i]), mode = "wb", quiet = quiet)
      unzip(paste0(workdir, zipfiles[i]), exdir = workdir)
      invisible(file.remove(paste0(workdir, zipfiles[i])))
    }
  }
  
  ## import data into workspace
  if(import == TRUE){
    if(quiet == FALSE){print("Importing data...")}
    bilfiles <- as.list(paste0(workdir, gsub(".zip", ".bil", zipfiles)))
    norm.rasters <- stack(lapply(bilfiles, function(x){crop(raster(x), focal.region)}))
    mean.raster <- overlay(norm.rasters, fun = mean)
    
    if(quiet == FALSE){print("Data are loaded into working environment")}
    return(mean.raster)
  }else{
    print(paste("PRISM", var, "normals have been downloaded to", workdir))
  }
}

getGHCNdata <- function(var = c("tmean", "ppt"), fun = c("mean", "sum", "raw"),focal.months = 1:12,
                        focal.years = 1980:2010, focal.region = extent(-180, 180, -180, 180),
                        dnld.dir = paste0(getwd(), "/"), import = TRUE, metadata = TRUE,
                        quiet = FALSE){
  ## load required packages
  require(raster)
  require(sp)
  
  ## create file system for data
  workdir <- paste0(dnld.dir, ifelse(var == "tmean", "Temperature", "Precipitation"), "/NOAA GHCN ",
                    var, "/")
  if(!file.exists(workdir)){
    dir.create(workdir, recursive = TRUE)
  }
  
  ## fetch metadata & documentation
  if(metadata == TRUE & !file.exists(paste0(workdir, "README.txt"))){
    if(quiet == FALSE){print("Fetching metadata...")}
    if(var == "tmean"){
      download.file("http://www1.ncdc.noaa.gov/pub/data/ghcn/v3/README",
                    destfile = paste0(workdir, "README.txt"), quiet = quiet)
      download.file("http://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCNDMS_documentation.pdf",
                    paste0(workdir, "GHCNDMS_documentation.pdf"), quiet = quiet)
    }else if(var == "ppt"){
      download.file("http://www1.ncdc.noaa.gov/pub/data/ghcn/v2/v2.prcp.readme",
                    paste0(workdir, "README.txt"), quiet = quiet)
      download.file("http://www1.ncdc.noaa.gov/pub/data/cdo/documentation/GHCNDMS_documentation.pdf",
                    paste0(workdir, "GHCNDMS_documentation.pdf"), quiet = quiet)
    }
  }
  
  ## fetch data
  if(quiet == FALSE){print("Fetching data...")}
  if(length(list.files(workdir, pattern = "\\.dat|\\.inv")) < 2){ ## better way to do this check is
                                                                  ## var=="ppt"&!file.exists() | var==
    if(var == "tmean"){
      download.file("ftp://anonymous@ftp.ncdc.noaa.gov/pub/data/ghcn/v3/ghcnm.tavg.latest.qca.tar.gz",
                    paste0(workdir, "ghcnm.tavg.latest.qca.tar.gz"), mode = "w", quiet = quiet)
      ghcn.files<-untar(tarfile = paste0(workdir, "ghcnm.tavg.latest.qca.tar.gz"), list = TRUE)
      untar(tarfile = paste0(workdir, "ghcnm.tavg.latest.qca.tar.gz"), exdir = workdir,
            compressed = "gzip")
      file.rename(from = paste0(workdir, substr(ghcn.files, 3 , nchar(ghcn.files))),
                  to = paste0(workdir, substr(ghcn.files, 25 ,nchar(ghcn.files))))
      unlink(c(paste0(workdir, substr(ghcn.files[1], 3, 24)),
               paste0(workdir, "ghcnm.tavg.latest.qca.tar.gz")), recursive = TRUE)
    }else if(var == "ppt"){
      download.file("ftp://anonymous@ftp.ncdc.noaa.gov/pub/data/ghcn/v2/v2.prcp_adj.Z",
                    paste0(workdir, "v2.prcp_adj.dat.Z"), mode = "w")
      download.file("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/v2/v2.prcp.inv",
                    paste0(workdir, "v2.prcp.inv"), mode = "w")
      workdir2 <- gsub(" ", "\\\\ ", workdir)
      system2("uncompress", args = paste0(workdir2, "v2.prcp_adj.dat.Z"))
    }
  }else{
    ghcn.files <- list.files(workdir, pattern = "\\.dat$|\\.inv")
  }
  
  ## import data into workspace
  if(import == TRUE){
    if(quiet == FALSE){print("Importing data...")}
    if(var == "tmean"){
      ghcn.df <- read.fwf(file = paste0(workdir, substr(ghcn.files[1], nchar(ghcn.files[1]) - 33,
                                                        nchar(ghcn.files[1]))),
                          widths = c(11, 4, 4, rep(c(5, 1, 1, 1), times = 12)),
                          col.names = c("ID", "YEAR", "ELEMENT",
                                        paste0(rep(c("VALUE", "DMFLAG", "QCFLAG", "DSFLAG"),
                                                   times = 12), rep(1:12, each = 4))),
                          header = FALSE, stringsAsFactors = FALSE)
      ghcn.meta <- read.fwf(file = paste0(workdir, substr(ghcn.files[2],nchar(ghcn.files[2]) - 33,
                                                          nchar(ghcn.files[2]))),
                            widths = c(11, 9, 10, 7, 31, 5, 2, 4, 2, 2, 2, 2, 1, 2, 16, 1),
                            col.names = c("ID", "LATITUDE", "LONGITUDE", "STNELEV", "NAME", "GRELEV",
                                          "POPCLS", "POPSIZ", "TOPO", "STVEG", "STLOC", "OCNDIS",
                                          "AIRSTN", "TOWNDIS", "GRVEG", "POPCSS"),
                            header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE,
                            comment.char = "~")
    }else if(var == "ppt"){
      ghcn.df <- read.fwf(file = paste0(workdir, "v2.prcp_adj"), widths = c(11, 1, 4, rep(5, 12)),
                          col.names = c("ID", "DUPLI", "YEAR",
                                        paste0(rep("VALUE", times = 12), 1:12)),
                          header = FALSE, stringsAsFactors = FALSE)
      ghcn.meta <- read.fwf(file = paste0(workdir, "v2.prcp.inv"), widths = c(11, 21, 11, 7, 8, 5),
                            col.names = c("ID", "NAME", "COUNTRY", "LATITUDE",
                                          "LONGITUDE", "STNELEV"),
                            header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE,
                            comment.char = "~")
    }
    # Clean data: replace missing values with NA
    ghcn.df[ghcn.df == "-9999"] <- NA
    
    # Crop to focal time period and region
    selcol <- switch(var,
                     tmean = c("ID", "YEAR", "ELEMENT",
                               paste0(rep(c("VALUE", "DMFLAG", "QCFLAG", "DSFLAG"),
                                          times=length(focal.months)), rep(focal.months,each=4))),
                     ppt = c("ID", "YEAR", paste0(rep("VALUE", times = length(focal.months)),
                                                  focal.months)))
    ghcn.df.crop <- subset(ghcn.df, subset = YEAR %in% focal.years, select = selcol)
    ghcn.spdf <- with(ghcn.meta, SpatialPointsDataFrame(coords = data.frame(LONGITUDE, LATITUDE),
                 data = ghcn.meta[,names(ghcn.meta) != "LATITUDE" & names(ghcn.meta) != "LONGITUDE"]))
    ghcn.df.focal <- ghcn.df.crop[ghcn.df.crop$ID %in% 
                                  ghcn.spdf[!is.na(ghcn.spdf %over%
                                                     as(focal.region, "SpatialPolygons")),]@data[,1],]
    ghcn.spdf.focal <- ghcn.spdf[ghcn.spdf$ID %in%
                                 ghcn.spdf[!is.na(ghcn.spdf %over%
                                                    as(focal.region, "SpatialPolygons")),]@data[,1],]
    ghcn.df.focal$ID <- factor(ghcn.df.focal$ID)
    
    # Convert to whole units
    if(var == "tmean"){
      cols <- paste0(rep("VALUE", times = length(focal.months)), focal.months)
      ghcn.df.focal[cols] <- ghcn.df.focal[cols]/100.0
    }else if(var == "ppt"){
      cols <- paste0(rep("VALUE", times = length(focal.months)), focal.months)
      ghcn.df.focal[cols] <- ghcn.df.focal[cols]/10
    }
    
    # Aggregate values using specified function
    if(fun == "mean"){
      ghcn.df.focal$MEAN.VALUE <- rowMeans(sapply(as.list(paste0("VALUE", focal.months)),
                                                  get, pos = ghcn.df.focal), na.rm = FALSE)
    }else if(fun == "sum"){
      ghcn.df.focal$SUM.VALUE <- rowSums(sapply(as.list(paste0("VALUE", focal.months)),
                                                get, pos = ghcn.df.focal), na.rm = FALSE)
    }else if(fun == "raw"){
      ghcn.df.focal <- ghcn.df.focal
    }
    
    return(ghcn.df.focal)
  }else{
    print(paste("NOAA GHCN-M", var, "data have been downloaded to", workdir))
  }
}

