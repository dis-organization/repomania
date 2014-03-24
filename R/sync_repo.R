require(assertthat)
require(R.utils)
require(RJSONIO) ## we use isValidJSON from here
require(jsonlite) ## and fromJSON from here

#' Load RAADtools data repository configuration file
#'
#' This configuration specifies global settings that control the synchronization behaviour in general, and provides details of each of the datasets in the repository. The config_file parameter is required. Any arguments provided to this function will override the corresponding setting in the configuration file.
#'
#' Each dataset is specified by the following parameters in the configuration file:
#' \itemize{
#' \item name string: dataset name
#' \item description string: description
#' \item local_directory string: the path to this dataset, relative to the local root directory specified in the global local_file_root parameter
#' \item source_url string: URL to the data
#' \item do_sync logical: if FALSE this dataset will not be synchronized
#' \item method string: the synchronization method, either "wget" or a dataset-specific custom handler
#' \item method_flags string: flags to pass to wget (if method=="wget") or the custom handler (if not)
#' \item also_get string array: any additional file URLs to synchronize, such as lonlat grid files
#' \item postprocess string array: operations to apply after all file downloads. Currently "decompress" (find any compressed files, uncompress them and delete the compressed copy), "unzip", "gunzip" (similar but specifying the compression type), "unzip_nodelete" (unzip but do not delete the zipped file), "cleanup <pattern>" (remove any files in local_directory with names matching <pattern>), "cleanup_recursive <pattern>" (as for cleanup but is applied recursively to child directories)
#' \item sync_from_year numeric: do not synchronize data from years prior to this (only supported for some datasets)
#' \item user string: username for access to restricted ftp/http sites (not yet implemented)
#' \item password string: password for access to restricted ftp/http sites (not yet implemented)
#' }
#'
#' @param config_file string: file or URL to JSON configuration file
#' @param local_file_root string: the location of the repository on the local file system
#' @param skip_downloads logical: use TRUE for a dry-run in which no data files will be downloaded
#' @param clobber numeric: 0=do not overwrite existing files, 1=overwrite if remote file is newer than local, 2=overwrite local files unconditionally. For files that are compressed on the server, a clobber value of 1 is currently treated as 2
#' @param wait numeric: seconds to pause in between successive downloads
#' @param wget_flags string: global flags to pass to wget. These will be applied in addition to any dataset-specific wget flags
#' @param http_proxy string: http proxy, in the form "http://proxy.server:port" (not yet implemented)
#' @param http_proxy_user string: username/password for http proxy, in the form "username:password" (not yet implemented)
#' @param ftp_proxy string: ftp proxy, in the form "ftp://proxy.server:port" (not yet implemented)
#' @param ftp_proxy_user string: username/password for ftp proxy, in the form "username:password" (not yet implemented)
#' @return a named list with components "global" and "datasets"
#' @export
repo_config=function(config_file="raad_repo_config.json",local_file_root,skip_downloads,clobber,wait,wget_flags,http_proxy,http_proxy_user,ftp_proxy,ftp_proxy_user) {
    ## check that config file is valid JSON
    if (!RJSONIO::isValidJSON(config_file)) {
        stop("configuration file ",config_file," is not valid JSON")
    }
    ## read configuration file and override with anything provided directly here
    cf=jsonlite::fromJSON(readLines(config_file))
    if (!missing(local_file_root)) {
        cf$global$local_file_root=local_file_root
    }
    assert_that(is.string(cf$global$local_file_root)) ## just check that it's a string, not that it's a directory
    ## make sure that local_file_root path is in correct form for this system (but don't test its existence)
    cf$global$local_file_root=normalizePath(cf$global$local_file_root,mustWork=FALSE)
    if (!missing(wget_flags)) {
        cf$global$wget_flags=wget_flags
    }
    assert_that(is.string(cf$global$wget_flags))
    if (!missing(http_proxy)) {
        cf$global$http_proxy=http_proxy
    }
    assert_that(is.string(cf$global$http_proxy))
    if (!missing(http_proxy_user)) {
        cf$global$http_proxy_user=http_proxy_user
    }
    assert_that(is.string(cf$global$http_proxy_user))
    if (!missing(ftp_proxy)) {
        cf$global$ftp_proxy=ftp_proxy
    }
    assert_that(is.string(cf$global$ftp_proxy))
    if (!missing(ftp_proxy_user)) {
        cf$global$ftp_proxy_user=ftp_proxy_user
    }
    assert_that(is.string(cf$global$ftp_proxy_user))
    if (!missing(skip_downloads)) {
        cf$global$skip_downloads=skip_downloads
    }
    assert_that(is.flag(cf$global$skip_downloads))
    if (!missing(clobber)) {
        cf$global$clobber=clobber
    }
    assert_that(cf$global$clobber %in% c(0,1,2))
    if (!missing(wait)) {
        cf$global$wait=wait
    }
    assert_that(is.na(cf$global$wait) || cf$global$wait>=0)
    ## make sure that we have all the expected fields populated with non-NULL
    check_fields=c("wget_flags","http_proxy","http_proxy_user","ftp_proxy","ftp_proxy_user","local_file_root")
    for (fi in check_fields) {
        if (is.null(cf$global[fi])) {
            cf$global[fi]=""
        }
    }
    if (is.null(cf$global$clobber)) {
        cf$global$clobber=0
    }
    if (is.null(cf$global$wait)) {
        cf$global$wait=5
    }
    if (is.null(cf$global$skip_downloads)) {
        cf$global$skip_downloads=FALSE
    }
    cf
}



sync_repo=function(config,create_root=FALSE) {
    ## general synchronization handler
    ## check that wget can be found
    tryCatch(
        system("wget --help",intern=TRUE),
        error=function(e) stop("could not find wget executable (error message was: ",e,")")
    )
    ## print a summary of the local repo settings
    cat(sprintf("\n\nSynchronizing local repository: %s\n",config$global$local_file_root))
    switch(as.character(config$global$clobber),
           "0"=cat(sprintf("Not overwriting existing files\n")),
           "1"=cat(sprintf("Overwriting existing files if remote is newer than local\n")),
           "2"=cat(sprintf("Overwriting all existing local files\n"))
           )
    if (config$global$skip_downloads) {
        cat(sprintf("Skipping all downloads\n"))
    }
    cat(sprintf("\n\n"))
    ## save current working directory
    working_dir=getwd()
    ## check that the repo root directory exists
    if (!direxists(config$global$local_file_root)) {
        ## no, it does not exist
        ## unless create_root is TRUE, we won't create it, in case the user simply hasn't specified the right location
        if (create_root) {
            dir.create(config$global$local_file_root,recursive=TRUE)
        } else {
            setwd(working_dir)
            stop("local_file_root: ",config$global$local_file_root," does not exist. Either create it or run sync_repo with create_root=TRUE")
        }
    }

    ## iterate through each dataset in turn
    for (di in 1:nrow(config$datasets)) {
        this_dataset=config$datasets[di,]
        if (this_dataset$do_sync) {
            cat(sprintf("\nSynchronizing dataset: %s\n",this_dataset$name))
            ## check that local directory exists
            this_dir=normalizePath(file.path(config$global$local_file_root,this_dataset$local_directory),mustWork=FALSE)
            create_chdir(this_dir)

            ## do the main synchonization, usually directly with wget, otherwise with custom methods
            switch(this_dataset$method,
                   wget={
                       do_wget(build_wget_call(config,this_dataset),config)
                   },
                   nsidc_nrt={
                       sync_nsidc_nrt(config,this_dataset)
                   },
                   ifremer_ssmi={
                       sync_ifremer_ssmi(config,this_dataset)
                   },
                   stop("unsupported method ",this_dataset$method," specified")
               )

            ## also get any "also_get" files
            also_get=this_dataset$also_get
            if (is.list(also_get) && length(also_get)==1) {
                also_get=also_get[[1]] ## seem to get char vector embedded in single-element list
            }
            if (!is.null(also_get) && also_get %in% c(NA,"NA")) also_get=NULL
            if (length(also_get)>0) {
                for (ii in 1:length(also_get)) {
                    ## does this file already exist?
                    if (!file_exists_uncompressed(basename(also_get[ii])) || config$global$clobber>0) {
                        do_wget(build_wget_call(config,this_dataset,fileurl=also_get[ii]),config)
                    }
                }
            }
            ## postprocessing
            pp=this_dataset$postprocess
            if (is.list(pp) && length(pp)==1) {
                pp=pp[[1]] ## may get char vector embedded in single-element list
            }
            if (!is.null(pp) && pp %in% c(NA,"NA")) pp=NULL
            if (length(pp)>0) {
                for (i in 1:length(pp)) {
                    if (identical(tolower(pp[i]),"decompress")) {
                        ## general decompress-then-delete-compressed files
                        do_decompress_files()
                    } else if (tolower(pp[i]) %in% c("unzip","unzip_nodelete","gunzip")) {
                        ## finer control over decompression behaviour
                        do_decompress_files(pp[i])
                    } else if (grepl("^cleanup",tolower(pp[i]))) {
                        file_pattern=sub("(cleanup|cleanup_recursive) ","",pp[i])
                        recursive=grepl("^cleanup_recursive",tolower(pp[i]))
                        to_delete=list.files(pattern=file_pattern,recursive=recursive)
                        cat(sprintf("cleaning up files: %s\n",paste(to_delete,collapse=",")))
                        unlink(to_delete)
                    } else {
                        stop("unrecognized postprocess option ",pp[i])
                    }
                }
            }
        } else {
            cat(sprintf("\nSkipping dataset (do_sync is FALSE): %s\n",this_dataset$name))
        }
    }
    setwd(working_dir)
}

##--
## specific synchronizers for various data sources

sync_ifremer_ssmi=function(config,dataset) {
    ## method for IFREMER SSMI data
    ## remote files live in directories by year, and are Z-compressed
    dir_list=get_file_list(config,dataset)$dirs
    dir_list=sort(dir_list,decreasing=TRUE) ## start from most recent year
    if (!is.null(dataset$sync_from_year) && !dataset$sync_from_year %in% c("NA",NA)) {
        assert_that(is.count(as.numeric(dataset$sync_from_year)))
        cat(sprintf("  only synchronizing data from year %s onwards\n",dataset$sync_from_year))
        dir_list=dir_list[as.numeric(dir_list)>=as.numeric(dataset$sync_from_year)]
    }
    for (this_year in dir_list) {
        create_chdir(as.character(this_year))
        file_list=get_file_list(config,dataset,url=paste(dataset$source_url,this_year,"/",sep=""))$files
        ## which files don't we have?
        need_files=sub("\\.Z$","",file_list)
        if (config$global$clobber<1) {
            ## don't re-download existing files
            need_files=setdiff(need_files,list.files())
        }
        if (length(need_files)>0) {
            need_files=paste(dataset$source_url,this_year,"/",need_files,".Z",sep="")
            ## dump these file names to a temporary file
            url_file=tempfile()
            writeLines(need_files,con=url_file)
            wget_call=build_wget_call(config,dataset,fileurl=paste("--input-file=",url_file,sep="")) ## pass the file of URLs to wget
            do_wget(wget_call,config)
            do_decompress_files("gunzip")
        } else {
            ## no files to retrieve
            ## might want to assume that we are up to date and bail out of the this_year loop?
        }
        setwd("..")
    }
}


sync_nsidc_nrt=function(config,dataset) {
    ## method for NSIDC NRT data
    ## remote files live in single directory, but we want them split into yearly directories as per the gsfc-final daily data
    file_list=get_file_list(config,dataset)$files
    this_year=as.numeric(format(Sys.time(),"%Y"))
    while (TRUE) {
        ## iterate through years
        create_chdir(as.character(this_year))
        this_files=file_list[grepl(sprintf("^.._%d",this_year),file_list)] ## files for this_year
        if (length(this_files)<1) {
            ## assume we've checked all available files
            setwd("..")
            break
        }
        this_files=paste(dataset$source_url,this_files,sep="")
        ## dump these file names to a temporary file
        url_file=tempfile()
        writeLines(this_files,con=url_file)
        wget_call=build_wget_call(config,this_dataset,fileurl=paste("--input-file=",url_file,sep="")) ## pass the file of URLs to wget
        do_wget(wget_call,config)
        this_year=this_year-1
        setwd("..")
        if (this_year<1978) {
            ## just to make sure we can't get trapped here endlessly
            break
        }
    }
}


##--
## various helper functions

file_exists_uncompressed=function(this_file) {
    ## does this file exist, either as-is or in uncompressed form?
    file.exists(sub("\\.(gz|zip|Z)$","",this_file))
}

do_decompress_files=function(method,recursive=FALSE) {
    ## decompress (unzip/gunzip) all compressed files in current directory (and children if recursive is TRUE)
    if (missing(method)) {
        ## decompress anything we can find, assuming delete-after-unzip
        do_decompress_files("unzip",recursive=recursive)
        do_decompress_files("gunzip",recursive=recursive)
        return
    }
    assert_that(is.string(method))
    method=match.arg(method,c("unzip","unzip_nodelete","gunzip"))
    assert_that(is.flag(recursive))
    ## unzip() issues warnings in some cases when operations have errors, and sometimes issues actual errors
    warn=getOption("warn") ## save current setting
    options(warn=0) ## so that we can be sure that last.warning will be set
    switch(method,
           "unzip_nodelete"=,
           "unzip"=zipped_files<-list.files(pattern="\\.zip$",recursive=recursive),
           "gunzip"=zipped_files<-list.files(pattern="\\.(gz|Z)$",recursive=recursive),
           stop("unsupported decompress method ",method)
           )
    for (thisf in zipped_files) {
        ## decompress, check for errors in doing so
        cat(sprintf("  decompressing: %s\n",thisf))
        switch(method,
               "unzip_nodelete"=,
               "unzip"={
                   was_ok=FALSE
                   suppressWarnings(warning("")) ## clear last.warning message
                   tryCatch({ unzipped_files<-unzip(thisf,list=TRUE) ## get list of files in archive
                              unzip(thisf) ## now actually unzip them
                              was_ok=is.null(last.warning[[1]]) && all(file.info(unzipped_files$Name)$size>0)
                          },
                            error=function(e) {
                                ## an error here might be because of an incompletely-downloaded file. Is there something more sensible to do in this case?
                                cat(sprintf("  %s failed to unzip, it may be incompletely-downloaded\n",thisf))
                            })
                   if (identical(method,"unzip")) {
                       ## if all looks OK, delete zipped file
                       if (was_ok) {
                           cat(sprintf("  unzip of %s appears OK, deleting\n",thisf))
                           unlink(thisf)
                       } else {
                           cat(sprintf("  problem unzipping %s, not deleting\n",thisf))
                       }
                   }
               },
               "gunzip"={
                   ## wrap this in tryCatch block so that errors do not halt our whole process
                   ## gunzip takes care of deleting the compressed file
                   tryCatch(gunzip(thisf,destname=sub("\\.(gz|Z)$","",thisf),overwrite=TRUE),
                            error=function(e){
                                cat(sprintf("  problem gunzipping %s",thisf))
                            }
                            )
               },
               stop("unsupported decompress method ",method)
               )
    }
    options(warn=warn) ## reset
}

direxists=function(z) file.exists(dirname(z)) && !(!file.info(z)$isdir || is.na(file.info(z)$isdir))

create_chdir=function(this_dir) {
    ## change to directory, creating it if necessary
    if (!direxists(this_dir)) {
        ## directory does not exist, let's create it
        cat(sprintf("  creating directory %s\n",this_dir))
        dir.create(this_dir,recursive=TRUE)
    }
    cat(sprintf("  changing to directory %s\n",this_dir))
    setwd(this_dir)
}

##--
## download and internet interaction functions

get_file_list=function(config,dataset,pattern,url) {
    ## retrieve file list from ftp server
    if (!missing(pattern)) {
        assert_that(is.string(pattern))
    }
    ## use dataset$source_url unless url has been provided
    if (!missing(url)) {
        assert_that(is.string(url))
    } else {
        url=dataset$source_url
    }
    this_transport=gsub(":.*","",url)
    if (identical(tolower(this_transport),"ftp")) {
        if (file.exists("file_list.txt")) {
            unlink("file_list.txt")
        }
        if (file.exists(".listing")) {
            unlink(".listing")
        }
        wget_call=build_wget_call(config,dataset,fileurl=paste("--no-remove-listing -O file_list.txt",url,sep=" "))
        temp_config=config
        temp_config$global$skip_downloads=FALSE ## else we won't get a listing!
        do_wget(wget_call,temp_config)
        fl=readLines(".listing")
        fl=fl[!grepl("^total",fl)]
        dir_list=fl[grepl("^d",fl)]
        dir_list=sapply(strsplit(dir_list," "),function(z)rev(z)[1]) ## extract names
        dir_list=dir_list[! dir_list %in% c(".","..")]
        file_list=fl[grepl("^[^d]",fl)]
        file_list=sapply(strsplit(file_list," "),function(z)rev(z)[1]) ## extract names
        ## clean up
        unlink(".listing")
        unlink("file_list.txt")
    } else {
        stop("no file list for http")
    }
    if (!missing(pattern)) {
        file_list=file_list[grepl(pattern,file_list)]
    }
    list(files=file_list,dirs=dir_list)
}

build_wget_call=function(config,dataset,fileurl) {
    ## build wget system call given our config and dataset
    assert_that(config$global$clobber %in% c(0,1,2))
    wget_call="wget"
    ## TODO: also need to take care of proxy settings here
    ## set clobber flags
    switch(as.character(config$global$clobber),
           "0"={ wget_call=paste(wget_call,"--no-clobber",sep=" ") },
           "1"={ wget_call=paste(wget_call,"-N",sep=" ") },
           "2"={ if (!missing(fileurl)) { output_file_name=basename(fileurl); wget_call=paste(wget_call,"-O",output_file_name,sep=" ") } }
                 ## for "2" and url supplied, do nothing. this may not work as expected if we are downloading a file rather than a directory. wget -r (with no -nc or -N flags) should overwrite an existing file, but wget a_url may not
           )
    if (nchar(config$global$wget_flags)>0) {
        wget_call=paste(wget_call,config$global$wget_flags,sep=" ")
    }
    if (!missing(fileurl)) {
        ## downloading single file
        ## don't use method_flags, since they are about recursiveness and such like
        assert_that(is.string(fileurl))
        wget_call=paste(wget_call,fileurl,sep=" ")
    } else {
        if (nchar(dataset$method_flags)>0) {
            assert_that(is.string(dataset$method_flags))
            wget_call=paste(wget_call,dataset$method_flags,sep=" ")
        }
        if (config$global$wait>0) {
            wget_call=paste(wget_call," --wait=",as.character(config$global$wait),sep="")
        }
        assert_that(is.string(dataset$source_url))
        wget_call=paste(wget_call,dataset$source_url,sep=" ")
    }
    wget_call
}

do_wget=function(wget_call,config) {
    assert_that(is.string(wget_call))
    if (config$global$skip_downloads) {
        cat(sprintf(" skip_downloads is TRUE, not executing: %s\n",wget_call))
    } else {
        cat(sprintf(" executing: %s\n",wget_call))
        system(wget_call)
    }
}



## random notes on wget flags:
## wget --mirror
## wget -N -r -l inf --no-remove-listing
## --ftp-user=USER, --ftp-password=PASS, (or just --user=USER, --password=PASS
## --no-passive-ftp
## --proxy-user=USER, --proxy-pass=PASS
## --cut-dirs=
## -nH no hostname
## --wait=
## -N --timestamping don't re-retrieve files unless newer than local
## --no-clobber

## can exclude files for download based on file type (extension) but not based on file name patterns
## --accept=csv
## --reject=csv
