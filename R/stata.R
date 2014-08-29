#' Send commands to a Stata process
#'
#' Function that sends commands to a Stata process.
#' @param src character vector of length 1 (path to \code{.do} file) or more
#' (a set of stata commands). See examples.
#' @param data.in \code{\link{data.frame}} to be passed to Stata
#' @param data.out logical value. If \code{TRUE}, the data at the end of
#' the Stata command are returned to R.
#' @param stata.path Stata command to be used
#' @param stata.version Version of Stata used
#' @param stata.echo logical value. If \code{TRUE} stata text output will be printed
#' @param stata.quiet logical value. If \code{TRUE} startup message will
#' not be printed
#' @param ... parameter passed to \code{\link{write.dta}}
#' @examples
#' \dontrun{
#' ## Single command
#' stata("help regress") #<- this won't work in Windows dued to needed batch mode
#' ## Many commands
#' stata(c( "set obs 200", "gen a = 1" ))
#' ## External .do file
#' stata("foo.do")
#'
#' ## Data input to Stata
#' x <- data.frame(a = rnorm(3), b = letters[1:3])
#' stata( "sum a", data.in = x)
#'
#' ## Data output from Stata (eg obtain 'auto' dataset)
#' auto <- stata("sysuse auto", data.out = TRUE)
#' head(auto)
#'
#' ## Data input/output
#' (y <- stata("replace a = 2", data.in = x, data.out = TRUE))
#' }
#' @export
stata <- function(src = stop("At least 'src' must be specified"),
                  data.in = NULL,
                  data.out = FALSE,
                  stata.path = getOption("RStata.StataPath", stop("You need to set up a Stata path; ?chooseStataBin")),
                  stata.version = getOption("RStata.StataVersion", stop("You need to specify your Stata version")),
                  stata.echo = getOption("RStata.StataEcho", TRUE),
                  stata.quiet = getOption("RStata.StataQuiet", TRUE),
                  ...
                  )
{
  ## -------------------------
  ## Data validation and setup
  ## -------------------------
  if (!is.character(src))
    stop("src must be a character")
  
  if (!(is.null(data.in) | is.data.frame(data.in)))
    stop("data.in must be NULL or a data.frame")
  
  if (!is.logical(data.out))
    stop("data.out must be logical")

  if (!is.numeric(stata.version))
    stop("stata.version must be logical")

  if (!is.logical(stata.echo))
    stop("stata.echo must be logical")

  if (!is.logical(stata.quiet))
    stop("stata.quiet must be logical")
  
  OS <- Sys.info()["sysname"]
  OS.type <- .Platform$OS.type
  dataIn <- is.data.frame(data.in)
  dataOut <- data.out[1L]
  stataVersion <- stata.version[1L]
  stataEcho <- stata.echo[1L]
  stataQuiet <- stata.quiet[1L]

  ## -----------------
  ## OS related config
  ## -----------------
  ## in Windows and batch mode a RStata.log (naming after RStata.do below)
  ## is generated in the current directory 
  if (OS %in% "Windows") {
    winRStataLog <- "RStata.log"
    on.exit(unlink(winRStataLog))
  }
  
  ## -----
  ## Files
  ## -----
  ## doFile <- tempfile("RStata", fileext = ".do")
  ## tempfile could be misleading if the do source other dos with relative paths
  doFile <- "RStata.do"
  on.exit(unlink(doFile), add = TRUE)

  if (dataIn){
    ## dtaInFile <- tempfile("RStataDataIn", fileext = ".dta") # Windows/Stata8 unhappy?
    dtaInFile <- "RStataDataIn.dta"
    on.exit(unlink(dtaInFile), add = TRUE)
    foreign::write.dta(data.in, file = dtaInFile, version = ifelse(stataVersion >= 7, 7L, 6L), ...)
  }  

  if (dataOut) {
    ## dtaOutFile <- tempfile("RStataDataOut", fileext = ".dta") # Windows/Stata8 unhappy?
    dtaOutFile <- "RStataDataOut.dta"
    on.exit(unlink(dtaOutFile), add = TRUE)
  }

  ## -------------------------
  ## Creating the .do file ...
  ## -------------------------
  ## External .do script 'support': KIS
  if (file.exists(src[1L]))
    src <- readLines(src[1L])

  ## put a use at the top of .do if a data.frame is passed to data.in
  if (dataIn)  src <- c(sprintf("use %s",  file_path_sans_ext(dtaInFile)), src)

  ## put a save or saveold at the end of .do if data.out == TRUE
  if (dataOut)  src <- c(src, sprintf("%s %s",
                                      ifelse(stataVersion >= 13, "saveold", "save"),
                                      file_path_sans_ext(dtaOutFile) ))
    
  ## adding this command to the end simplify life if user make changes but
  ## doesn't want a data.frame back
  src <- c(src, "exit, clear STATA")

  ## -------------
  ## Stata command
  ## -------------
  quietPar <- if (!stataQuiet) {
    ""
  } else {
    if (OS %in% "Linux"){
      "-q"
    } else if (OS %in% "Windows") {
      "/q"
    } else {
      ""
    }
  }

  ## With Windows version, /e is almost always needed (if Stata is
  ## installed with GUI)
  stataCmd <- paste(stata.path,
                    ifelse(OS %in% "Windows", "/e", ""),
                    quietPar ,
                    "do",
                    doFile)


  ## ----------------
  ## Directory change
  ## ----------------
  ## Move to a temp directory: on some OS (Windows) /e (batch mode with
  ## ASCII log and no prompting without exiting from Stata) is needed. This
  ## keeps directory clean
  ## ... but a nested call of do file could break if relative paths are used
  
  ## oldpwd <- getwd()
  ## on.exit(setwd(oldpwd), add = TRUE)
  
  ## ---
  ## IPC
  ## ---
  ## setup the .do file
  ## con <- fifo(doFile, "w+") # <- freeze with fifo in Window
  con <- file(doFile, "w")
  writeLines(src, con)
  close(con)
  
  ## execute Stata
  rdl <- pipe(stataCmd, "r")
  stataLog <- readLines(rdl)
  close(rdl)

  if (stataEcho) {
    if (OS %in% "Windows") stataLog <- readLines(winRStataLog)
    cat(stataLog, sep = "\n")
  }
  ## ------------------
  ## Get data outputted
  ## ------------------
  if (dataOut){
    res <- foreign::read.dta(dtaOutFile, ...)
    invisible(res)
  }
  
  
}
