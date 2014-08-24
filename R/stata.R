#' Send commands to a Stata process
#'
#' Functions to send commands to a Stata process. DESCRIBE MORE: todo
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
#' stata("help regress")
#' ## Many commands
#' stata(c( "set obs 200", "gen a = 1" ))
#' ## External file
#' stata("foo.do")
#'
#' ## Data input to Stata
#' x <- data.frame(a = rnorm(3), b = letters[1:3])
#' stata( c('sum a'), data.in = x)
#'
#' ## Data output from Stata
#' x <- stata(c("set obs 200", "gen a = 1"), data.out = TRUE)
#' head(x)
#'
#' ## Data input/output
#' y <- stata("replace a = 2", data.in = x, data.out = TRUE)
#' y
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

  OS <- Sys.info()["sysname"]
  OS.type <- .Platform$OS.type
  
  ## External .do script 'support': KIS
  ## ----------------------------------
  if (length(src)==1 && file.exists(src))
    src <- readLines(src)

  ## Connections
  ## -----------
  ## con: R -> Stata command interface
  fifoFile <- tempfile("RStataFifo", fileext = ".do")
  con <- fifo(fifoFile, "w+")

  ## Stata invocation parameters handling
  quietPar <- if (!stata.quiet) {
    ""
  } else {
    if (OS=="Linux"){
      "-q"
    } else if (OS=="Windows") {
      "/q"
    } else {
      ""
    }
  }
  
  ## rdl: Stata -> R output retrieval
  rdl <- pipe(paste(stata.path, quietPar , "do", fifoFile))
  
  ## data.in 'connection'
  if (is.data.frame(data.in)){
    dtainFile <- tempfile("RStataIn", fileext = ".dta")
    write.dta(data.in, file = dtainFile, version = ifelse(stata.version >= 7, 7L, 6L), ...)
    src <- c(sprintf("use %s",  file_path_sans_ext(dtainFile)), src)
  }

  ## src management dued to data.out
  if (data.out) {
    dtaoutFile <- tempfile("RStataOut", fileext = ".dta")
    src <- c(src, sprintf("%s %s",
                          ifelse(stata.version >= 13, "saveold", "save"),
                          file_path_sans_ext(dtaoutFile) ))
  }
    
  ## adding this command to the end simplify life if user make changes but
  ## doesn't want a data.frame back
  src <- c(src, "exit, clear STATA")
  
  ## write to and read from Stata
  writeLines(src, con)
  stataLog <- readLines(rdl)
  if (stata.echo) cat(stataLog, sep = "\n")

  ## Required connection management
  close(con)
  close(rdl)
  unlink(fifoFile)

  ## cleaning file passed to Stata
  if (is.data.frame(data.in)) unlink(dtainFile)
  
  ## get data
  if (data.out){
    res <- read.dta(dtaoutFile, ...)
    unlink(dtaoutFile)
    invisible(res)
  }
  
  
}
