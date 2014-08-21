#' Send commands to a Stata process
#'
#' Simple functions to send commands to a Stata process
#' @param x character vector of length 1 (path to \code{.do} file) or more
#' (a set of stata commands). See examples.
#' @examples
#' ## Single command
#' stata("help regress")
#' ## Many commands
#' do <- c( "set obs 200",
#'          "gen a = 1"   )
#' stata(do)
#' ## External file
#' stata("foo.do")
#' @export
stata <- function(x)
{
  ## script support
  if (length(x)==1 && file.exists(x))
    x <- readLines(x)

  ## connection management
  on.exit(close(con, rdl))
  on.exit(unlink(fifoFile))

  ## connection creation
  fifoFile <- "RStata.fifo"
  con <- fifo(fifoFile, "w+")
  rdl <- pipe(paste(Sys.which("stata"), "-q", "do", fifoFile))

  ## write to and read from stata
  writeLines(c(x, "exit, clear STATA"), con)
  cat(readLines(rdl), sep = "\n")
}
