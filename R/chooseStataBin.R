#' Set Stata binary path
#'
#' Set Stata binary (among found alternatives) path. These settings are
#' lost when R is closed, therefore you should consider adding a
#' \code{options("RStata.StataPath")} line in your \code{.Rprofile}.
#' 
#' @export
chooseStataBin <- function()
{
    OS <- Sys.info()["sysname"]
    OS.type <- .Platform$OS.type
  
    ## ------------------------------
    if (OS %in% "Linux") {
        m <- c(`Stata MP` = "stata-mp",
               `Stata SE` = "stata-se",
               `Stata IC` = "stata",
               `Small Stata` = "stata-sm" )
        
        bin <- Sys.which(m)
        names(bin) <- names(m)
        nApps <- length(availProg <- bin[ "" != bin])
    
        if (0 == nApps) {
            stop("No application (detected) availables.\n",
                 "Set options('RStata.StataPath'), instead." )

        } else if (1 == nApps) {
            
            cat("Only", names(availProg), "found; I'll use it.\n")
            unnprog <- unname(availProg)
            options(RStata.StataPath = unnprog)
            return(unnprog)

        } else if (nApps > 1) {

            if (!interactive())
                stop("Cannot choose a Stata bin path non-interactively.\n",
                     "Set options('RStata.StataPath'), instead.")
            res <- utils::menu(names(availProg), title = "Stata availables")
            if (res > 0L) {
                unnprog <- unname(availProg[res])
                options(RStata.StataPath = unnprog)
                return(unnprog)
            }

        } else {
            stop("Unexpected error")
        }
        ## ------------------------------
    } else if (OS %in% "Windows"){
        prog <- file.choose()
        prog <- tools::file_path_sans_ext(prog)
        options(RStata.StataPath = prog)
        return(prog)
    } else {
        ""
    }
    
}
