stata_return_code <- function(SRC) { 
  
out <- c(SRC,"local est_name \"Active\"

***Rreturns***
  local returns : r(scalars)
version 13: putexcel set ReturnsToR.xls, sheet(\"`est_name'_r_scalars\") modify
local i = 1
foreach e in `returns'{
	cap confirm scalar r(`e')
if !_rc{
  
  version 13: putexcel A`i' = (\"`e'\")
		version 13: putexcel B`i' = (`r(`e')')
		local ++i
} 

}

local returns : r(matrices)
foreach e in `returns' { 
	version 13: putexcel set ReturnsToR.xls, sheet(\"`est_name'_r_matrix_`e'\") modify
	version 13: putexcel A1 = matrix(r(`e')', names)
}

***Ereturns***

local returns : e(scalars)
version 13: putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_scalars\") modify
local i = 1
foreach e in `returns'{
	cap confirm scalar e(`e')
	if !_rc{

		version 13: putexcel A`i' = (\"`e'\")
		version 13: putexcel B`i' = (`e(`e')')
		local ++i
		} 

}

local returns : e(macros)
version 13: putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_macros\") modify
local i = 1
foreach e in `returns'{
		version 13: putexcel A`i' = (\"`e'\")
version 13: putexcel B`i' = (\"`e(`e')'\")

	local ++i
}


local returns : e(matrices)
 
foreach e in `returns' { 
	version 13: putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_matrix_`e'\") modify
	version 13: putexcel A1 = matrix(e(`e')', names)
}


************************Stored Returns************************************
estimates dir
foreach n in `r(names)'{
  
  estimates restore `n'
	
	local returns : r(scalars)
	version 13: putexcel set ReturnsToR.xls, sheet(\"`n'_r_scalars\") modify
	local i = 1
	foreach e in `returns'{
		cap confirm scalar r(`e')
		if !_rc{

			version 13: putexcel A`i' = (\"`e'\")
			version 13: putexcel B`i' = (`r(`e')')
			local ++i
} 

}



local returns : r(matrices)
foreach e in `returns' { 
		version 13: putexcel set ReturnsToR.xls, sheet(\"`n'_r_matrix_`e'\") modify
		version 13: putexcel A1 = matrix(r(`e')', names)
	}

	***Ereturns***

	local returns : e(scalars)
	version 13: putexcel set ReturnsToR.xls, sheet(\"`n'_e_scalars\") modify
	local i = 1
	foreach e in `returns'{
		cap confirm scalar e(`e')
		if !_rc{

			version 13: putexcel A`i' = (\"`e'\")
			version 13: putexcel B`i' = (`e(`e')')
			local ++i
			} 

}

local returns : e(macros)
version 13: putexcel set ReturnsToR.xls, sheet(\"`n'_e_macros\") modify
local i = 1
foreach e in `returns'{
			version 13: putexcel A`i' = (\"`e'\")
version 13: putexcel B`i' = (\"`e(`e')'\")

		local ++i
	}


	local returns : e(matrices)
	 
	foreach e in `returns' { 
		version 13: putexcel set ReturnsToR.xls, sheet(\"`n'_e_matrix_`e'\") modify
		version 13: putexcel A1 = matrix(e(`e')', names)
	}

}"
)

out <- unlist(lapply(out, strsplit, '\n'))
return(out)
}

get_stata_returns <- function(file){  
  library(stringr, quietly = T)

  sheets <- readxl::excel_sheets(file)
  
  out <- list()
  
  for (s in sheets){
    s_token <- unlist(strsplit(s, '_'))

    if( s_token[3] %in% c('scalars', 'macros')){    
      insheet <- suppressMessages(readxl::read_excel(file, sheet = s, col_names = F))

      scalars <- insheet$...2
      names(scalars) <- insheet$...1
      out[[s_token[1]]][[s_token[2]]][[s_token[3]]] <- scalars
    } 
    else {
      insheet <- suppressMessages(readxl::read_excel(file, sheet = s))
      
      matname <- paste0(s_token[4:length(s_token)], collapse = '_')
      insheet <- as.data.frame(insheet)
      

      while (all(grepl('[A-Z, a-z]', insheet[1,])|is.na(insheet[1,]))){
        colnames(insheet) <- paste(colnames(insheet), insheet[1,], sep = '_')
        insheet <- insheet[-1,]

      }
      
      while (all(grepl('[A-Z, a-z]', insheet[[2]])|is.na(insheet[[2]]))){
        insheet[,1] <- do.call(paste, c(insheet[1:2], sep='_'))
        insheet <- insheet[,-2]
        
      }
      
   
      
      
      rnames <- insheet[[1]] 
      rnames[is.na(rnames)] <- ''
      rownames(insheet) <- rnames
      insheet <- insheet[,-1, drop=F] 
      
      for (c in 1:ncol(insheet)){
        insheet[[c]] <- as.numeric(insheet[[c]])
      }
      
      
      insheet <- as.matrix(insheet) 
      
      out[[s_token[1]]][[s_token[2]]][[s_token[3]]][[matname]] <- insheet
    }
    
  }
  
  invisible(out)
}