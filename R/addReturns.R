stata_return_code <- function(SRC) { 
  
out <- c(SRC,"local est_name \"Active\"

***Rreturns***
  local returns : r(scalars)
putexcel set ReturnsToR.xls, sheet(\"`est_name'_r_scalars\") modify
local i = 1
foreach e in `returns'{
	cap confirm scalar r(`e')
if !_rc{
  
  putexcel A`i' = \"`e'\"
		putexcel B`i' = `r(`e')'
		local ++i
} 

}

local returns : r(matrices)
foreach e in `returns' { 
	putexcel set ReturnsToR.xls, sheet(\"`est_name'_r_matrix_`e'\") modify
	putexcel A1 = matrix(r(`e')'), names
}

***Ereturns***

local returns : e(scalars)
putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_scalars\") modify
local i = 1
foreach e in `returns'{
	cap confirm scalar e(`e')
	if !_rc{

		putexcel A`i' = \"`e'\"
		putexcel B`i' = `e(`e')'
		local ++i
		} 

}

local returns : e(macros)
putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_macros\") modify
local i = 1
foreach e in `returns'{
		putexcel A`i' = \"`e'\"
putexcel B`i' = \"`e(`e')'\"

	local ++i
}


local returns : e(matrices)
 
foreach e in `returns' { 
	putexcel set ReturnsToR.xls, sheet(\"`est_name'_e_matrix_`e'\") modify
	putexcel A1 = matrix(e(`e')'), names
}


************************Stored Returns************************************
estimates dir
foreach n in `r(names)'{
  
  estimates restore `n'
	
	local returns : r(scalars)
	putexcel set ReturnsToR.xls, sheet(\"`n'_r_scalars\") modify
	local i = 1
	foreach e in `returns'{
		cap confirm scalar r(`e')
		if !_rc{

			putexcel A`i' = \"`e'\"
			putexcel B`i' = `r(`e')'
			local ++i
} 

}



local returns : r(matrices)
foreach e in `returns' { 
		putexcel set ReturnsToR.xls, sheet(\"`n'_r_matrix_`e'\") modify
		putexcel A1 = matrix(r(`e')'), names
	}

	***Ereturns***

	local returns : e(scalars)
	putexcel set ReturnsToR.xls, sheet(\"`n'_e_scalars\") modify
	local i = 1
	foreach e in `returns'{
		cap confirm scalar e(`e')
		if !_rc{

			putexcel A`i' = \"`e'\"
			putexcel B`i' = `e(`e')'
			local ++i
			} 

}

local returns : e(macros)
putexcel set ReturnsToR.xls, sheet(\"`n'_e_macros\") modify
local i = 1
foreach e in `returns'{
			putexcel A`i' = \"`e'\"
putexcel B`i' = \"`e(`e')'\"

		local ++i
	}


	local returns : e(matrices)
	 
	foreach e in `returns' { 
		putexcel set ReturnsToR.xls, sheet(\"`n'_e_matrix_`e'\") modify
		putexcel A1 = matrix(e(`e')'), names
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
      rnames <- insheet[,1] 
      rnames[is.na(rnames)] <- ''
      rownames(insheet) <- rnames
      insheet <- insheet[,-1, drop=F] 
      insheet <- as.matrix(insheet)
      
      out[[s_token[1]]][[s_token[2]]][[s_token[3]]][[matname]] <- insheet
    }
    
  }
  
  invisible(out)
}