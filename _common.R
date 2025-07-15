### ---- Set knitr options ----

# set knitr options
knitr::opts_chunk$set(
  cache = FALSE, # cache output
  error = TRUE, # continue code evaluation even if there is an error, in which case the error messages will print
  comment = '', # remove ## prefix for printed text output
  message = FALSE, # suppress printing of messages in the output document
  warning = FALSE, # suppress printing of warnings in the output document
  fig.pos = "hold" # hold figure position so it stays in the order it should be in relative to the surrounding code / text
)

# disable scientific notation for readability purposes
options(scipen = 999)

### ---- Load packages and define functions ----

# load packages for common functions
library(tidyverse)
library(magrittr)
library(kableExtra)
library(broom)

# default function to display dataframes nicely
# options -> vector of column names (gets processed by kable(), so can be latex), number of rows to display, and rounding digits
# -> needed because formatting of raw dataframe is bad when output in markdown
# -> nesting functions instead of piping so doesn't require magrittr
display_nice <- function(df, col.names = NA, nrow = 10, digits = 3) {
  
  # set columns names to the given vector or keep names or original df
  if (identical(col.names, NA)) {
    col.names = colnames(df)
  }
  
  # convert to knitr_kable and style
  # -> always want html format, left aligned with not full width
  # -> table.attr -> have to tell quarto to not process the table (https://github.com/quarto-dev/quarto-cli/issues/5737)
  kable_styling(kable(head(df, n = nrow),
                      col.names = col.names,
                      format = "html",
                      digits = digits,
                      table.attr = 'data-quarto-disable-processing="true"'),
                bootstrap_options = "striped",
                full_width = FALSE,
                position = "left")
}

# set global plot theme
theme_set(theme_bw())

# function to compare two objects
# -> using it to compare results of same thing from different methods
# -> works for all data types (numeric vectors, character / factor vectors, dataframes with same dimensions, lists of anything)
# default options -> return original items (for easy visual comparison in same output), tolerance in numeric comparison is 1 x 10^-5 (out to 5 decimals, 0.00001), check.names = FALSE to only check values and NOT the names of elements
# return values -> list of 1) summary of comparison, 2 and 3) original items if desired
# -> 1) includes logical of overall comparison and if NOT EQUAL description of differences + element-wise comparison of vectors or columns of dataframe
compare <- function(item_1, item_2, return_items = TRUE, tolerance = 0.00001, check.names = FALSE) {
  
  # initialize a list with helpful names for the results and original items being compared
  # -> the extra functions for the names just return the name / call of the original arguments passed to item_1 and item_2
  results = vector(mode = "list", length = 3)
  names(results) = c("comparison", deparse(substitute(item_1)), deparse(substitute(item_2)))
  
  # check equality with all.equal()
  # -> returns TRUE when TRUE and a character string describing the difference when FALSE
  # -> allows for a tolerance in numeric comparisons, which is ideal to account for rounding
  # --> AND includes option to ignore names (so just focus comparison on values of elements)
  comparison = all.equal(item_1, item_2, tolerance = tolerance, check.names = check.names)
  
  # check class of item 1 (assuming item 2 is the same)
  # check if numeric 
  
  # if equal (with tolerance), returns TRUE comparison
  #-> by default, it is only looking at the values and NOT the names
  # -> check by seeing if comparison is TRUE, if so return TRUE
  if (identical(comparison, TRUE)) {
    
    results[[1]] = comparison
    
  }
  
  # if not equal, return a list with items describing comparison 
  else {
    
    # fill results with elements for:
    # -> overall comparison result = FALSE
    # -> description of inequalities = output of all.equal when not TRUE
    # -> conditionally return vector of element wise comparison (with names removed) based on class of items
    # --> throws warning message when lengths differ, but still does comparison
    results[[1]] = list(
      "result" = identical(comparison, TRUE),
      "description" = comparison,
      "element-wise" = 
        
        # if numeric, compare items after rounding
        if (identical(class(item_1), "numeric")) {
          
          # calculate number of decimals to round to before element-wise comparison as a function of the tolerance, 
          # -> e.g) tolerance = 0.00001 --> abs(log10(tolerance)) = 5
          # -> ceiling() accounts for non base 10 tolerances so still have a whole number of digits for rounding
          decimals = ceiling(abs(log10(tolerance)))
          round(as.numeric(item_1), decimals) == round(as.numeric(item_2), decimals)
          
          
        }
      
      # first if character or factor, simply compare
      else if (identical(class(item_1), "character") | identical(class(item_1), "factor")) {
        
        # convert to character to take into account factors
        as.character(item_1) == as.character(item_2)
        
        
      }
      
      # if dataframe, correctly compare columns after checking column data type
      # different comparison to also capture tibbles
      else if (sum(class(item_1) == "data.frame") > 0) {
        
        # convert to dataframes so element-wise comparisons work as desired
        tmp_item_1 = data.frame(item_1)
        tmp_item_2 = data.frame(item_2)
        
        decimals = ceiling(abs(log10(tolerance)))
        
        tmp = sapply(1:ncol(tmp_item_1), function(i) {
          
          if(identical(class(tmp_item_1[, i]), "numeric")) {
            
            # compare numerics after rounding
            round(as.numeric(tmp_item_1[, i]), decimals) == round(as.numeric(tmp_item_2[, i]), decimals)
            
          } else {
            
            # compare factors or characters
            as.character(tmp_item_1[, i]) == as.character(tmp_item_2[, i])
            
          }
          
        })
        
        # give result names of first item
        colnames(tmp) = colnames(item_1)
        tmp
        
      }
      
      # if other data type, do not do element-wise comparison
      else {
        
        NULL
        
      })
  }
  
  # add the original items being compared to the results for display
  results[[2]] = item_1
  results[[3]] = item_2
  
  # return results with conditional elements
  # return comparison and original items
  if (return_items) {
    
    return(results)
    
  }
  
  # just return the comparison by dropping original items
  else {
    
    return(results[-c(2:3)])
    
  }
  
}