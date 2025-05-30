hist_binned_fun = function(df) {
  # Calculate histogram midpoints for the 'value' column within each nested dataframe
  hist_result <- hist(df$value, plot = FALSE)
  breaks <- hist_result$breaks
  
  # Add a new column 'mids' containing the histogram midpoints to each row in df
  df$breaks <- NA  #Initialize the column.  We will replace the values in the column.
  
  #Now loop through the values
  for(i in 1:nrow(df)){
    #Find the bin for each value
    bin_index <- which(hist_result$breaks[-length(hist_result$breaks)] <= df$value[i] & hist_result$breaks[-1] > df$value[i])
    # Assign the corresponding midpoint to each row
    if(length(bin_index) > 0){
      df$breaks[i] <- breaks[bin_index] #Add the midpoint to this row
    } else {
      #Handle cases where value falls outside the histogram range
      df$breaks[i] <- NA #or handle the case
    }
    
  }
  
  return(df)  # Return the modified dataframe
}


