
getTable.log.fun <- function(startdate, enddate) {
  
  Sys.sleep(5)
  
  startdate <- anydate(startdate)
  enddate <- anydate(enddate)
  
  if (class(startdate)=="Date" & class(enddate)=="Date" & !is.na(startdate) & !is.na(enddate)) {
    
    
    site <- paste("https://www.fangraphs.com/leaders.aspx?pos=all&stats=bat&lg=all&qual=0&type=8&season=2022&month=1000&season1=2022&ind=0&team=0%2Cts&rost=&age=&filter=&players=0&startdate=",
                  startdate,
                  "&enddate=",
                  enddate,
                  sep = "") %>%
      read_html()
    
    data <- html_table(site)[[17]][c(-1:-3),]
    colnames(data) <- html_table(site)[[17]][2,]
    return(data)
  }
  else {
    print("Please ensure date formats are entered as 'YYYY-MM-DD'")
    return()
  }
}


