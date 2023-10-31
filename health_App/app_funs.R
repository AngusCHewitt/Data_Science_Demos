## gend rand names 
random_names <- function(vector, setSeed) {
  
  # unique rand number gen for each patient id 
  set.seed(setSeed)
  
  if(vector == 0)
  {randomNames(1, gender = "Female")}
  else{randomNames(1, gender = "Male")}
}
