

remove_transparency <- function(x){
  if(sum(nchar(x)) == 9 && grepl("FF$",x)){
    return(substring(x, 1, 7))
  }
  x
}


