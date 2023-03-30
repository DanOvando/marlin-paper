# simple function to make prettier labels
titler <- function(x){

  stringr::str_to_title(stringr::str_replace_all(x,"_"," "))

}
