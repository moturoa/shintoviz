


make_value_label <- function(values, label_function = NULL, label_k = FALSE, label_perc = FALSE){

  if(label_k == "auto"){
    label_k <- max(values) > 5000
  }

  if(is.null(label_function)){
    label_function <- format_n2
  } else {
    label_function <- base::get(label_function)
  }

  label_function(values, label_k, label_perc)

}


