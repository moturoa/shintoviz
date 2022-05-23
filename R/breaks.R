my_breaks_pretty <- function(x){
  function(x) unique(floor(pretty(x)))
}

my_breaks_pretty2 <- function(x, allowed){
  function(x) intersect(unique(floor(pretty(x))), allowed)
}

my_breaks_pretty3 <- function(x, allowed){
  function(x){

    a <- unique(allowed)
    if(length(a) < 20){
      a
    } else {
      unique(floor(pretty(x)))
    }

  }
}
