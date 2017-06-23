#'Use clean fuzzy strings by using approximated string matching
#'
#'@param vector Vector containing strings to clean
#'@param distance Distance between strings to match.
#'Can take a ratio or a whole number.
#'May also take a list as an argument as per agrep().
#'@param iterations Number of itereations the function will do. A smart idea is to tune the function with low distance and high iterations.
#'
#'@examples
#' df$Vector2 <- fuzzyClean(df$Vector1, 0.25, 5)
#'@export fuzzyClean

fuzzyClean <- function(vector, distance, iterations){

  if(missing(iterations)){
    iterations = 1
  }
  if(missing(distance)){
    distance = 0.2
  }

  dat <- vector
  #dat <- gsub(" ", "", vector)
  courses <- unique(dat)
  while(iterations > 0){
    for(i in seq_along(courses)){
      dat[
        agrep(courses[i], dat,
              max.distance = distance)] <- courses[i]
    }

    dat <- gsub("and", "\\&", dat, ignore.case = T)
    dat <- gsub("\\&", " \\& ", dat)
    dat <- gsub("\\,", " \\, ", dat)
    dat <- gsub(" \\,", "\\, ", dat)
    dat <- gsub("of", " of", dat)
    dat <- gsub("to", " to", dat)
    dat <- gsub(" with", " with", dat)
    dat <- gsub("mgmt", "Management", dat, ignore.case = T)
    dat <- gsub("fund\\b", "Fundamentals", dat, ignore.case = T)
    dat <- gsub("occ\\b", "Occupational", dat, ignore.case = T)
    dat <- gsub("mgmnt", "Management", dat, ignore.case = T)
    dat <- gsub("Programs", "", dat, ignore.case = T)

    iterations <- iterations - 1
  }
  # dat <- gsub("([[:lower:]])([[:upper:]])",
  #                       "\\1 \\2",
  #             dat)
  dat <- trimws(dat, "right")
  courses <- unique(dat)

  return(dat)
}



