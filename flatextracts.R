
# The following two functions are copy pasted from Stack Overflow.
# Future me: You're welcome.  

starts_with <- function(vars, match, ignore.case = TRUE) {
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)
  
  if (ignore.case) vars <- tolower(vars)
  substr(vars, 1, n) == match
}

ends_with <- function(vars, match, ignore.case = TRUE) {
  if (ignore.case) match <- tolower(match)
  n <- nchar(match)
  
  if (ignore.case) vars <- tolower(vars)
  length <- nchar(vars)
  
  substr(vars, pmax(1, length - n + 1), length) == match
}

strip_empties <- function(vect) {
  return(
    Filter(
      function(y) !is.na(y),
      Filter(function(x) x != "", vect)
    )
    )
}

subrecord_field_values <- function (data, subrecord_name, field_name){
  #
  # subrecord_field_values(data, "Diagnosis", "Condition)
  #
  # Returns a vector of combined Diagnosis.1 Diagnosis.2 etc values
  # Strips empty or NA values
  #
  column_names <- as.data.frame(names(data))
  names(column_names) <- c("col")
  column_names <- as.data.frame(column_names[starts_with(column_names$col, subrecord_name),])
  names(column_names) <- c("col")
  column_names <- as.vector(column_names[ends_with(column_names$col, field_name),])

  getcolumn <- function(name) data[[name]]
  
  values <- sapply(
              Map(as.vector, 
                    Map(getcolumn, column_names)
                ),
              c
            )
              
  without.empties <- strip_empties(values)
  return(without.empties)
}

subrecord_field_value_counts <- function(data, subrecord_name, field_name){
  #
  # subrecord_field_value_caounts(data, "Travel", "Destination")
  #
  # Returns a data frame of the count of field values, having cleaned for
  # case sensitivity
  #
  values <- subrecord_field_values(data, subrecord_name, field_name)
  values <- sapply(values, tolower)
  counts <- as.data.frame(table(values))
  names(counts) <- c(field_name, "Frequency")
  return(counts)
}