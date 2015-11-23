LogVar <- function(var, nameOf=""){
  switch(class(var),
         "data.frame" = LogDF(var, nameOf),
         "list" = LogList(var, nameOf),
         "character" = LogCharacter(var, nameOf),
         "numeric" = LogNum(var, nameOf),
         ... = LogGeneral(var, nameOf))
}

LogDF <- function(var, nameOf="") {
  print(paste("Logging: ",nameOf))
  print(paste("Class is: ",class(var)))
  print(paste(nameOf, "has ",nrow(var), " rows and ", ncol(var), " columns"))
  print("Head (n=5) is:")
  head(var, n=5)
}

LogList <- function(var, nameOf="") {
  print(paste("Logging: ",nameOf))
  print(paste("Class is: ",class(var)))
  print("Structure of list (n=5) is: ")
  str(var, list.len = 5)
}

LogNum <- function(var, nameOf) {
  print(paste("Logging: ",nameOf))
  print(paste("Class is: ",class(var)))
  print(paste("Value is: ",var))
  print(paste("it is ", length(var), " long"))
}

LogGeneral <- function(var, nameOf) {
  print(paste("General logging function, specific logging not implemented for class ", class(var)))
  print(paste("Logging: ",nameOf))
  print(paste("Class is: ",class(var)))
  print(paste("Value is: ",var))
}
