


#function to make GEOID into a character vector with the leading zero for Alabama:
# This function was written for use if GEOID is an integer in the dataset.
# If GEOID isn't an integer, use with caution, especially if GEOID is a factor.
fix.geo <- function(df){
  df$GEOID <- as.character(df$GEOID)
  df$GEOID[which(nchar(df$GEOID) == 4)] <- paste0(0, df$GEOID[which(nchar(df$GEOID) == 4)])
  return(df$GEOID)
}



#THIS IS SOME EXAMPLE CODE TO GENERATE THE GEOID COLUMN:

#make the countyfp be 3 characters long
#  make countyfp into a character vector instead of a number:
climate$COUNTYFP <- as.character(climate$COUNTYFP)
#  if the countyfp number is only 2 character, add a zero:
climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 2)] <- paste0("0", climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 2)])
#  if the countyfp number is one character, add two zeros:
climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 1)] <- paste0("00", climate$COUNTYFP[which(nchar(climate$COUNTYFP) == 1)])


#make the statefp be two characters long
climate$STATEFP <- as.character(climate$STATEFP)
climate$STATEFP[which(nchar(climate$STATEFP) == 1)] <- paste0("0", climate$STATEFP[which(nchar(climate$STATEFP) == 1)])

#concatenate the state and county fp to make the GEOID
climate$GEOID <- paste0(climate$STATEFP, climate$COUNTYFP)
class(climate$GEOID)