PATH <- dirname(sys.frame(1)$ofile)
source(paste(PATH,"/vlookup.R",sep=""))

#-----------------------------------------------------
# SECTION 1. PREPARE FILES
#-----------------------------------------------------

#READ LOOKUP FILES
lookup.a2.ae <- read.csv(paste(PATH,"/lookups/lookup.a2.ae.csv",sep=""), stringsAsFactors = FALSE)
lookup.as.ae <- read.csv(paste(PATH,"/lookups/lookup.a2.ae.csv",sep=""), stringsAsFactors = FALSE)
lookup.btec.ae <- read.csv(paste(PATH,"/lookups/lookup.btec.ae.csv",sep=""), stringsAsFactors = FALSE)

lookup.GCSE.to.ALPS.a2 <-  read.csv(paste(PATH,"/lookups/lookup.gcse.ucas.a2.csv",sep=""), stringsAsFactors = FALSE)
lookup.GCSE.to.ALPS.as <-  read.csv(paste(PATH,"/lookups/lookup.gcse.ucas.as.csv",sep=""), stringsAsFactors = FALSE)

lookup.subjects.code <- read.csv(paste(PATH,"/lookups/lookup.subjects.csv",sep=""), stringsAsFactors = FALSE)
lookup.a2.to.ucas <- read.csv(paste(PATH,"/lookups/lookup.a2.ucas.csv",sep=""), stringsAsFactors = FALSE)
lookup.as.to.ucas <- read.csv(paste(PATH,"/lookups/lookup.as.ucas.csv",sep=""), stringsAsFactors = FALSE)

vlookup.alps.a2 <- read.csv(paste(PATH,"/lookups/vlookup.alps.a2.csv",sep=""), stringsAsFactors = FALSE)
vlookup.alps.a2[,1] <- as.numeric(vlookup.alps.a2[,1])
vlookup.alps.as <- read.csv(paste(PATH,"/lookups/vlookup.alps.as.csv",sep=""), stringsAsFactors = FALSE)
vlookup.alps.as[,1] <- as.numeric(vlookup.alps.as[,1])

#-----------------------------------------------------
# SECTION 2. FUNCTIONS
#-----------------------------------------------------

# A*-E BREAKDOWNS USING BUCKETS GIVEN IN LOOKUP
# 
aeCounts <- function(category, grades, lookup) {
  #remove all null values
  include <- !is.na(grades) & grades != "" 
  cat.val <- category[include]
  grades.val <- grades[include]
  
  #convert to a matrix of [grade count] * [lookup buckets]
  grade.indices <- match(grades.val, lookup[,1])
  counts <- lookup[grade.indices,]
  counts <- counts[,2:length(counts[1,])]

  #unique list of categories
  cat.unique <- sort(unique(cat.val))
  grade.summary <- as.data.frame( sapply(cat.unique, 
                                         function (p) 
                                           colSums(counts[cat.val == p,])))
  
  return(t(grade.summary))
}

# A*-E BREAKDOWNS AS %AGE
# assumes that column 1 of lookup is a 'count' column
aePercentages <- function(category, grades, lookup) {
  counts <- aeCounts(category, grades, lookup)
  percentages <- counts[,2:length(counts[1,])]/counts[,1]
  
  return(percentages)
}


# VALUE ADDED AND ALPS SCORES

Value.Added.Using.GCSE  <- function(category, grades, gcse.aps, grade.to.points.table, gcseAPS.to.points.table) {
  #1. REMOVE DATA THAT SHOULD NOT BE INCLUDED
  include <- !is.na(grades) & grades != "" & !is.na(gcse.aps) & !(gcse.aps == '') & !(gcse.aps == 0) & !(grades == 'X')
  grades.VA <- grades[include]
  category.VA <- category[include]
  GCSE.APS.VA <- gcse.aps[include]
  
  #2. CALCULATE V.A. VECTOR
  Result.Points <- grade.to.points.table[match(grades.VA, grade.to.points.table[,1]),2]
  Target.Points <- vlookup(GCSE.APS.VA, gcseAPS.to.points.table, 2, TRUE)
  Value.Added.Points <- Result.Points - Target.Points
  
  #3. SUMMARIZE
  category.unique <- unique(category.VA)
  category.unique <- sort(category.unique)
  Value.Added.average <- sapply(category.unique, 
                        function(p) mean(Value.Added.Points[category.VA == p]))
  
  return(data.frame(va = Value.Added.average))
}

ALPS.scores.A2 <- function(category, grades, gcseAPS) {
  
  value <- Value.Added.Using.GCSE(category = category, grades = grades, gcse.aps = gcseAPS, 
                                  grade.to.points.table = lookup.a2.to.ucas, 
                                  gcseAPS.to.points.table = lookup.GCSE.to.ALPS.a2)
  value$va <- (100 + value$va) / 100
  
  return(value)
}

ALPS.scores.AS <- function(category, grades, gcseAPS) {
  value <- Value.Added.Using.GCSE(category = category, grades = grades, gcse.aps = gcseAPS, 
                                  grade.to.points.table = lookup.as.to.ucas, 
                                  gcseAPS.to.points.table = lookup.GCSE.to.ALPS.as)
  value$va <- (50 + value$va) / 50
  
  return(value)  
}


#ALPS LEVEL CONVERSION
# functions equally well for A2 & AS
ALPS.scores.to.grades <- function(scores, subject.codes, alps.vlookup) {
  return (sapply(1:length(scores), ALPS.score.to.grade.single.subject, scores = scores, 
                 subject.code = subject.codes, alps.vlookup = alps.vlookup))
}

ALPS.score.to.grade.single.subject <- function(index, scores, subject.code, alps.vlookup) {
  score = scores[index]
  
  cut.vlookup <- alps.vlookup[alps.vlookup$ALPS.Subject == subject.code[index],]
  return (vlookup(score, cut.vlookup, 3, TRUE))
}


# SUMMARY FUNCTIONS

# used for mixed calculations using A2 general ALPS
dashboard.for.general.a2 <- function(category, grades, gcse.aps) {
  counts <- aeCounts(category, grades, lookup.a2.ae)
  percentages <- aePercentages(category, grades, lookup.a2.ae)
  
  scores <- ALPS.scores.A2(category, grades, gcse.aps)
  grades <- ALPS.scores.to.grades(scores[,1], rep("General", length(scores[,1])), vlookup.alps.a2)
  
  return( cbind(counts[,1],percentages,grades))
}

# used for mixed calculations using AS general ALPS
dashboard.for.general.as <- function(category, grades, gcse.aps) {
  counts <- aeCounts(category, grades, lookup.as.ae)
  percentages <- aePercentages(category, grades, lookup.as.ae)
  
  scores <- ALPS.scores.AS(category, grades, gcse.aps)
  grades <- ALPS.scores.to.grades(scores[,1], rep("General", length(scores[,1])), vlookup.alps.as)
  
  return( cbind(counts[,1],percentages,grades))
}


# used for single subject calculations using A2 subject specific ALPS
dashboard.for.subjects.a2 <- function(category, grades, subjects, gcse.aps) {
  #SIMPLE START GET THE COUNTS & PERCENTAGES
  counts <- aeCounts(category, grades, lookup.a2.ae)
  percentages <- aePercentages(category, grades, lookup.a2.ae)
  
  #CALCULATE THE ALPS SCORES
  scores <- ALPS.scores.A2(category, grades, gcse.aps)
  
  #FIND THE APPROPRIATE CATEGORIES AND 
  category.unique <- row.names(scores)
  category.unique.subject <- subject.for.category.list(category.unique, category, subjects)
  category.unique.code <- lookup.subjects.code$ALPS.Code[match(category.unique.subject, 
                                                    lookup.subjects.code$LaSwap.Subject)]
  
  #CONVERT TO GRADES
  grades <- ALPS.scores.to.grades(scores$va, category.unique.code, vlookup.alps.a2)
  
  #COMBINE AND RETURN
  return( cbind(counts[,1],percentages,grades))
}

# used for single subject calculations using A2 subject specific ALPS
dashboard.for.subjects.as <- function(category, grades, subjects, gcse.aps) {
  #SIMPLE START GET THE COUNTS & PERCENTAGES
  counts <- aeCounts(category, grades, lookup.as.ae)
  percentages <- aePercentages(category, grades, lookup.as.ae)
  
  #CALCULATE THE ALPS SCORES
  scores <- ALPS.scores.AS(category, grades, gcse.aps)
  
  #FIND THE APPROPRIATE CATEGORIES AND 
  category.unique <- row.names(scores)
  category.unique.subject <- subject.for.category.list(category.unique, category, subjects)
  category.unique.code <- lookup.subjects.code$ALPS.Code[match(category.unique.subject, 
                                                               lookup.subjects.code$LaSwap.Subject)]
  
  #CONVERT TO GRADES
  grades <- ALPS.scores.to.grades(scores$va, category.unique.code, vlookup.alps.as)
  
  #COMBINE AND RETURN
  return( cbind(counts[,1],percentages,grades))
}


# helper for dashboard.for.subjects functions
subject.for.category.list <- function(category.list, category.all, subject.all) {
  return( sapply(category.list, function(p) subject.all[category.all %in% p][1]) )
}