vcsFile <- "jobs/CD-info-vcs.log"

vcsEvents<-read.csv(vcsFile,sep="\t", quote="", row.names = NULL, stringsAsFactors = FALSE)
if(nrow(vcsEvents) == 0) {
  print(sprintf("Error: no vcs log file found: %s", vcsFile))
}
vcsEvents$date <- as.Date(vcsEvents$date)
vcsEvents$branch <- as.character(vcsEvents$branch)

parseAction <- function(event) {
  desc <- event$desc
  branch <- event$branch
  parents <- as.vector(unlist(event$parents))

  #FEATURE_START
  m <- regexec("hg flow, add branch `feature/(.*)`.", desc)
  if(length(m[[1]])>1) {
    return("FEATURE_START")
  }

  m <- regexec("flow: Created branch 'feature/(.*)'.", desc)
  if(length(m[[1]])>1) {
    return("FEATURE_START")
  }

  #FEATURE_FINISH
  m <- regexec("hg flow, close feature (.*)", desc)
  if(length(m[[1]])>1) {
    return("FEATURE_FINISH")
  }

  m <- regexec("flow: Closed <feature> '(.*)'", desc)
  if(length(m[[1]])>1) {
    return("FEATURE_FINISH")
  }

  #FEATURE_CLOSE Closed branch feature/Vapautus_vesihuoltolaki
  m <- regexec("Closed branch feature/(.*)", desc)
  if(length(m[[1]])>1) {
    return("FEATURE_CLOSE")
  }

  #DEPLOY_TO_PROD
  m <- regexec("hg flow, close release release/.*", desc)
  if(m != -1) {
    return("DEPLOY_TO_PROD")
  }
  m <- regexec("flow: Closed <release> '.*'\\.", desc)
  if(m != -1) {
    return("DEPLOY_TO_PROD")
  }

  #COMMIT_TO_FEATURE
  if(length(branch) > 0 & grepl("^feature/.*", branch) & desc != "Merge with develop" & !grepl("(^flow:|^hg flow,)", desc)) {
    return("COMMIT_TO_FEATURE")
  }

  #MERGE_WITH_DEVELOP
  if(length(branch) > 0 & grepl("^release/.*", branch) & length(parents) == 2) {
    p1 <- vcs[vcs$rev == event$parents[1],]
    p2 <- vcs[vcs$rev == event$parents[2],]
    if(nrow(p1) == 1 & nrow(p2) == 1) {
      if( (grepl("^release/.*", p1$branch) | p1$branch == "develop") & (grepl("^release/.*", p2$branch) | p2$branch == "develop")) {
        return("MERGE_WITH_DEVELOP")
      }
    }
  }

  #RELEASE_START
  m <- regexec("hg flow, add branch `release/(.*)`.", desc)
  if(length(m[[1]])>1) {
    return("RELEASE_START")
  }

  m <- regexec("flow: Created branch 'release/(.*)'.", desc)
  if(length(m[[1]])>1) {
    return("RELEASE_START")
  }

  return("")
}

parseFeatureName <- function(event) {
  s <- as.character(event["branch"])
  m <- regmatches(s,regexec("feature/(.*)", s))[[1]]
  if(length(m)>1) {
    return(m[2])
  }
  return("")
}

parseRev <- function(str) {
  if(is.na(str) | str == "") {
    return(NULL)
  }
  st <- strsplit(as.character(str), " ")
  return(sapply(unlist(st), FUN = function(s) { return(regmatches(s,regexec("(.*):.*", s))[[1]][2])}))
}

vcs <- vcsEvents

vcs$children <- apply(vcs, 1, function(d) { return(unlist(parseRev(d["children"]))) })
vcs$parents <- apply(vcs, 1, function(d) { return(unlist(parseRev(d["parents"]))) })
vcs$action <- apply(vcs, 1, parseAction)
vcs$featureName <- apply(vcs, 1, parseFeatureName)


rr <<- NA
parseRow <- function(row) {
  rr <<- row
  list(datetime = as.character(row["date"]), 
  id = as.character(row["featureName"]),
  action = as.character(row["action"]))
}

rows <- apply(vcs, 1, parseRow)

t <- do.call(rbind.data.frame, rows)
df <- as.data.frame(t)

formatRow <- function(row) {
  row$state <- ""
  if(is.na(row$action) | row$action == "") {
    return(NULL)
  }
  if(row$action == "FEATURE_START") {
    row$action = "STATE_CHANGE"
    row$state = "DEV_ONGOING"
  }
  if(row$action == "FEATURE_FINISH") {
    row$action = "STATE_CHANGE"
    row$state = "DEV_DONE"
  }
  if(row$action == "FEATURE_CLOSE") {
    row$action = "STATE_CHANGE"
    row$state = "DEV_ABORTED"
  }
  return(row)
}

isRevFoundInChildren <- function(vcs, revToFind, rev) {
  vcsRevToFind <- vcs[vcs$rev == revToFind,]
  vcsRev <- vcs[vcs$rev == rev,]
  children <- vcsRev$children[[1]]
  if(is.null(children) | revToFind < rev) {
    return(F)
  }
  if(revToFind %in% children) {
    return(T)
  } else {
    for(childRev in children) {
      if(isRevFoundInChildren(vcs, revToFind, childRev)) {
        return(T)
      }
    }
    return(F)
  }
}

getFeatureDeployToProdDate <- function(vcs, featureName) {
  featureFinishChangeSet <- vcs[vcs$featureName == featureName & vcs$action == "FEATURE_FINISH",]
  if(!nrow(featureFinishChangeSet)) {
    return(NA)
  }

  futureDeployments <- vcs[vcs$rev > featureFinishChangeSet$rev & vcs$action == "DEPLOY_TO_PROD",]
  futureDeployments <- futureDeployments[with(futureDeployments, order(date, decreasing = F)), ]

  if(nrow(futureDeployments) == 0) {
    return(NA)
  }

  for(deployedToProdRev in futureDeployments$rev) {
    if(isRevFoundInChildren(vcs, deployedToProdRev, featureFinishChangeSet$rev)) {
      deployedToProdVcs <- vcs[vcs$rev == deployedToProdRev,]
      return(deployedToProdVcs$date)
    }
  }

  return(NA)
}

getFeatureDate <- function(vcs, featureName, action) {
  d <- vcs[vcs$featureName == featureName & vcs$action == action,]$date
  if(length(d)>0) {
    return(d)
  } else {
    return(NA)
  }
}

countWorkingDays <- function(date1, date2) {
  sum(!weekdays(seq(date1, date2, "days")) %in% c("lauantai", "sunnuntai", "saturday", "sunday")) - 1
}


features <- as.data.frame(unique(vcs[vcs$featureName != "",]$featureName))
colnames(features) <- "featureName"

features$deploymentDatetime <- as.Date(as.numeric(sapply(features$featureName, function(featureName) {getFeatureDeployToProdDate(vcs, featureName)})), origin = "1970-01-01")
features$featureStartDate <- as.Date(as.numeric(apply(features, 1, function(d) { return(getFeatureDate(vcs, d["featureName"], "FEATURE_START"))})), origin = "1970-01-01")
features$featureFinishDate <- as.Date(as.numeric(apply(features, 1, function(d) { return(getFeatureDate(vcs, d["featureName"], "FEATURE_FINISH"))})), origin = "1970-01-01")
features <- features[!is.na(features$featureStartDate),]

countStats <- function(compareDate, features) {
  compareDate <- as.Date(compareDate)
  notDeployed <- features[!is.na(features$featureStartDate) & !is.na(features$featureFinishDate) & features$featureFinishDate <= compareDate & (is.na(features$deploymentDatetime) | features$deploymentDatetime > compareDate),]
  notDeployed <- notDeployed[with(notDeployed, order(featureFinishDate, decreasing = F)), ]
  oldestDoneFeatureDays <- NA
  oldestDoneFeatureWorkingDays <- NA
  if(nrow(notDeployed) > 0) {
    f <- notDeployed[1,]
    oldestDoneFeatureDays <- as.numeric(compareDate - f$featureFinishDate)
    oldestDoneFeatureWorkingDays <- countWorkingDays(f$featureFinishDate, compareDate)
#    print(sprintf("%s: oldest done feature '%s' was done %d days ago (%d working days), total number of features done: %d.", compareDate, f$featureName, oldestDoneFeatureDays, oldestDoneFeatureWorkingDays, nrow(notDeployed)))
  } else {
#    print(sprintf("All done features deployed."))
  }
  list(compareDate = compareDate, nDoneFeatureDays = nrow(notDeployed), oldestDoneFeatureDays = oldestDoneFeatureDays, oldestDoneFeatureWorkingDays = oldestDoneFeatureWorkingDays)
}

stats <- countStats(Sys.Date(), features)

stats$oldestDoneFeatureDays
