# read vcs log events from a \tsv file
#
# execute CD-info-update-vcs-log.sh
#

vcsEvents<-read.csv("CD-info-vcs.log",sep="\t", quote="", row.names = NULL, stringsAsFactors = FALSE)
vcsEvents$date <- as.POSIXct(vcsEvents$date, format="%Y-%m-%d %H:%M:%S")
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
#vcs$date <- as.Date(as.character(vcs$date))

vcs$children <- apply(vcs, 1, function(d) { return(unlist(parseRev(d["children"]))) })
vcs$parents <- apply(vcs, 1, function(d) { return(unlist(parseRev(d["parents"]))) })
vcs$action <- apply(vcs, 1, parseAction)
vcs$featureName <- apply(vcs, 1, parseFeatureName)

df <- vcs[,c("date", "featureName", "action")]
df <- df[!is.na(df$featureName) & df$featureName != "",]
colnames(df) <- c("datetime", "id", "action")

df$state <- apply(df, 1, function(row) {
  action <- row["action"]
  if(is.null(action) | is.na(action) | action == "") {
    return("")
  }
  if(action == "FEATURE_START") {
    return("DEV_ONGOING")
  }
  if(action == "FEATURE_FINISH") {
    return("DEV_DONE")
  }
  if(action == "FEATURE_CLOSE") {
    return("DEV_ABORTED")
  }
  return("")
})
df$action <- apply(df, 1, function(row) {
  action <- row["action"]
  if(action == "FEATURE_START" | action == "FEATURE_FINISH" | action == "FEATURE_CLOSE") {
    return("STATE_CHANGE")
  } else {
    return(action)
  }
})

df <- df[!is.na(df$action) & df$action != "",]

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
      d <- as.character(deployedToProdVcs$date)
      return(d)
    }
  }
  return(NA)
}

features <- as.data.frame(unique(vcs[vcs$featureName != "",]$featureName))
colnames(features) <- "featureName"
features$featureName <- as.character(features$featureName)

features$deploymentDatetime <- apply(features, 1, function(feature) {getFeatureDeployToProdDate(vcs, feature["featureName"])})

featureDeployments <- features
colnames(featureDeployments) <- c("id", "datetime")
featureDeployments <- featureDeployments[!is.na(featureDeployments$datetime),]
featureDeployments$action = "STATE_CHANGE"
featureDeployments$state = "IN_PROD"

df <- rbind(df, featureDeployments)

jiraFeatures <- as.data.frame(unique(vcs[grepl("lpk-[0-9]+", vcs$featureName, ignore.case = TRUE),]$featureName))
colnames(jiraFeatures) <- "featureName"
jiraIds <- apply(jiraFeatures, 1, function(d) { 
  s <- d["featureName"]
  return(toupper(regmatches(s, regexec(".*(lpk-[0-9]+).*", s, ignore.case = TRUE))[[1]][2]))
})
jiraIds <- as.data.frame(unique(jiraIds))
write.table(jiraIds, file = "jira-issue-ids.txt", row.names = FALSE, sep = "\t", quote = FALSE)

#findIdByKey <- function(jiraEvents, df, Key) {
#  return(unique(df[grepl(Key, df$id),]$id)[1])
#}

# python ./analyze3.py target/output.xml ../jira-20150114.csv
jiraEvents<-read.csv("jira-20150114.csv", sep=",", row.names = NULL)
jiraEvents$id <- apply(jiraEvents, 1, function(d) { return(findIdByKey(jiraEvents, df, d["Key"]))})
jiraEvents$action <- jiraEvents$Action
jiraEvents$state <- jiraEvents$State
jiraEvents$datetime <- as.character(strptime(jiraEvents$Timestamp, "%d.%m.%Y %H:%M"))

jes <- jiraEvents[,c("datetime", "id", "action", "state")]
# no states from Jira
jes <- jes[jes$action != "STATE_CHANGE" | jes$state == "ISSUE_CREATED" | jes$state == "DEV_ONGOING" | jes$state == "DEV_DONE" | jes$state == "IN_PROD",]
#df <- rbind(df, jes)

#df$datetime <- strptime(df$datetime, "%Y-%m-%d %H:%M"
#vcs[grepl("unsubscr", vcs$featureName),]
#vcs[vcs$action == "MERGE_WITH_DEVELOP" | vcs$action == "RELEASE_START",]

# password="password"
# csvfile=LUPA-1647_unsubscribe-notifications-use.csv; curl -k -u lupapiste:$password https://splunk.address.here/servicesNS/nobody/search/search/jobs/export --data-urlencode search="search /api/command/unsubscribe-notifications" -d output_mode=csv > $csvfile; logfile='LUPA-1647_unsubscribe-notifications-use.tsv'; echo -e "datetime\tid\taction" > $logfile; cat $csvfile | sed -n -e 's/^[^,]*,\"\([^,]*\)EE.*,.*/\1\tLUPA-1647_unsubscribe-notifications\tUSED_IN_PROD/' -e '2,$'p >> $logfile
useEvents<-read.csv("LUPA-1647_unsubscribe-notifications-use.tsv",sep="\t", row.names = NULL)
useEvents$state <- ""
df <- rbind(df, useEvents)

# csvfile=LUPA-1537_Sopimuksen_allekirjoitus-use.csv; curl -k -u lupapiste:$password https://splunk.address.here/servicesNS/nobody/search/search/jobs/export --data-urlencode search="search /api/command/sign-verdict" -d output_mode=csv > $csvfile; logfile='LUPA-1537_Sopimuksen_allekirjoitus-use.tsv'; echo -e "datetime\tid\taction" > $logfile; cat $csvfile | sed -n -e 's/^[^,]*,\"\([^,]*\)EE.*,.*/\1\tLUPA-1537_Sopimuksen_allekirjoitus\tUSED_IN_PROD/' -e '2,$'p >> $logfile
useEvents<-read.csv("LUPA-1537_Sopimuksen_allekirjoitus-use.tsv",sep="\t", row.names = NULL)
useEvents$state <- ""
df <- rbind(df, useEvents)

# csvfile='LUPA-1498_Toimenpiteen_(rakennuksen)_valinta_liitteelle-use.csv'; curl -k -u lupapiste:$password https://splunk.address.here/servicesNS/nobody/search/search/jobs/export --data-urlencode search="search /api/command/set-attachment-operation" -d output_mode=csv > $csvfile; logfile='LUPA-1498_Toimenpiteen_(rakennuksen)_valinta_liitteelle-use.tsv'; echo -e "datetime\tid\taction" > $logfile; cat $csvfile | sed -n -e 's/^[^,]*,\"\([^,]*\)EE.*,.*/\1\tLUPA-1498_Toimenpiteen_\(rakennuksen\)_valinta_liitteelle\tUSED_IN_PROD/' -e '2,$'p >> $logfile
useEvents<-read.csv("LUPA-1498_Toimenpiteen_(rakennuksen)_valinta_liitteelle-use.tsv",sep="\t", row.names = NULL)
useEvents$state <- ""
df <- rbind(df, useEvents)

# csvfile='LUPA-1332_Viranomaiselle_oma_muistiinpanokentta-use.csv'; curl -k -u lupapiste:$password https://splunk.address.here/servicesNS/nobody/search/search/jobs/export --data-urlencode search="search /api/command/add-authority-notice" -d output_mode=csv > $csvfile; logfile='LUPA-1332_Viranomaiselle_oma_muistiinpanokentta-use.tsv'; echo -e "datetime\tid\taction" > $logfile; cat $csvfile | sed -n -e 's/^[^,]*,\"\([^,]*\)EE.*,.*/\1\tLUPA-1332_Viranomaiselle_oma_muistiinpanokentta\tUSED_IN_PROD/' -e '2,$'p >> $logfile
useEvents<-read.csv("LUPA-1332_Viranomaiselle_oma_muistiinpanokentta-use.tsv",sep="\t", row.names = NULL)
useEvents$state <- ""
df <- rbind(df, useEvents)


# csvfile='Lupa-1507-pakollisten-kenttien-tarkistus-use.csv'; curl -k -u lupapiste:$password https://splunk.address.here/servicesNS/nobody/search/search/jobs/export --data-urlencode search="search /api/command/set-organization-app-required-fields-filling-obligatory" -d output_mode=csv > $csvfile; logfile='Lupa-1507-pakollisten-kenttien-tarkistus-use.tsv'; echo -e "datetime\tid\taction" > $logfile; cat $csvfile | sed -n -e 's/^[^,]*,\"\([^,]*\)EE.*,.*/\1\tLupa-1507-pakollisten-kenttien-tarkistus\tUSED_IN_PROD/' -e '2,$'p >> $logfile
useEvents<-read.csv("Lupa-1507-pakollisten-kenttien-tarkistus-use.tsv",sep="\t", row.names = NULL)
useEvents$state <- ""
df <- rbind(df, useEvents)

df$datetime <- substr(df$datetime, 1, 16)
df$datetime <- strptime(df$datetime, "%Y-%m-%d %H:%M", tz = "EET")
df <- df[with(df, order(id, datetime)),]

##write.table(df, file = "m4s-poc-20141212.tsv", row.names = FALSE, sep = "\t", quote = FALSE)
colnames(df) <- c("Timestamp","Key","Action","State")

#09.08.2012 13:29
df$Timestamp <- format(df$Timestamp, format="%d.%m.%Y %H:%M")

#write.table(df, file = "Lupapiste-m4s-20141217.csv", row.names = FALSE, sep = ",", quote = FALSE)

write.table(df, file = "v4s-20150921.csv", row.names = FALSE, sep = ",", quote = FALSE)
