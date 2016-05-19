# insert instead of client_id id of application 
# https://oauth.vk.com/authorize?client_id=1&display=page&redirect_uri=http://vk.com&scope=groups&response_type=token&v=5.52

# function below load all .rds files in folder (data.frames) as variables with names of files
# junk <- lapply(list.files(pattern = ".rds$"),function(x){eval(parse(text=paste0(strsplit(x,split = "\\.")[[1]][1],"<<- readRDS('",x,"')")))})

require(dplyr)
require(jsonlite)
require(httr)

source('./codenames.R')

token <- ""   ## insert your token
user_id <- ""  ## insert your id 

##################
#Example of usage#
##################

#response <- sapply(knu_name_code, pipeline)
#response_2015 <- sapply(response, selectOnTime)
#response_2015_source <- sapply(response_2015, getRepostSources)

##################
##End of example##
##################


#main function
pipeline <- function(id){
    posts <- loadPosts(id)
    posts_list <- convertResponseToListofDF(posts)
    posts_df <- convertListofDFtoDF(posts_list)
    return(posts_df)
}

selectOnTime <- function(posts_df, start = "20150901", end="20160901"){
    return(posts_df[posts_df$date >= as.POSIXct(start,format="%Y%m%d") & posts_df$date < as.POSIXct(end,format="%Y%m%d"),])
}

getRepostSources <- function(posts_df){
    reposts_sources <- sort(table(posts_df$copy_from_id))
    return (reposts_sources)
}

getGroupMembers <- function(id){
    res <- numeric()
    id <- sub("^-","",id)
    reqstring <- paste0("https://api.vk.com/method/groups.getMembers?group_id=",id,"&count=1000&offset=0&user_id=",user_id,"&v=5.52&access_token=",token,collapse = "")
    temp <- GET(reqstring)
    counter <- ceiling(fromJSON(content(temp,"text"))$response$count/1000)
    res <- as.numeric(fromJSON(content(temp,"text"))$response$items)
    if (counter >= 2){
        for (i in 2:counter) {
            reqstring <- paste0("https://api.vk.com/method/groups.getMembers?group_id=",id,"&count=1000&offset=",(counter-1)*1000,"&user_id=",user_id,"&v=5.52&access_token=",token,collapse = "")
            temp <- GET(reqstring)
            res <- c(res, as.numeric(fromJSON(content(temp,"text"))$response$items))
        } 
    }
    return(res)
}

#support function
loadPosts <- function(id){
    res <- list()
    counter <- 1
    repeat {
        reqstring <- paste0("https://api.vk.com/method/wall.get?owner_id=",id,"&count=100&offset=",(counter-1)*100,"&user_id=",user_id,"&v=5.52&access_token=",token,collapse = "")
        res[[counter]] <- GET(reqstring)
        if (res[[counter]]$status_code!=200) {break}
        else if (nchar(content(res[[counter]],"text"))<45) {break}
        counter <- counter + 1
    } 
    return(res)
}

#support function 2 
convertResponseToListofDF <- function(posts){
    res <- lapply(posts, function(x){return(fromJSON(content(x,"text"))[[1]]$items)})
    return(res)
}

#support function 3 
convertListofDFtoDF <- function(posts){
    # id, from_id, owner_id, date, post_type, text, attachments, post_source, comments, likes, reposts, copy_history, signer_id
    res <- cbind(character(),character(),character(),character(),character(),character(),character(),character(),character(),character(),character(),list())
    for(i in 1:length(posts)){
        if(class(posts[[i]])=="list") break
        signer <- character()
        copy_from_id <- character()
        for(k in 1:nrow(posts[[i]])){
            if(is.null(posts[[i]]$copy_history[k][[1]])){
                copy_from_id[k] <- NA
            } else if (is.na(posts[[i]]$copy_history[k][[1]]$from_id[1])) {
                copy_from_id[k] <- NA
            } else {
                if(length(posts[[i]]$copy_history[k][[1]]$from_id)!=1){
                    copy_from_id[k] <- posts[[i]]$copy_history[k][[1]]$from_id[1]
                } else {
                    copy_from_id[k] <- posts[[i]]$copy_history[k][[1]]$from_id
                }
            }
            if(is.null(posts[[i]]$signer_id[k])){
                signer[k] <- NA
            } else if (is.na(posts[[i]]$signer_id[k])){
                signer[k] <- NA
            } else {
                signer[k] <- posts[[i]]$signer_id[k] 
            }
        }
        temp <- cbind(posts[[i]]$id,signer,posts[[i]]$from_id,posts[[i]]$owner_id,posts[[i]]$date,posts[[i]]$post_type,posts[[i]]$post_source$type,posts[[i]]$comments$count,posts[[i]]$likes$count,posts[[i]]$reposts$count,posts[[i]]$text,copy_from_id)
        res <- rbind(res,temp)
    }
    res <- as.data.frame(res,stringsAsFactors=FALSE)
    names(res) <- c("id","signer_id","from_id","owner_id","date","post_type","post_source_type","comments","likes","reposts","text","copy_from_id")
    res$post_type <- as.factor(unlist(res$post_type))
    res$post_source_type <- as.factor(unlist(res$post_source_type))
    res$comments <- as.numeric(unlist(res$comments))
    res$likes <- as.numeric(unlist(res$likes))
    res$reposts <- as.numeric(unlist(res$reposts))
    res$date <- as.POSIXct(as.numeric(unlist(res$date)), origin="1970-01-01")
    res$id <- as.numeric(unlist(res$id))
    res$signer_id <- unlist(res$signer_id)
    res$from_id <- unlist(res$from_id)
    res$owner_id <- unlist(res$owner_id)
    res$text <- unlist(res$text)
    res$copy_from_id <- unlist(res$copy_from_id)
    res <- res[order(res$id),]
    return(res)
}