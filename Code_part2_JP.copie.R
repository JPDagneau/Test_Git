##travail longitudinal partie 2
##Jean-Pascal Dagneau
#### importStations ####

importStations <- function(file)
{
    f <- file
    read.csv(f, colClasses = c(code = "integer", name = "character")) [, c("code", "name")]
}


#### importData ####

importData <- function(start, end, path)
{
    start.month <- as.numeric(substr(start, 6, 7))
    end.month <- as.numeric(substr(end, 6, 7))
    p <- path
    if (start.month < 4)
        start.month <- 4
    if (end.month > 11)
        end.month <- 11
    else if (start.month > end.month)
        stop("Dates invalides")
    
    for (i in seq(from = start.month, to = end.month))
    {
        if (i < 10)
            di <- paste("0", i, sep = "")
        else
        {
            di <- i
        }
        f <-
            paste(p, "/OD_", substr(start, 1, 5), di, ".csv", sep = "")
        
        l <-
            read.csv(
                f,
                colClasses = c(
                    start_date = "Date",
                    end_date = "Date",
                    start_station_code = "factor",
                    end_station_code = "factor",
                    duration_sec = "numeric",
                    is_member = "factor"
                )
            )
        if (i == start.month)
            Data <- l
        else
            Data <- rbind(Data, l)
    }
    subset(Data, start_date >= start & start_date <= end)
    
}

#Test sur mon ordi
#importData("2017-04-15","2017-04-15","data")



#### summary.duration ####
summary.duration <- function(x, per.status = FALSE)
{
    if (per.status == FALSE)
        return(summary(x[, 5]))
    else
        stats.m  <- summary((x[x[, 6] == 1, ])[, 5])
    stats.nm <- summary((x[x[, 6] == 0, ])[, 5])
    list("0" = unlist(stats.nm), "1" = unlist(stats.m))
}


#### getStations ####
getStations <- function(data, name)
{
    nlen <- length(name)
    l <- list("")
    n <- vector(length = nlen)
    for (i in seq(length(name)))
    {
        n[i] <- name[i]
        if (i > 1)
        {
            l[i]  <- numeric(1)
        }
    }
    for (i in seq(length(name)))
    {
        
        g <-data[grep(name [i], data[, 2]), ]
        s <- numeric(length(g[,1]))
        for (a in seq(length(g[,1])))
        {
         s[a]<- g[a,1]
        }
        l[[i]] <- s 
    }
    names(l) <- n
    l
}


