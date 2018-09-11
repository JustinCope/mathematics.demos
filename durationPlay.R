
# From Mosteller, Frederick, Robert Rourke & George Thomas, Jr. 1970. 
# Probability: A First Course, 2nd Edition. Reading: Addison-Wesley.

# Two players in a fair game have as fortunes m and n units, 
# and the stake on each play is one unit each.  
# Each player has an equal chance of winning a play.
# If they play until one player is ruined, how long will they play, 
# and what is the chance that the player starting with m units wins?


durationPlaySimulator <- function(m,n){
	plays = 0
	while(m > 0 && n >0){
		play <- sample(x=c('m','n'),size=1,replace=TRUE)
		if(play=='m'){
			m = m+1
			n = n-1
		} else {
			m = m-1
			n = n+1
		}
		plays <- plays + 1
	}
		return(c(m,n,plays))
	}


durationPlayEstimator <- function(iterations,x,y){
	iteration <- 1
	m <- 0
	n <- 0
	avgPlays <- vector()
	while(iteration<iterations){
		result <- durationPlaySimulator(x,y)
		m <- m + result[[1]]
		n <- n + result[[2]]
		avgPlays[[iteration]] <- result[[3]]
		iteration <- iteration + 1
	}
	return(c(m,n,mean(avgPlays)))
}

