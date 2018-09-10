# From: 
# An ordinary deck of 52 playing cards containing four aces is shuffled thoroughly, 
# and we count from the top the number of cards down to and including the first ace, and record the count.
# The process is repeated.
# What is the average count?
# What is the probability that the first ace is on card 1? 2? ... 52?
# Within what number of cards will we find the first ace half the time?

firstAceSimulator <- function(){
# randomize the numbers 1 through 52
# modular division by 13 yields four "suits" of 0 through 12
# let the 0s be our four Aces
	shuffledCards <- sample(x=1:52,size=52,replace=FALSE)%%13
	firstAce <- FALSE # firstAce is initialized to FALSE
	index <- 1 # index is initialized to 1
	while(firstAce==FALSE){ # search the deck until the first Ace is encountered
		if(shuffledCards[[index]]!=0){ 
			index <- index + 1 
		} else {
			firstAce <- TRUE
		}
	}
	return(index) # return the index of the first Ace
}

# iterations parameter determines how many simulations will be carried out
SimulationIterator <- function(iterations){
	iteration <- 1 # initialize counter to track progress through iterations
	firstAceIndices <- vector() # initialize vector as container for the first Ace index returned by each simulation
	while(iteration<=iterations){ 
		firstAceIndices[[iteration]] <- firstAceSimulator() # perform a single simulation and store the result 
		iteration <- iteration + 1 
	}
	print(paste("mean: ",mean(firstAceIndices))) # calculate and print the mean
	print(paste("median: ",median(firstAceIndices))) # calculate and print the median
}