# How to use the Dirichlet-tree R package to audit plurality elections.


# =============================================================================
# Load package.

library(elections.dtree)


# =============================================================================
# Create the data.

# Function to create plurality data in the `ranked_ballots` format.
# This is required for use with the R package.
plurality_ballots <- function(tallies, candidates = NULL) {
    if (is.null(candidates))
        candidates <- paste0("c", seq_along(tallies))
    ballots <- lapply(candidates, identity)  # convert to a list
    ballots <- rep(ballots, tallies)
    ballots <- ranked_ballots(ballots)
    return(ballots)
}

# Create some synthetic data.
candidates <- c("Damjan", "Alex", "Jacky")
num_ballots <- 20000
candidate_proportions <- c(0.49, 0.48, 0.03)
sample_tallies <- num_ballots * candidate_proportions
ballots <- plurality_ballots(sample_tallies, candidates)

# Check the data (calculate tallies).
table(sapply(ballots, '[', 1))


# =============================================================================
# Set up the model.

# Initialise a Dirichlet-tree prior.
dtree <- dirtree(candidates, min_depth = 1, max_depth = 1, a0 = 1)

# Update with data.
update(dtree, ballots)

# Check the state of the tree (it should show the candidate tallies).
dtree


# =============================================================================
# Calculate the posterior.

# Monte Carlo calculation of the posterior probability of the winner.
#
# Uses 5000 Monte Carlo samples (`n_elections').
#
# Assumes 10,000 ballots have been cast in the full election (`n_ballots`).
#
# The model will assume that the ballots in the sample that were previously
# used to update the tree (`ballots`, above) have come from a sample without
# replacement from this full set ballots.
posterior <- sample_posterior(dtree, n_elections = 100, n_ballots = 20000)


# =============================================================================
# Simulating an audit.
#
# Suppose that we observe the ballots one at a time.
# We can iteratively update the model and calculate the posterior.

# Reset the tree (forget the data and return to the prior).
reset(dtree)

# Shuffle the data (to simulate a random sample).
#
# Also, only take a very small sample size (n = 10), for illustrative purposes.
ballots_shuffled <- sample(ballots, 2000)

# Initialise a matrix to store the sequence of posterior probabilities.
posteriors <- matrix(nrow = length(ballots_shuffled),
                     ncol = length(candidates),
                     dimnames = list(num_ballots = seq_along(ballots_shuffled),
                                     candidates  = candidates))

# Run the audit.
for (i in seq_along(ballots_shuffled)) {
    update(dtree, ballots_shuffled[i])
    posteriors[i, ] <- sample_posterior(dtree, n_elections = 1000, n_ballots = 20000)
}

# Look at the posteriors calculated at each stage of the audit.
posteriors

BF = posteriors[,1]/(posteriors[,2]+posteriors[,3])
Test = which(BF > (0.95)/0.05)
Test[1]

mc_test <- function(pT, n=100){
    
    candidates <- c("Damjan", "Alex", "Jacky")
    num_ballots <- 20000
    sample_tallies <- num_ballots * pT
    ballots <- plurality_ballots(sample_tallies, candidates)
    
    trials = replicate(n, run_model(candidates,ballots))
    sample_sizes = trials[1,]
    sample_outcomes = trials[2,]
    certifications = sum(sample_outcomes == TRUE)
    
    certification_rate = certifications/n
    
    #lastly, calculate the mean sample size tested
    mean_sample_size = mean(unlist(sample_sizes))
    
    return (list(certification_rate, mean_sample_size))
    
}

run_model <- function(candidates, ballots){
    dtree <- dirtree(candidates, min_depth = 1, max_depth = 1, a0 = 1)
    
    update(dtree, ballots)
    
    posterior <- sample_posterior(dtree, n_elections = 10, n_ballots = 20000)
    
    reset(dtree)
    
    ballots_shuffled <- sample(ballots, 2000)
    
    # Initialise a matrix to store the sequence of posterior probabilities.
    posteriors <- matrix(nrow = length(ballots_shuffled),
                         ncol = length(candidates),
                         dimnames = list(num_ballots = seq_along(ballots_shuffled),
                                         candidates  = candidates))
    
    # Run the audit.
    for (i in seq_along(ballots_shuffled)) {
        update(dtree, ballots_shuffled[i])
        posteriors[i, ] <- sample_posterior(dtree, n_elections = 1000, n_ballots = 20000)
    }
    BF = posteriors[,1]/(posteriors[,2]+posteriors[,3])
    Test = which(BF > (0.95)/0.05)
    
    if (length(Test)==0){
        return (list(2000, FALSE))
    } else {
        return(list(Test[1], TRUE))
    }
    

}
