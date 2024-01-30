
#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#' Compute the updated expected anxiety
#'
#' @param momAnx The momentary anxiety, on a scale from 0 to 1
#' @param prevAnx The previous expected anxiety, on a scale from 0 to 1
#' @param alpha A factor that shifts the weight between the momentary anxiety (alpha=1) and the previous expected anxiety (alpha=0) when the updated expected anxiety is computed.
expAnx <- function(momAnx, prevAnx, alpha=0.5) {
  momAnx*alpha + prevAnx*(1-alpha)
}



momAnx <- function(affToneIIC) {
  0.05*exp(-2*affToneIIC)
}


affToneIIC <- function(affToneInstruction,
                       expAnx,
                       beta_instruction=1,
                       beta_anx=-1,
                       beta_IA=-0.3) {

    beta_instruction*affToneInstruction +
    beta_anx*expAnx +
    beta_IA*expAnx*affToneInstruction
}

# TODO:
appTendency <- function() {

}



#------------------------------------------------------------
# Plots for testing
#------------------------------------------------------------

affToneIIC_values <- seq(-1, 1, by=.01)
plot(affToneIIC_values, momAnx(affToneIIC_values), xlim=c(-1, 1), ylim=c(0, 1), type="l")

# test a sequence of momentary anxieties
momAnxSeq <- c(0, 0, 0, 0.37, 0, 1, 0, 0)

expAnxRes <- c(0.3, rep(NA, length(momAnxSeq)))

for (i in seq_along(momAnxSeq)) {
  expAnxRes[i+1] <- expAnx(momAnxSeq[i], expAnxRes[i], alpha=0.17)
}

plot(seq_along(expAnxRes), expAnxRes, type="b", ylim=c(0, 1))

affToneIIC(affToneInstruction=c(0, 0, 0, 1, 1, 1),
           expAnx=c(0, .5, 1, 0, .5, 1))
