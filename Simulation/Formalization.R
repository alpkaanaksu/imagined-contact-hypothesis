
#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#' Compute the updated expected anxiety
#'
#' @param momentary_anxiety The momentary anxiety, on a scale from 0 to 1
#' @param previous_expected_anxiety The previous expected anxiety, on a scale from 0 to 1
#' @param alpha A factor that shifts the weight between the momentary anxiety (alpha=1) and the previous expected anxiety (alpha=0) when the updated expected anxiety is computed.
expected_anxiety <- function(momentary_anxiety, previous_expected_anxiety, alpha=0.5) {
  momentary_anxiety*alpha + previous_expected_anxiety*(1-alpha)
}

#' ALTERNATIVE: Compute the updated expected anxiety
#'
#' @param momentary_anxiety The momentary anxiety, on a scale from 0 to 1
#' @param previous_expected_anxiety The previous expected anxiety, on a scale from 0 to 1
#' @param previous_experience Number of previous experiences, should increase through IIC
expected_anxiety_alternative <- function(momentary_anxiety, previous_expected_anxiety, previous_experience = 1) {
  alpha <- 1 / (1 + 0.1 * previous_experience)
  momentary_anxiety*alpha + previous_expected_anxiety*(1-alpha)
}

#' Compute the momentary anxiety 
#'
#' @param affective_tone_iic The affective tone of the iic, on a scale from 0 to 1
momentary_anxiety <- function(affective_tone_iic) {
  0.05*exp(-2*affective_tone_iic)
}

#' Compute the affective tone of iic
#'
#' @param affective_tone_instruction The affective tone of instruction, on a scale from -1 to 1
#' @param expected_anxiety The expected anxiety, on a scale from 0 to 1
#' @param beta_instruction Regression slope of the effect of the affective tone of instruction 
#' @param beta_anxiety Regression slope of the effect of the expected anxiety
#' @param beta_interaction Regression slope of the interaction between expected anxiety and the tone of instruction
affective_tone_iic <- function(affective_tone_instruction,
                       expected_anxiety,
                       beta_instruction=1,
                       beta_anxiety=-1,
                       beta_interaction=-0.3) {

    beta_instruction*affective_tone_instruction +
    beta_anxiety*expected_anxiety +
    beta_interaction*expected_anxiety*affective_tone_instruction
}

#' Compute the resulting approach tendency
#'
#' @param expected_anxiety The expected anxiety, on a scale from 0 to 1
approach_tendency <- function(expected_anxiety) {
  exp(-5 * expected_anxiety)
}


#------------------------------------------------------------
# Simulation
#------------------------------------------------------------


#' Compute updated person after one iic
#'
#' @param affective_tone_instruction The affective tone of instruction, on a scale from -1 to 1
#' @param person Name vector c(expected_anxiety, alpha) where expected anxiety is
#'  the initial expected anxiety and alpha of that person
#' 
#' @return updated person vector after one iteration
iic_iteration <- function(affective_tone_instruction, person) {
  ati <- affective_tone_iic(affective_tone_instruction, person$expected_anxiety)
  ma <- momentary_anxiety(affective_tone_iic = ati)
  ea <- expected_anxiety(momentary_anxiety = ma,
                        previous_expected_anxiety = person$expected_anxiety,
                        alpha = person$alpha)

  person$expected_anxiety <- ea
  person
}

#' Compute updated person after complete iic intervention
#'
#' @param num_iterations The number of iics the person will go through
#' @param affective_tone_instruction The affective tone of instruction throughout
#'  the intervention, on a scale from -1 to 1
#' @param person Name vector c(expected_anxiety, alpha) where expected anxiety is
#'  the initial expected anxiety before the intervention and alpha of that person
#' 
#' @return updated person vector after complete intervention
iic_intervention <- function(num_iterations, person, affective_tone_instruction) {
  for (i in seq_len(num_iterations)) {
    person <- iic_iteration(affective_tone_instruction, person)
  }

  person
}

#------------------------------------------------------------
# Helper Functions
#------------------------------------------------------------

generate_group <- function(n) {
  data.frame(
    id = seq_len(n),
    alpha = rnorm(n, 0.2, 0.1),
    expected_anxiety = rnorm(n, 0.3, 0.1)
  )
}

#------------------------------------------------------------
# Test
#------------------------------------------------------------

library(dplyr)

instruction_tone <- 0.8
intervention_iterations <- 20

group <- generate_group(1000)

group <- group |>
  mutate(condition = ifelse(id %% 2 == 0, "treatment", "control"))

t.test(group$expected_anxiety ~ group$condition, alternative = "greater")

group <- group |>
  mutate(
    expected_anxiety =
      ifelse(
        condition == "treatment",
        iic_intervention(
          intervention_iterations,
          person = list(expected_anxiety = expected_anxiety, alpha = alpha),
          affective_tone_instruction = instruction_tone)$expected_anxiety,
        expected_anxiety
      )
  )


t.test(group$expected_anxiety ~ group$condition, alternative = "greater")

#------------------------------------------------------------
# Plots for testing
#------------------------------------------------------------


affective_tone_iic_values <- seq(-1, 1, by=.01)
plot(affective_tone_iic_values, momentary_anxiety(affective_tone_iic_values), xlim=c(-1, 1), ylim=c(0, 1), type="l")

# test a sequence of momentary anxieties
momentary_anxietySeq <- c(0, 0, 0, 0.37, 0, 1, 0, 0)

expected_anxietyRes <- c(0.3, rep(NA, length(momentary_anxietySeq)))

for (i in seq_along(momentary_anxietySeq)) {
  expected_anxietyRes[i+1] <- expected_anxiety(momentary_anxietySeq[i], expected_anxietyRes[i], alpha=0.17)
}

plot(seq_along(expected_anxietyRes), expected_anxietyRes, type="b", ylim=c(0, 1))

affective_tone_iic(affective_tone_instruction=c(0, 0, 0, 1, 1, 1),
           expected_anxiety=c(0, .5, 1, 0, .5, 1))
