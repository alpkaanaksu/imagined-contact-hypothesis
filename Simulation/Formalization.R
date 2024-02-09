
#------------------------------------------------------------
# single path formulae
#------------------------------------------------------------

#' Compute the updated expected anxiety
#'
#' @param momentary_anxiety The momentary anxiety, on a scale from 0 to 1
#' @param previous_expected_anxiety The previous expected anxiety, on a scale from 0 to 1
#' @param alpha A factor that shifts the weight between the momentary anxiety (alpha=1) and the previous expected anxiety (alpha=0) when the updated expected anxiety is computed.

expected_anxiety <- function(momentary_anxiety, previous_expected_anxiety, alpha=0.5) {
  ea <-  momentary_anxiety * alpha + previous_expected_anxiety * (1 - alpha)
  min(1, ea)
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
    alpha = rbeta(n, shape1=3, shape2=12),
    expected_anxiety = rbeta(n, shape1=4, shape2=4.7)
  )
}

#------------------------------------------------------------
# Test
#------------------------------------------------------------

library(dplyr)


# Do a simulated study with n=100 (overall)
#------------------------------------------------

instruction_tone <- 0.5
intervention_iterations <- 1
n <- 100 # overall sample size (control + experimental group)

group <- generate_group(n)

group <- group |>
  mutate(condition = ifelse(id %% 2 == 0, "treatment", "control"))


group <- group |>
  mutate(
    expected_anxiety_t2 =
      ifelse(
        condition == "treatment",
        iic_intervention(
          intervention_iterations,
          person = list(expected_anxiety = expected_anxiety, alpha = alpha),
          affective_tone_instruction = instruction_tone)$expected_anxiety,
        expected_anxiety
      )
  )

# groups difference after intervention: treatment effect
t1 <- t.test(group$expected_anxiety_t2 ~ group$condition, alternative = "greater")
t1

library(compute.es)
ES <- tes(t1$statistic, n/2, n/2, level = 95, verbose=FALSE)
ES$d

# compare with pusblished treatment effects: Turner et al. 2013, Exp 2
tes(t=4.63, n.1=20, n.2=21) # -> d = 1.45 LOL

# install.packages('ggrain')
library(ggrain)
ggplot(group, aes(x=condition, y=expected_anxiety_t2)) + geom_rain() + ggtitle(paste0("Effect size: d = ", round(ES$d, 2)))


# Systematically investigate the effect of 
# affective_tone_instruction on raw treatment effect
#------------------------------------------------

# show raw treatment effect (i.e., mean difference for different intervention strengths
# Use huge samples to get precise estimates
TE <- data.frame()
for (instr_tone in seq(-1, 1, by=0.2)) {
  print(instr_tone)
  group <- generate_group(n=10000)

  group <- group |>
    mutate(condition = ifelse(id %% 2 == 0, "treatment", "control"))

  group <- group |>
    mutate(
      expected_anxiety_t2 =
        ifelse(
          condition == "treatment",
          iic_intervention(
            intervention_iterations,
            person = list(expected_anxiety = expected_anxiety, alpha = alpha),
            affective_tone_instruction = instr_tone)$expected_anxiety,
          expected_anxiety
        )
    )

  # groups difference after intervention: raw treatment effect
  mean_diff <- diff(tapply(group$expected_anxiety_t2, group$condition, mean))

  TE <- rbind(TE, data.frame(
    instr_tone = instr_tone,
    mean_diff = mean_diff
  ))
}

ggplot(TE, aes(x=instr_tone, y=mean_diff)) + geom_point() + geom_line()


#------------------------------------------------------------
# Sensitivity analyses
#------------------------------------------------------------

# explore the boundaries (and combinations) of certain parameters
params <- expand.grid(
  expected_anxiety = c(0, .2, .4, .6, .8, 1),
  alpha = c(0, .25, .5, .75, 1)
)

update <- iic_intervention(1, params, affective_tone_instruction=-1)
res <- cbind(params, expected_anxiety_updated = update[, 1])
res$alpha_label <- paste0("alpha = ", res$alpha)
ggplot(res, aes(x=expected_anxiety, y=expected_anxiety_updated)) + geom_point() + geom_line() + facet_grid(~alpha_label)


# add one more dimension: vary the impact of the affective_tone_instruction
res <- data.frame()
for (ati in seq(-1, 1, by=0.5)) {
  update <- iic_intervention(1, params, affective_tone_instruction=ati)
  res0 <- cbind(params, expected_anxiety_updated = update[, 1])
  res0$affective_tone_instruction <- ati
  res <- rbind(res, res0)
}

library(ggplot2)
res$alpha_label <- paste0("alpha = ", res$alpha)
res$ati_label <- factor(res$affective_tone_instruction, levels=sort(unique(res$affective_tone_instruction)), labels=paste0("ati = ", sort(unique(res$affective_tone_instruction))))
ggplot(res, aes(x=expected_anxiety, y=expected_anxiety_updated)) + 
  geom_point() + 
  geom_line() + 
  facet_grid(ati_label~alpha_label) +
  geom_abline(intercept=0, slope=1, linetype="dashed") +
  geom_hline(yintercept=0, linetype="dotted", color="red", size=1) +
  geom_hline(yintercept=1, linetype="dotted", color="red", size=1)

# Uppsi: Unser Modell generiert Werte für expAnx_Updated, die außerhalb des Wertebereichs liegen

# different visualization: the difference from expected_anxiety and expected_anxiety_updated (= treatment effect)
res$expAnxChange <- res$expected_anxiety_updated - res$expected_anxiety
ggplot(res, aes(x=expected_anxiety, y=expAnxChange)) + geom_point() + geom_line() + facet_grid(ati_label~alpha_label)

# New hypotheses generated from the model:

# if the expected_anxiety at t0 is (close to) zero, the intervention makes no difference
# 

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
