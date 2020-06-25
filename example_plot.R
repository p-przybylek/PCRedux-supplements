library(dplyr)
library(PCRedux)
library(reshape2)
library(patchwork)

curves <- 400

dec <- unlist(lapply(1L:(curves -1), function(i) {
  decision_modus(decision_res_kbqPCR[i, 2:8])
}))

class <- c(y = sum(dec == "y"), a = sum(dec == "a"), n = sum(dec == "n"))

p1_pos <- data.frame(kbqPCR[, c(1L, which(dec == "y") + 1)]) %>% 
  melt(id.vars = "cyc") %>%
  ggplot(aes(x = cyc, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  xlab("Cycle") +
  ylab("Raw RFU") +
  ggtitle(paste0("A) positive, ", "n = ", class[1])) + #qPCR curves
  theme(legend.position = "none")

# Positive Curves
p1_pos

p1_neg <- data.frame(kbqPCR[, c(1L, which(dec == "n") + 1)]) %>% 
  melt(id.vars = "cyc") %>%
  ggplot(aes(x = cyc, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  xlab("Cycle") +
  ylab("Raw RFU") +
  ggtitle(paste0("negative, ", "n = ", class[3])) + #qPCR curves
  theme(legend.position = "none")

# Negative Curves
p1_neg

p1_amb <- data.frame(kbqPCR[, c(1L, which(dec == "a") + 1)]) %>% 
  melt(id.vars = "cyc") %>%
  ggplot(aes(x = cyc, y = value, color = variable)) +
  geom_line() +
  theme_bw() +
  xlab("Cycle") +
  ylab("Raw RFU") +
  ggtitle(paste0("ambiguous, ", "n = ", class[2])) + #qPCR curves
  theme(legend.position = "none")

# Ambiguous Curves
p1_amb

res <- encu(kbqPCR[, 1L:curves])

dat <- cbind(res, decision = factor(c("ambiguous", "negative", "positive")[dec], 
                                    levels = c("positive", "ambiguous", "negative"))) %>%
  select(cpD2, amptester_polygon, cpDdiff, decision) %>%
  filter(!is.na(dec))

p2 <- ggplot(data = dat %>%
               mutate(id = rownames(dat)) %>%
               melt(id.vars = c("id", "decision")), 
             aes(x = decision, y = value)) +
  geom_boxplot() +
  theme_bw() +
  facet_wrap(~ variable, scales = "free_y") +
  scale_y_continuous("Value [A.U.]") +
  scale_x_discrete("Assessment") +
  ggtitle("B)") # Separation of types of curves by encu() parameters

# tsk <- makeClassifTask("pcr_classif", data = dat, target = "decision")
# 
# mdls <- list()
# mdls[[1]] <- makeLearner("classif.ranger", predict.type = "prob")
# mdls[[2]] <- makeLearner("classif.ksvm", predict.type = "prob")
# mdls[[3]] <- makeLearner("classif.lda", predict.type = "prob")
# mdls[[4]] <- makeLearner("classif.gbm", predict.type = "prob")
# mdls[[5]] <- makeLearner("classif.multinom", predict.type = "prob")
# mdls[[6]] <- makeLearner("classif.glmnet", predict.type = "prob")
# 
# set.seed(4732)
# results <- do.call(rbind, lapply(mdls, function(mdl) {
#   res <- resample(mdl, tsk, cv10, measures = list(mmce, multiclass.au1u))
#   cbind(model = res[["learner.id"]], res[["measures.test"]])
# }))
# results[["model"]] <- fct_recode(results[["model"]],
#                                  `ranger::ranger` = "classif.ranger", 
#                                  `kernlab::ksvm` = "classif.ksvm",
#                                  `MASS::lda` = "classif.lda",
#                                  `gbm::gbm` = "classif.gbm",
#                                  `nnet::multinom` = "classif.multinom",
#                                  `glmnet::glmnet` = "classif.glmnet")
# 
# p3 <- ggplot(data = results, aes(x = model, y = multiclass.au1u)) +
#   geom_point(position = position_jitter(width = 0.2, seed = 4)) + 
#   geom_errorbar(data = results %>% 
#                   group_by(model) %>% 
#                   summarise(auc = median(multiclass.au1u)), 
#                 aes(x = model, ymin = auc, ymax = auc), 
#                 inherit.aes = FALSE, color = "#FC5E61",
#                 width = 0.5) +
#   theme_bw() + 
#   xlab("Model") +
#   ylab("Mean AUC (one vs all)") +
#   ggtitle("C)") # Results of crossvalidating models trained on encu() parameters
# 
# p3

cairo_ps("figure1.eps", width = 11, height = 8.1)
(p1_pos + p1_neg + p1_amb) / p2 
dev.off()
