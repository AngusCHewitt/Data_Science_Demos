# Load packages
library("rankdist")
library("Rankcluster")
library("png")
source("helping_functions.R")


# Examples in Chapter 6: Using the rankdist package 

set.seed(1080)
gen1 <- GenerateExample(ranking = TRUE)
tail(gen1$ranking)
dat1 <- new("RankData", ranking = gen1$ranking, count = gen1$count)

gen2 <- GenerateExample(ranking = FALSE)
tail(gen2$ordering)
dat2 <- new("RankData", ordering = gen2$ordering, count = gen2$count)

str1 <- MomentsEst(dat1, 500)
avg_rank <- dat1@count %*% dat1@ranking
modal_ranking.init <- OrderingToRanking(order(avg_rank))
modal_ranking.init.c1 <- sample(dat1@nobj, dat1@nobj)
modal_ranking.init.c2 <- sample(dat1@nobj, dat1@nobj)

init1 <- new("RankInit", 
			 param.init = list(str1), 
			 modal_ranking.init = list(modal_ranking.init), 
			 clu = 1L)
init1c <- new("RankInit", 
			  param.init = list(str1, str1), 
			  modal_ranking.init = list(modal_ranking.init.c1, modal_ranking.init.c2), 
			  clu = 2L)
ctrl1 <- new("RankControlKendall", SearchPi0_show_message = FALSE)
model1 <- RankDistanceModel(dat1, init1, ctrl1)
ModelSummary(model1)
model1c <- RankDistanceModel(dat1, init1c, ctrl1)

genq <- GenerateExampleTopQ()
head(genq$ranking)
datq <- new("RankData", ranking = genq$ranking, count = genq$count,
			nobj = ncol(genq$ranking), topq = max(genq$ranking)-1)
initq <- new("RankInit", 
			 param.init = list(rep(0.5, datq@topq)), 
			 modal_ranking.init = list(modal_ranking.init), 
			 clu = 1L)
ctrlq <- new("RankControlWeightedKendall", SearchPi0_show_message = FALSE)
modelq <- RankDistanceModel(datq, initq, ctrlq)


# Code for Chapter 7.1 simulation study 

# 7.1.2 Estimate pi0 given w 
set.seed(43)

# small sample size (Table 1: n = 200)
pi0 <- 1:40
w0 <- log(40:2) / 5
sample_size <- 200
res2 <- replicate(500, run_given_w_estimate_pi0(pi0, w0, sample_size))
res2_rank <- t(res2[1:length(pi0), ])
res2_step <- res2[length(pi0) + 1, ]
res2_borda <- t(res2[(length(pi0) + 2):nrow(res2), ])
table(res2_step)
table(DistanceBlock(res2_rank, pi0))
table(DistanceBlock(res2_borda, pi0))

# large sample size (Table 1: n = 500)
sample_size <- 500
res3 <- replicate(500, run_given_w_estimate_pi0(pi0, w0, sample_size))
res3_rank <- t(res3[1:length(pi0), ])
res3_step <- res3[length(pi0) + 1, ]
res3_borda <- t(res3[(length(pi0) + 2):nrow(res3), ])
table(DistanceBlock(res3_rank, pi0))
table(DistanceBlock(res3_borda, pi0))

# Replicate Table 1 exactly
tbl1 = rbind(table(DistanceBlock(res2_borda, pi0)),
			 table(DistanceBlock(res2_rank, pi0)),
			 c(table(DistanceBlock(res3_borda, pi0)), 0),
			 c(table(DistanceBlock(res3_rank, pi0)), 0))
header1 = cbind(c(200, 200, 500, 500), c("initial", "final", "initial", "final"))
result1 = data.frame(cbind(header1, tbl1))
message("Table 1")
print(result1)

# 7.1.3 Estimate w given pi0, Figure 2
# Produces Figure 2
set.seed(42)
pi0 <- 1:40
w0 <- log(40:2) / 5
sample_size <- 500
res <- replicate(500, run_given_pi0_estimate_w(pi0, w0, sample_size))
res_long <- melt(as.data.frame(t(res)))
res_long$variable <- factor(substr(res_long$variable, 2, 10),
							levels = unique(as.numeric(substr(res_long$variable, 2, 10))))
true_long <- data.frame(variable = 1:39, value = w0)
ggplot(data = res_long, aes(x = variable, y = value)) +
	geom_boxplot() +
	geom_point(data = true_long, aes(x = variable, y = value), col = "red") +
	labs(x = "w", y = "") +
	theme_classic()
ggsave(filename = "Estimating_weights.png")

# 7.1.4 Estimate both w and pi0. 
# Produces Table 2 and Figure 3
# (this experiment will take a few hours ...)
set.seed(44)
pi0 <- 1:40
w0 <- log(40:2) / 5
sample_size <- 500
res4 <- replicate(500, run_estimate_both(pi0, w0, sample_size))
res4_w <- res4[1:(length(pi0) - 1), ]
res4_rank <- t(res4[length(pi0):(2*length(pi0) - 1), ])
res4_borda <- t(res4[(2*length(pi0)):nrow(res4), ])
table(DistanceBlock(res4_rank, pi0))
table(DistanceBlock(res4_borda, pi0))
res_long <- melt(as.data.frame(t(res4_w)))
res_long$variable <- factor(substr(res_long$variable, 2, 10),
							levels = unique(as.numeric(substr(res_long$variable, 2, 10))))
true_long <- data.frame(variable = 1:39, value = w0)
ggplot(data = res_long, aes(x = variable, y = value)) +
	geom_boxplot() +
	geom_point(data = true_long, aes(x = variable, y = value), col = "red") +
	labs(x = "w", y = "") +
	theme_classic()
ggsave(filename = "Estimating_weights_full_small.png")

# Produce Table 2 Exactly
tbl2 <- rbind(table(DistanceBlock(res4_borda, pi0)),
			  c(table(DistanceBlock(res4_rank, pi0)), 0))
header2 <- cbind(rep(500, 2), c("initial", "final"))
result2 <- data.frame(cbind(header2, tbl2))
message("Table 2")
print(result2)

# Code for Chapter 7.2: Modeling ranking probability 
# Produces Table 3, 4, 5, 6 and Figure 4
ModelSummaryTable <- function(model){
	# summary of fitted models
	clus <- rev(order(model$p))
	nclu <- length(clus)
	dof <- model$free_params
	bic <- model$BIC
	summary_fitted <- c(components=nclu, free_parameters=dof, BIC=bic)
	
	# summary for par
	summary_params <- NULL
	for (i in 1:length(clus)) {
		tmp_res <- c(cluster=i, model$modal_ranking.est[[clus[i]]], 
					 round(model$w.est[[clus[i]]], digits = 2), 
					 proportion=round(model$p[clus[i]], digits = 2))
		if (i==1){
			summary_params <- tmp_res
		} else {
			summary_params <-rbind(summary_params, tmp_res)
		}
	}
	row.names(summary_params) <- NULL
	colnames(summary_params)[2:6] <- LETTERS[1:5]
	list(summary_fitted=summary_fitted, summary_params=summary_params)
}
## Weighted Kendall distance for complete votes
set.seed(666)
apa_wk_ctrl <- new("RankControlWeightedKendall",
				   SearchPi0_fast_traversal = TRUE,
				   SearchPi0_show_message = FALSE)
geoKendall_model <- vector(mode = "list", length = 3)
complete_mode <- list(c(2, 3, 1, 5, 4), c(3, 4, 5, 1, 2), c(4, 2, 5, 3, 1))
mode <- list()
mode[[1]] <- complete_mode[1]
mode[[2]] <- complete_mode[1:2]
mode[[3]] <- complete_mode
for (i in 1L:3L) {
	init <- new("RankInit", param.init = replicate(i, rep(0.5, 4), FALSE),
				modal_ranking.init = mode[[i]], clu = i, p.init = rep(1, i)/i)
	geoKendall_model[[i]] <- RankDistanceModel(apa_obj, init, apa_wk_ctrl)
}
dummy <- sapply(geoKendall_model, function(x) cat("BIC:", x$BIC, "\tSSR", x$SSR, "\n"))
ModelSummary(geoKendall_model[[3]])
## visualization
d1 <- DistanceMatrix(apa_obj@ranking)
sc <- scale(apa_obj@count, center = FALSE)
sc <- sc*6
pc <- log(geoKendall_model[[3]]$expectation)
pc <- round((pc-min(pc))/(max(pc)-min(pc))*100)
hc <- heat.colors(100, alpha = 1)
hc <- rev(hc)
cols <- hc[pc]
png(filename = "MDS_APA.png", width = 800, height = 800)
plot(cmdscale(d1), lwd = sc, pch = 19, col = cols, 
	 main = "Multidimensional scaling plot for APA data", 
	 xlab = "x", ylab = "y")
points(x = cmdscale(d1)[89, 1], y = cmdscale(d1)[89, 2]-0.3, col = "blue", pch = "A", lwd = 10)
points(x = cmdscale(d1)[56, 1], y = cmdscale(d1)[56, 2]-0.2, col = "blue", pch = "B", lwd = 10)
points(x = cmdscale(d1)[37, 1], y = cmdscale(d1)[37, 2]-0.2, col = "blue", pch = "C", lwd = 10)
dev.off()
geoKendall_model_sum <- ModelSummaryTable(geoKendall_model[[3]])

## Weighted Kendall distance for all votes
set.seed(123)
# tied-rank
apa_wk_ctrl@assumption <- "tied-rank"
geoKendall_model_q <- vector(mode = "list", length = 3)
complete_mode <- list(c(3, 4, 5, 2, 1), c(2, 1, 5, 3, 4), c(2, 3, 1, 5, 4))
mode <- list()
mode[[1]] <- complete_mode[1]
mode[[2]] <- complete_mode[1:2]
mode[[3]] <- complete_mode
for (i in 1L:3L) {
	init <- new("RankInit", param.init = replicate(i, rep(0.1, 4), FALSE),
				modal_ranking.init = mode[[i]], clu = i, p.init = rep(1, i)/i)
	geoKendall_model_q[[i]] <- RankDistanceModel(apa_partial_obj, init, apa_wk_ctrl)
}
dummy <- sapply(geoKendall_model_q, function(x) cat("BIC:", x$BIC, "\tSSR", x$SSR, "\n"))
ModelSummary(geoKendall_model_q[[3]])
# equal-probability
apa_wk_ctrl@assumption <- "equal-probability"
geoKendall_model_eq <- vector(mode = "list", length = 3)
complete_mode <- list(c(3, 4, 5, 1, 2), c(2, 3, 1, 5, 4), c(3, 1, 5, 4, 2))
mode <- list()
mode[[1]] <- complete_mode[1]
mode[[2]] <- complete_mode[1:2]
mode[[3]] <- complete_mode
for (i in 1L:3L) {
	init <- new("RankInit", param.init = replicate(i, rep(0.5, 4), FALSE),
				modal_ranking.init = mode[[i]], clu = i, p.init = rep(1, i)/i)
	geoKendall_model_eq[[i]] <- RankDistanceModel(apa_partial_obj, init, apa_wk_ctrl)
}
dummy <- sapply(geoKendall_model_eq, function(x) cat("BIC:", x$BIC, "\tSSR", x$SSR, "\n"))
ModelSummary(geoKendall_model_eq[[3]])

geoKendall_model_q_sum <- ModelSummaryTable(geoKendall_model_q[[3]])
geoKendall_model_eq_sum <- ModelSummaryTable(geoKendall_model_eq[[3]])

## Mallows" Phi (Kendall distance) for complete votes
set.seed(788)
apa_k_ctrl<-new("RankControlKendall", SearchPi0_fast_traversal = TRUE,
				SearchPi0_show_message = FALSE)
Kendall_model <- vector(mode = "list", length = 8)
complete_mode <- list(c(2, 3, 1, 5, 4), c(3, 4, 5, 2, 1), c(3, 2, 5, 1, 4),
					  c(4, 3, 2, 5, 1), c(2, 5, 1, 3, 4), c(1, 2, 3, 4, 5),
					  c(5, 4, 3, 2, 1), c(3, 4, 2, 1, 5))
mode <- list()
for (i in 1:8) {
	mode[[i]] <- complete_mode[1:i]
}
for (i in 1L:8L) {
	init <- new("RankInit", param.init = replicate(i, 0.1, FALSE),
				modal_ranking.init = mode[[i]], clu = i, p.init = rep(1, i)/i)
	Kendall_model[[i]] <- RankDistanceModel(apa_obj, init, apa_k_ctrl)
}
dummy <- sapply(Kendall_model, function(x)cat("BIC:", x$BIC, "\tSSR", x$SSR, "\n"))
ModelSummary(Kendall_model[[5]])
Kendall_model_sum <- ModelSummaryTable(Kendall_model[[5]])

## Weighted-tau model for complete votes
set.seed(123)
apa_wt_ctrl <- new("RankControlWtau", SearchPi0_fast_traversal = TRUE,
				   SearchPi0_show_message = FALSE)
wt_model <- vector(mode = "list", length = 3)
for (i in 1L:3L) {
	init <- new("RankInit", param.init = replicate(i, rep(0.05, 5), FALSE),
				modal_ranking.init = replicate(i, sample(5, 5), FALSE),
				clu = i, p.init = rep(1, i)/i)
	wt_model[[i]] <- RankDistanceModel(apa_obj, init, apa_wt_ctrl)
}
dummy <- sapply(wt_model, function(x)cat("BIC:", x$BIC, "\tSSR", x$SSR, "\n"))
ModelSummary(wt_model[[3]])
wt_model_sum <- ModelSummaryTable(wt_model[[3]])

## ISR Model for complete votes
# This will take several hours
data("APA", package = "Rankcluster")
str(APA)
set.seed(42)
model_1 <- rankclust(data = APA$data, K = 1, detail = TRUE, Qsem = 1000,
					 Bsem = 100, Ql = 500, Bl = 50, maxTry = 20, run = 10)
summary(model_1)
model_2 <- rankclust(data = APA$data, K = 2, detail = TRUE, Qsem = 1000,
					 Bsem = 100, Ql = 500, Bl = 50, maxTry = 20, run = 10)
summary(model_2)
model_3 <- rankclust(data = APA$data, K = 3, detail = TRUE, Qsem = 1000,
					 Bsem = 100, Ql = 500, Bl = 50, maxTry = 20, run = 10)
summary(model_3)
set.seed(41)
model_4 <- rankclust(data = APA$data, K = 4, detail = TRUE, Qsem = 1000,
					 Bsem = 100, Ql = 500, Bl = 50, maxTry = 20, run = 10)
summary(model_4)

## ISR Model for all votes
# re-format data set to Rankcluster
str(apa_partial_obj)
rank_mat_compact <- matrix(ncol = 5, nrow = 0)
for (i in 1:4) {
	tmp_mat <- apa_partial_obj@ranking[apa_partial_obj@q_ind[i]:(apa_partial_obj@q_ind[i + 1] - 1), ]
	if (i < 4) tmp_mat[tmp_mat == max(tmp_mat)] <- NA
	rank_mat_compact <- rbind(rank_mat_compact, tmp_mat)
}
rank_mat_full <- matrix(ncol = 5, nrow = 0)
for (i in 1:length(apa_partial_obj@count)) {
	r <- rank_mat_compact[i, ]
	tmp_mat <- matrix(data = rep(r, apa_partial_obj@count[i]),
					  ncol = 5, byrow = TRUE)
	rank_mat_full <- rbind(rank_mat_full, tmp_mat)
}
# fit models
set.seed(39)
model_1_f <- rankclust(data = rank_mat_full, K = 1, detail = TRUE)
summary(model_1_f)
set.seed(38)
model_2_f <- rankclust(data = rank_mat_full, K = 2, detail = TRUE)
summary(model_2_f)
set.seed(37)
model_3_f <- rankclust(data = rank_mat_full, K = 3, detail = TRUE)
summary(model_3_f)
set.seed(36)
model_4_f <- rankclust(data = rank_mat_full, K = 4, detail = TRUE)
summary(model_4_f)

# Produce Table 3
geoKendall_model_sum$summary_fitted
Kendall_model_sum$summary_fitted
wt_model_sum$summary_fitted

model_4_n_param <- (model_4@results[[1]]@bic + 2 * model_4@results[[1]]@ll) / log(length(model_4@results[[1]]@partition))
model_4_sum <- c(components=model_4@K, free_parameters=model_4_n_param, BIC=model_4@results[[1]]@bic)

tbl3 = rbind(geoKendall_model_sum$summary_fitted,
			 Kendall_model_sum$summary_fitted,
			 wt_model_sum$summary_fitted,
			 model_4_sum)
row.names(tbl3) = c("Weighted Kendall", "Mallows", "Weighted Tau", "ISR")
message("Table 3")
print(tbl3)

# Produce Table 4
row.names(geoKendall_model_sum$summary_params) <- rep("Weighted Kendall", 3)
row.names(Kendall_model_sum$summary_params) <- rep("Mallows", 5)
row.names(wt_model_sum$summary_params) <- rep("Weighted Tau", 3)
message("Table 4")
geoKendall_model_sum$summary_params
Kendall_model_sum$summary_params
wt_model_sum$summary_params

# Produce Table 5

geoKendall_model_eq_sum$summary_fitted
geoKendall_model_q_sum$summary_fitted

model_3f_n_param <- (model_3_f@results[[1]]@bic + 2 * model_3_f@results[[1]]@ll) / log(length(model_3_f@results[[1]]@partition))
model_3_f_sum <- c(components=model_3_f@K, free_parameters=model_3f_n_param, BIC=model_3_f@results[[1]]@bic)

tbl5 = rbind(geoKendall_model_eq_sum$summary_fitted,
			 geoKendall_model_q_sum$summary_fitted,
			 model_3_f_sum)
row.names(tbl5) = c("Equal probability", "Tied rank", "ISR")
message("Table 5")
print(tbl5)

# Produce Table 6
row.names(geoKendall_model_eq_sum$summary_params) <- rep("Equal probability", 3)
row.names(geoKendall_model_q_sum$summary_params) <- rep("Tied rank", 3)
tbl6 = rbind(geoKendall_model_eq_sum$summary_params,
			 geoKendall_model_q_sum$summary_params)
message("Table 6")
tbl6


# Code for Chapter 7.3: Test of general knowledge 
# Produce Table 7
load("CogSurvey.RData")
set.seed(777)
cog_wk_ctrl <- new("RankControlWeightedKendall",
				   SearchPi0_fast_traversal = TRUE,
				   SearchPi0_show_message = FALSE)
autoInit <- function(dat) {
	init <- new("RankInit", param.init = list(rep(0.1, dat@nobj-1)),
				modal_ranking.init = list(sample(dat@nobj, dat@nobj)), clu = 1L)
	init
}
rank_init_list <- lapply(rank_data_list, autoInit)
agg_model <- mapply(RankDistanceModel, rank_data_list, rank_init_list,
					MoreArgs = list(cog_wk_ctrl), SIMPLIFY = FALSE)
dist_to_truth <- function(dat, model) {
	dist <- list()
	truth <- seq_len(dat@nobj)
	dist$obs_truth <- DistanceBlock(dat@ranking, truth)
	borda <- order(order(colSums(dat@ranking)))
	dist$borda_truth <- DistancePair(truth, borda)
	dist$model_truth <- DistancePair(model$modal_ranking.est[[1]], truth)
	dist
}
d <- mapply(dist_to_truth, rank_data_list, agg_model, SIMPLIFY = FALSE)
BetterObs <- sapply(d, function(x) sum(x$obs_truth < x$model_truth)/length(x$obs_truth))
DistToTruth <- sapply(d, function(x)x$model_truth)
BordaToTruth <- sapply(d, function(x)x$borda_truth)

data_summary = sapply(rank_data_list, function(x) c(nobj=x@nobj, nobs=x@nobs))
summary_table = data.frame(t(data_summary), BetterObs, DistToTruth, BordaToTruth)
summary_table = summary_table[order(summary_table$nobj), ]
message("Table 7")
summary_table

###################### PRODUCE ALL TABLES ##########################
message("Printing all tables")
message("Table 1")
print(result1)

message("Table 2")
print(result2)

message("Table 3")
print(tbl3)

message("Table 4")
geoKendall_model_sum$summary_params
Kendall_model_sum$summary_params
wt_model_sum$summary_params

message("Table 5")
print(tbl5)

message("Table 6")
print(tbl6)

message("Table 7")
print(summary_table)
