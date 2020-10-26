#0.01
rSet_AS_signals_oe_2000 <- sracipeSimulate(topology_AS, integrate = FALSE, numModels = 2000)

rSet_AS_parameters_values_2000 <- sracipeParams(rSet_AS_signals_oe_2000)

testest <- sracipeSimulate(demoCircuit)
rSet_AS_parameters_values_2000[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(2000, 90, 100), runif(2000, 90, 100), runif(2000, 90, 100))
head(rSet_AS_parameters_values_2000[, c("G_CH", "G_PD", "G_LIF")])

sracipeParams(rSet_AS_signals_oe_2000) <- rSet_AS_parameters_values_2000

rSet_AS_signals_oe_2000_01 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .01, numModels = 2000)

rSet_AS_signals_oe_expression_2000_01 <- assay(rSet_AS_signals_oe_2000_01)

rSet_AS_signals_oe_expression_2000_01_t <- t(rSet_AS_signals_oe_expression_2000_01)

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_01_t)) {
  hist(rSet_AS_signals_oe_expression_2000_01_t[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_01_t)[i]))
}


rSet_AS_signals_oe_expression_2000_01_t_filtered <- rSet_AS_signals_oe_expression_2000_01_t[complete.cases(rSet_AS_signals_oe_expression_2000_01_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_01_t_filtered <- rSet_AS_signals_oe_expression_2000_01_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_01_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_01_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_01_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_01_t_filtered)[i]))
}

test_01 <- rSet_AS_signals_oe_expression_2000_01_t > 1000
head(rSet_AS_signals_oe_expression_2000_01_t > 1000)
test_01 <- if_na(test_01, "NAN", label = NULL)

sum(as.character(test_01) != "FALSE")


#0.005
rSet_AS_signals_oe_2000_005 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .005, numModels = 2000)

rSet_AS_signals_oe_expression_2000_005 <- assay(rSet_AS_signals_oe_2000_005)

rSet_AS_signals_oe_expression_2000_005_t <- t(rSet_AS_signals_oe_expression_2000_005)

rSet_AS_signals_oe_expression_2000_005_t_filtered <- rSet_AS_signals_oe_expression_2000_005_t[complete.cases(rSet_AS_signals_oe_expression_2000_005_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_005_t_filtered <- rSet_AS_signals_oe_expression_2000_005_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_005_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_005_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_005_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_005_t_filtered)[i]))
}

test_005 <- rSet_AS_signals_oe_expression_2000_005_t > 1000
head(rSet_AS_signals_oe_expression_2000_005_t > 1000)
test_005 <- if_na(test_005, "NAN", label = NULL)

sum(as.character(test_005) != "FALSE")

#0.001
rSet_AS_signals_oe_2000_001 <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, integrateStepSize = .001, numModels = 2000)

rSet_AS_signals_oe_expression_2000_001 <- assay(rSet_AS_signals_oe_2000_001)

rSet_AS_signals_oe_expression_2000_001_t <- t(rSet_AS_signals_oe_expression_2000_001)

rSet_AS_signals_oe_expression_2000_001_t_filtered <- rSet_AS_signals_oe_expression_2000_001_t[complete.cases(rSet_AS_signals_oe_expression_2000_001_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_001_t_filtered <- rSet_AS_signals_oe_expression_2000_001_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_001_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_001_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_001_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_001_t_filtered)[i]))
}

test_001 <- rSet_AS_signals_oe_expression_2000_001_t > 1000
head(rSet_AS_signals_oe_expression_2000_001_t > 1000)
test_001 <- if_na(test_001, "NAN", label = NULL)

sum(as.character(test_001) != "FALSE")


#DP
rSet_AS_signals_oe_2000_DP <- sracipeSimulate(rSet_AS_signals_oe_2000, integrate = TRUE, genParams = F, stepper = "DP", numModels = 2000)

rSet_AS_signals_oe_expression_2000_DP <- assay(rSet_AS_signals_oe_2000_DP)

rSet_AS_signals_oe_expression_2000_DP_t <- t(rSet_AS_signals_oe_expression_2000_DP)

test_DP <- rSet_AS_signals_oe_expression_2000_DP_t > 1000
head(rSet_AS_signals_oe_expression_2000_DP_t > 1000)
sum(as.character(test_DP) != "FALSE")

rSet_AS_signals_oe_expression_2000_DP_t_filtered <- rSet_AS_signals_oe_expression_2000_DP_t[complete.cases(rSet_AS_signals_oe_expression_2000_DP_t), ] #remove NAs

rSet_AS_signals_oe_expression_2000_DP_t_filtered <- rSet_AS_signals_oe_expression_2000_DP_t_filtered[(rowSums(rSet_AS_signals_oe_expression_2000_DP_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered)[i]))
}
dim(rSet_AS_signals_oe_expression_2000_01_t_filtered)

pca <- prcomp(rSet_AS_signals_oe_expression_2000_01_t_filtered)

densityPlot(data.frame(pca$x[,1:2]))
densityPlot(data.frame(rSet_AS_signals_oe_expression_2000_01_t_filtered[,1:2]))

type(rSet_AS_signals_oe_expression_2000_01_t_filtered)
test_DP <- if_na(test_DP, "NAN", label = NULL)
head(rSet_AS_signals_oe_expression_2000_DP_t)

min(rSet_AS_signals_oe_expression_2000_DP_t_filtered[unmatrix(rSet_AS_signals_oe_expression_2000_DP_t_filtered) > 0])
rSet_AS_signals_oe_expression_2000_DP_t_filtered_log <- log2(min(rSet_AS_signals_oe_expression_2000_DP_t_filtered[unmatrix(rSet_AS_signals_oe_expression_2000_DP_t_filtered) > 0]) + rSet_AS_signals_oe_expression_2000_DP_t_filtered)

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log)[i]))
}

means_log <- colMeans(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log)
sds_log <- apply(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log, 2, sd)

rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z <- sweep(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log, 
                                                                2, means_log, FUN = "-")

rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z <- sweep(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z, 
                                                                2, sds_log, FUN = "/")

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z)[i]))
}

rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1 <- log2(1 + rSet_AS_signals_oe_expression_2000_DP_t_filtered)

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1)[i]))
}


means_log1 <- colMeans(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1)
sds_log1 <- apply(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1, 2, sd)

rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z <- sweep(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1, 
                                                                2, means_log, FUN = "-")

rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z <- sweep(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z, 
                                                                2, sds_log, FUN = "/")

for (i in 1:ncol(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z)) {
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log1_z)[i]))
}

head(rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z)
#variables in columns and observations in rows
#center + scale does z-score calculation on data, which I already did
#center subracts colMeans from each col
#scale divides by standard deviation
#PCA
prcomp_2000_DP_t_filtered_log_z <- prcomp(x = rSet_AS_signals_oe_expression_2000_DP_t_filtered_log_z, center = F, scale. = F)

screeplot_2000_DP_t_filtered_log_z <- prcomp_2000_DP_t_filtered_log_z$sdev^2/sum(prcomp_2000_DP_t_filtered_log_z$sdev^2)*100
screeplot_2000_DP_t_filtered_log_z <- data.frame(PCs = factor(colnames(prcomp_2000_DP_t_filtered_log_z$x), levels = colnames(prcomp_2000_DP_t_filtered_log_z$x) ),
                                                               Percent_Variance = screeplot_2000_DP_t_filtered_log_z)
  
screeplot_2000_DP_t_filtered_log_z <- ggplot2::ggplot(data = screeplot_2000_DP_t_filtered_log_z, aes(x=PCs, y = Percent_Variance)) + geom_bar(stat = "identity")

PCA_2000_DP_t_filtered_log_z <- ggplot(data = as.data.frame(prcomp_2000_DP_t_filtered_log_z$x), aes(x = PC1, y = PC2)) + geom_point()
prcomp_2000_DP_t_filtered_log_z$x

den3d <- kde2d(x = prcomp_2000_DP_t_filtered_log_z$x[, 1], y = prcomp_2000_DP_t_filtered_log_z$x[, 2])
plot_ly(x = den3d$x, y = den3d$y, z = den3d$z) %>% add_surface()

#general 10000 models with randomized signals
rSet_AS_10000 <- sracipeSimulate(topology_AS, integrate = TRUE, numModels = 10000, integrateStepSize = 0.01)

rSet_AS_10000_expression_01_t <- t(assay(rSet_AS_10000))

for (i in 1:ncol(rSet_AS_10000_expression_01_t)) {
  hist(rSet_AS_10000_expression_01_t[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_10000_expression_01_t)[i]))
}

rSet_AS_10000_expression_01_t_filtered <- rSet_AS_10000_expression_01_t[complete.cases(rSet_AS_10000_expression_01_t), ] #remove NAs

rSet_AS_10000_expression_01_t_filtered <- rSet_AS_10000_expression_01_t_filtered[(rowSums(rSet_AS_10000_expression_01_t_filtered) > 1000) == 0, ] #remove all rows that contain at least 1 or more values greater than 1000

for (i in 1:ncol(rSet_AS_10000_expression_01_t_filtered)) {
  hist(rSet_AS_10000_expression_01_t_filtered[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_10000_expression_01_t_filtered)[i]))
}

test_01_10000 <- rSet_AS_10000_expression_01_t > 1000
test_01_10000 <- if_na(test_01_10000, "NAN", label = NULL)

which(complete.cases(rSet_AS_10000_expression_01_t) == FALSE)

sum(as.character(test_01_10000) != "FALSE")

hist(rSet_AS_10000_expression_01_t_filtered,
     col=rgb(1,0,0,0.5), ylim=c(0,10000), main="Overlapping Histogram", xlab="Variable")
hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered, col=rgb(0,0,1,0.5), add=T)
box()

for (i in 1:ncol(rSet_AS_10000_expression_01_t_filtered)) {
  hist(rSet_AS_10000_expression_01_t_filtered[, i],
       col=rgb(1,0,0,0.5), ylim=c(0,10000), main=paste(colnames(rSet_AS_10000_expression_01_t)[i]), xlab="Expression")
  hist(rSet_AS_signals_oe_expression_2000_DP_t_filtered[, i], col=rgb(0,0,1,0.5), add=T)
  box()
}

min_expression <- min(rSet_AS_10000_expression_01_t_filtered[unmatrix(rSet_AS_10000_expression_01_t_filtered) > 0])
rSet_AS_10000_expression_01_t_filtered_log <- log2(min_expression + rSet_AS_10000_expression_01_t_filtered)

for (i in 1:ncol(rSet_AS_10000_expression_01_t_filtered_log)) {
  hist(rSet_AS_10000_expression_01_t_filtered_log[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_10000_expression_01_t_filtered_log)[i]))
}

means_log_10000 <- colMeans(rSet_AS_10000_expression_01_t_filtered_log)
sds_log_10000 <- apply(rSet_AS_10000_expression_01_t_filtered_log, 2, sd)

rSet_AS_10000_expression_01_t_filtered_log_z <- sweep(rSet_AS_10000_expression_01_t_filtered_log, 
                                                                2, means_log, FUN = "-")

rSet_AS_10000_expression_01_t_filtered_log_z <- sweep(rSet_AS_10000_expression_01_t_filtered_log_z, 
                                                                2, sds_log, FUN = "/")

for (i in 1:ncol(rSet_AS_10000_expression_01_t_filtered_log_z)) {
  hist(rSet_AS_10000_expression_01_t_filtered_log_z[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_10000_expression_01_t_filtered_log_z)[i]))
}

prcomp_10000_expression_01_t_filtered_log_z <- prcomp(x = rSet_AS_10000_expression_01_t_filtered_log_z, center = F, scale. = F)
prcomp_10000_expression_01_t_filtered_log <- prcomp(x = rSet_AS_10000_expression_01_t_filtered_log, center = T, scale. = T)

screeplot_10000_expression_01_t_filtered_log_z <- prcomp_10000_expression_01_t_filtered_log_z$sdev^2/sum(prcomp_10000_expression_01_t_filtered_log_z$sdev^2)*100
screeplot_10000_expression_01_t_filtered_log_z <- data.frame(PCs = factor(colnames(prcomp_10000_expression_01_t_filtered_log_z$x), levels = colnames(prcomp_10000_expression_01_t_filtered_log_z$x) ),
                                                 Percent_Variance = screeplot_10000_expression_01_t_filtered_log_z)

screeplot_10000_expression_01_t_filtered_log_z <- ggplot2::ggplot(data = screeplot_10000_expression_01_t_filtered_log_z, aes(x=PCs, y = Percent_Variance)) + geom_bar(stat = "identity")

PCA_10000_expression_01_t_filtered_log_z <- ggplot(data = as.data.frame(prcomp_10000_expression_01_t_filtered_log_z$x), aes(x = PC1, y = PC2)) + geom_point()
PCA_10000_expression_01_t_filtered_log <- ggplot(data = as.data.frame(prcomp_10000_expression_01_t_filtered_log$x), aes(x = PC1, y = PC2)) + geom_point()


den3d <- kde2d(x = prcomp_10000_expression_01_t_filtered_log_z$x[, 1], y = prcomp_10000_expression_01_t_filtered_log_z$x[, 2])
plot_ly(x = den3d$x, y = den3d$y, z = den3d$z) %>% add_surface()


#generate 10000 OE models

rSet_AS_10000_oe <- rSet_AS_10000
rSet_AS_parameters_values_10000_oe <- sracipeParams(rSet_AS_10000_oe)

rSet_AS_parameters_values_10000_oe[, c("G_CH", "G_PD", "G_LIF")] <- cbind(runif(10000, 90, 100), runif(10000, 90, 100), runif(10000, 90, 100))
head(rSet_AS_parameters_values_10000_oe[, c("G_CH", "G_PD", "G_LIF")])

sracipeParams(rSet_AS_10000_oe) <- rSet_AS_parameters_values_10000_oe

rSet_AS_signals_oe_10000_01 <- sracipeSimulate(rSet_AS_10000_oe, integrate = TRUE, genParams = F, integrateStepSize = .01, numModels = 10000)

rSet_AS_signals_oe_10000_01_expression <- t(assay(rSet_AS_signals_oe_10000_01))

dim(which(is.na(rSet_AS_signals_oe_10000_01_expression), arr.ind = T))
dim(which(rSet_AS_signals_oe_10000_01_expression > 1000, arr.ind = T))
dim(which(rSet_AS_signals_oe_10000_01_expression > 1000 | is.na(rSet_AS_signals_oe_10000_01_expression) , arr.ind = T))
filter <- which(rSet_AS_signals_oe_10000_01_expression > 1000 | is.na(rSet_AS_signals_oe_10000_01_expression) , arr.ind = T)[,"row"]
filter <- filter[-which(duplicated(filter))]

rSet_AS_signals_oe_10000_01_expression_filtered <- rSet_AS_signals_oe_10000_01_expression[-filter, ]

for (i in 1:ncol(rSet_AS_signals_oe_10000_01_expression_filtered)) {
  hist(rSet_AS_signals_oe_10000_01_expression_filtered[, i], breaks = 100, ylim = c(0,8000), main = paste(colnames(rSet_AS_signals_oe_10000_01_expression_filtered)[i]))
}

for (i in 1:ncol(rSet_AS_10000_expression_01_t_filtered)) {
  hist(log2(rSet_AS_10000_expression_01_t_filtered)[, i],
       col=rgb(1,0,0,0.5), ylim=c(0,10000), main=paste(colnames(rSet_AS_10000_expression_01_t)[i]), xlab="Expression")
  hist(log2(rSet_AS_signals_oe_10000_01_expression_filtered)[, i], col=rgb(0,0,1,0.5), add=T)
  box()
}

min_expression <- min(rSet_AS_signals_oe_10000_01_expression_filtered[unmatrix(rSet_AS_signals_oe_10000_01_expression_filtered) > 0])
rSet_AS_signals_oe_10000_01_expression_filtered_log <- log2(min_expression + rSet_AS_signals_oe_10000_01_expression_filtered)

for (i in 1:ncol(rSet_AS_signals_oe_10000_01_expression_filtered_log)) {
  hist(rSet_AS_signals_oe_10000_01_expression_filtered_log[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_10000_01_expression_filtered_log)[i]))
}

means_log <- colMeans(rSet_AS_signals_oe_10000_01_expression_filtered_log)
sds_log <- apply(rSet_AS_signals_oe_10000_01_expression_filtered_log, 2, sd)

rSet_AS_signals_oe_10000_01_expression_filtered_log_z <- sweep(rSet_AS_signals_oe_10000_01_expression_filtered_log, 
                                                      2, means_log, FUN = "-")

rSet_AS_signals_oe_10000_01_expression_filtered_log_z <- sweep(rSet_AS_signals_oe_10000_01_expression_filtered_log_z, 
                                                      2, sds_log, FUN = "/")

for (i in 1:ncol(rSet_AS_signals_oe_10000_01_expression_filtered_log_z)) {
  hist(rSet_AS_signals_oe_10000_01_expression_filtered_log_z[, i], breaks = 100, ylim = c(0,200), main = paste(colnames(rSet_AS_signals_oe_10000_01_expression_filtered_log_z)[i]))
}

prcomp_signals_oe_10000_01_expression_filtered_log_z <- prcomp(x = rSet_AS_signals_oe_10000_01_expression_filtered_log_z, center = F, scale. = F)
prcomp_signals_oe_10000_01_expression_filtered_log <- prcomp(x = rSet_AS_signals_oe_10000_01_expression_filtered_log, center = T, scale. = T)

screeplot_signals_oe_10000_01_expression_filtered_log_z <- prcomp_signals_oe_10000_01_expression_filtered_log_z$sdev^2/sum(prcomp_signals_oe_10000_01_expression_filtered_log_z$sdev^2)*100
screeplot_signals_oe_10000_01_expression_filtered_log_z <- data.frame(PCs = factor(colnames(prcomp_signals_oe_10000_01_expression_filtered_log_z$x), levels = colnames(prcomp_signals_oe_10000_01_expression_filtered_log_z$x) ),
                                                             Percent_Variance = screeplot_signals_oe_10000_01_expression_filtered_log_z)

screeplot_signals_oe_10000_01_expression_filtered_log_z <- ggplot2::ggplot(data = screeplot_signals_oe_10000_01_expression_filtered_log_z, aes(x=PCs, y = Percent_Variance)) + geom_bar(stat = "identity")

PCA_signals_oe_10000_01_expression_filtered_log_z <- ggplot(data = as.data.frame(prcomp_signals_oe_10000_01_expression_filtered_log_z$x), aes(x = PC1, y = PC2)) + geom_point()
PCA_signals_oe_10000_01_expression_filtered_log <- ggplot(data = as.data.frame(prcomp_signals_oe_10000_01_expression_filtered_log$x), aes(x = PC1, y = PC2)) + geom_point()


den3d <- kde2d(x = prcomp_signals_oe_10000_01_expression_filtered_log_z$x[, 1], y = prcomp_signals_oe_10000_01_expression_filtered_log_z$x[, 2])
plot_ly(x = den3d$x, y = den3d$y, z = den3d$z) %>% add_surface()

#50 to 100,

#generate zscore using mean/sd from general models
means_log_10000 <- colMeans(rSet_AS_10000_expression_01_t_filtered_log)
sds_log_10000 <- apply(rSet_AS_10000_expression_01_t_filtered_log, 2, sd)

rSet_AS_signals_oe_10000_01_expression_filtered_log_z_normal <- sweep(rSet_AS_signals_oe_10000_01_expression_filtered_log, 
                                                                      2, means_log_10000, FUN = "-")

rSet_AS_signals_oe_10000_01_expression_filtered_log_z_normal <- sweep(rSet_AS_signals_oe_10000_01_expression_filtered_log_z_normal, 
                                                                      2, sds_log_10000, FUN = "/")

mat_multiply <- rSet_AS_signals_oe_10000_01_expression_filtered_log_z_normal %*% prcomp_10000_expression_01_t_filtered_log$rotation


ggplot(data = as.data.frame(mat_multiply), aes(x = PC1, y = PC2)) + geom_point() + geom_point(data = as.data.frame(prcomp_10000_expression_01_t_filtered_log$x),
                                                                                      aes(x = PC1, y = PC2), color = "red", alpha = 0.02)

#50k models
rSet_AS_50000 <- sracipeSimulate(topology_AS, integrate = TRUE, numModels = 50000)
#sracipePlotData(rSet_AS_50000, plotToFile = T)

OE_rows <- which(sracipeParams(rSet_AS_50000)[,"G_CH"] > 50 & sracipeParams(rSet_AS_50000)[,"G_PD"] > 50 & sracipeParams(rSet_AS_50000)[,"G_LIF"] > 50, arr.ind = T)

rSet_AS_50000_expression <- t(assay(rSet_AS_50000))
rSet_AS_50000_expression_OE50 <- rSet_AS_50000_expression[OE_rows, ]

for (i in 1:ncol(rSet_AS_50000_expression)) {
  hist(rSet_AS_50000_expression[, i],
       col=rgb(1,0,0,0.5), ylim=c(0,10000), main=paste(colnames(rSet_AS_50000_expression)[i]), xlab="Expression")
  hist(rSet_AS_50000_expression_OE50[, i], col=rgb(0,0,1,0.5), add=T)
  box()
}

for (i in 1:ncol(rSet_AS_50000_expression)) {
  hist(log2(rSet_AS_50000_expression)[, i],
       col=rgb(1,0,0,0.5), ylim=c(0,10000), main=paste(colnames(rSet_AS_50000_expression)[i]), xlab="Expression")
  hist(log2(rSet_AS_50000_expression_OE50)[, i], col=rgb(0,0,1,0.5), add=T)
  box()
}

rSet_AS_50000_expression_log <- log2(rSet_AS_50000_expression)
rSet_AS_50000_expression_OE50_log <- log2(rSet_AS_50000_expression_OE50)

means_log_50000 <- colMeans(rSet_AS_50000_expression_log)
sds_log_50000 <- apply(rSet_AS_50000_expression_log, 2, sd)

rSet_AS_50000_expression_log_z <- sweep(rSet_AS_50000_expression_log, 
                                                                      2, means_log_50000, FUN = "-")

rSet_AS_50000_expression_log_z <- sweep(rSet_AS_50000_expression_log_z, 
                                                                      2, sds_log_50000, FUN = "/")

rSet_AS_50000_expression_OE50_log_z <- rSet_AS_50000_expression_log_z[OE_rows, ]

for (i in 1:ncol(rSet_AS_signals_oe_10000_01_expression_log_z_normal)) {
  hist(rSet_AS_signals_oe_10000_01_expression_log_z_normal[, i],
       col=rgb(1,0,0,0.5), ylim=c(0,10000), main=paste(colnames(rSet_AS_signals_oe_10000_01_expression_log_z_normal)[i]), xlab="Expression")
  hist(rSet_AS_50000_expression_OE50_log_z_normal[, i], col=rgb(0,0,1,0.5), add=T)
  box()
}

prcomp_50000_expression_log_z  <- prcomp(x = rSet_AS_50000_expression_log_z, center = F, scale. = F)
PCA_50000_expression_log_z <- ggplot(data = as.data.frame(prcomp_50000_expression_log_z$x), aes(x = PC1, y = PC2)) + geom_point()

ggplot(data = as.data.frame(prcomp_50000_expression_log_z$x), aes(x = PC1, y = PC2)) + geom_point() + geom_point(data = as.data.frame(prcomp_50000_expression_log_z$x[OE_rows, ]),
                                                                                              aes(x = PC1, y = PC2), color = "red", alpha = 0.2)

Density <- ggplot(data = as.data.frame(prcomp_50000_expression_log_z$x), aes(x = PC1, y = PC2) ) +
  geom_hex(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

Density_OE <- ggplot(data = as.data.frame(prcomp_50000_expression_log_z$x[OE_rows, ]), aes(x = PC1, y = PC2) ) +
  geom_hex(bins = 150) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

gridExtra::grid.arrange(Density, Density_OE, ncol = 2)

test_minpts12 <- dbscan::optics(prcomp_50000_expression_log_z$x, eps = 20, minPts = 12 )

plot(test_minpts18)
plot(test_minpts12)

for (i in 1:ncol(rSet_AS_50000_expression_log_z)) {
  print(i)
plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_50000_expression_log_z[, i]), aes(x = expression)) + 
  geom_density() + geom_density(data = data.frame(expression = rSet_AS_50000_expression_OE50_log_z[, i]), aes(x = expression),
    color = "red") + geom_density(data = data.frame(expression = rSet_AS_signals_oe_10000_01_expression_filtered_log_z[, i]), aes(x = expression),
                                                            color = "green") + labs(title = paste(colnames(rSet_AS_50000_expression_log_z)[i]))
print(plot)
}

rSet_AS_signals_oe_10000_01_expression_filtered_log_z

OE_rows_60 <- which(sracipeParams(rSet_AS_50000)[,"G_CH"] > 60 & sracipeParams(rSet_AS_50000)[,"G_PD"] > 60 & sracipeParams(rSet_AS_50000)[,"G_LIF"] > 60, arr.ind = T)
UE_rows_40 <- which(sracipeParams(rSet_AS_50000)[,"G_CH"] < 40 & sracipeParams(rSet_AS_50000)[,"G_PD"] < 40 & sracipeParams(rSet_AS_50000)[,"G_LIF"] < 40, arr.ind = T)

for (i in 1:ncol(rSet_AS_50000_expression_log_z)) {
  
  plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_50000_expression_log_z[, i]), aes(x = expression)) + 
    geom_density() + geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[OE_rows_60, ][, i]), aes(x = expression),
                                  color = "red") + geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[UE_rows_40, ][, i]), aes(x = expression),
                                                                color = "green") + labs(title = paste(colnames(rSet_AS_50000_expression_log_z)[i]))
  print(plot)
}

plotColor <- c("#5E4FA2", "#4F61AA", "#4173B3", "#3386BC", "#4198B6",
                             "#51ABAE", "#62BEA6", "#77C8A4", "#8ED1A4", "#A4DAA4",
                             "#B8E2A1", "#CBEA9D", "#DEF199", "#EAF69F", "#F2FAAC",
                             "#FAFDB8", "#FEFAB6", "#FEF0A5", "#FEE695", "#FDD985",
                             "#FDC978", "#FDB96A", "#FCA75E", "#F99254", "#F67D4A",
                             "#F26943", "#E85A47", "#DE4B4B", "#D33C4E", "#C1284A",
                             "#AF1446", "#9E0142")
heatmap_euc_D2_5000 <- heatmaply(rSet_AS_50000_expression_log_z[1:5000, ], dist_method = "euclidean", hclust_method ="ward.D2", colors = plotColor, k_row = 4, dendrogram = "row")

distance_euc_8000 <- dist(rSet_AS_50000_expression_log_z[1:8000, ])
clusters_euc_D2_8000 <- hclust(distance_euc_8000, method = "ward.D2")
assignedClusters_euc_D2_8000 <- cutree(clusters_euc_D2_8000, 4)

for (i in 1:ncol(rSet_AS_50000_expression_log_z)) {
  
  plot <- ggplot2::ggplot(data = data.frame(expression = rSet_AS_50000_expression_log_z[, i]), aes(x = expression)) + 
    geom_density() + geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[1:8000, ][which(assignedClusters_euc_D2_8000 == 1), i]), aes(x = expression), color = "red") + 
    geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[1:8000, ][which(assignedClusters_euc_D2_8000 == 2), i]), aes(x = expression), color = "green") +
    geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[1:8000, ][which(assignedClusters_euc_D2_8000 == 3), i]), aes(x = expression), color = "blue") +
    geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[1:8000, ][which(assignedClusters_euc_D2_8000 == 4), i]), aes(x = expression), color = "yellow") +
    geom_density(data = data.frame(expression = rSet_AS_50000_expression_log_z[UE_rows_40, ][, i]), aes(x = expression), color = "purple") +
    labs(title = paste(colnames(rSet_AS_50000_expression_log_z)[i]))
  
  print(plot)
}


distance_euc_10000 <- dist(rSet_AS_50000_expression_log_z[1:10000, ])
clusters_euc_D2_10000 <- hclust(distance_euc_10000, method = "ward.D2")
assignedClusters_euc_D2_10000 <- cutree(clusters_euc_D2_10000, 4)

distance_euc_20000 <- dist(rSet_AS_50000_expression_log_z[1:20000, ])
clusters_euc_D2_20000 <- hclust(distance_euc_20000, method = "ward.D2")
assignedClusters_euc_D2_20000 <- cutree(clusters_euc_D2_20000, 4)

distance_euc_40000 <- dist(rSet_AS_50000_expression_log_z[1:40000, ])
clusters_euc_D2_40000 <- hclust(distance_euc_40000, method = "ward.D2")
assignedClusters_euc_D2_40000 <- cutree(clusters_euc_D2_40000, 4)



binarized <- binarize.kMeans(rSet_AS_50000_expression_log_z[,1], dip.test = TRUE)
Binarize::plot(binarized, twoDimensional = T)

rSet_AS_50000_expression_log_z_binarized <- rSet_AS_50000_expression_log_z

for (i in 1:ncol(rSet_AS_50000_expression_log_z)) {
  
  binarized <- binarize.kMeans(rSet_AS_50000_expression_log_z[,i], dip.test = TRUE)
  rSet_AS_50000_expression_log_z_binarized[, i] <- binarized@binarizedMeasurements
  
}
dim(unique(rSet_AS_50000_expression_log_z_binarized))

distance_euc_50000 <- dist(rSet_AS_50000_expression_log_z)
clusters_euc_D2_50000 <- hclust(distance_euc_50000, method = "ward.D2")
assignedClusters_euc_D2_8000 <- cutree(clusters_euc_D2_8000, 4)

which(assignedClusters_euc_D2_8000 == 1)
