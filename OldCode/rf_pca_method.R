library(tidyverse)
data<-vi_list_gt20[[5]]

data<- data %>%
  select(where(is.numeric)) %>%
  select(-any_of("Date_1"))%>%
select(any_of(c("kNDVI", "NDVI", "EVI", "LSWI", "NDWI", "NIRv", "NIR")))
pr.out = prcomp(data, scale=FALSE)
pr.out
biplot(pr.out,scale=0,var.axes = TRUE)


# data standardization
for (i in 1:ncol(X)){
  X[,i]=(X[,i]-mean(X[,i]))/sqrt(var(X[,i]))
}
pr.out = prcomp(data, scale=FALSE)

PVE = pr.out$sdev^2/sum(pr.out$sdev^2)
barplot(PVE, names=paste("Z", 1:6, sep=" "))

PVE = round(PVE,2)
par(mar=c(4,4,4,4))
pc = barplot(PVE,
             width = 1, space = 0.2, border = NA, axes = F,
             ylim = c(0, 1.05 * max(PVE, na.rm = T)), 
             ylab = "PVE" , cex.names = 0.7, 
             names.arg = paste("PC", 1:length(PVE), sep=" "),
             main = "Pareto Chart")
## anotate left axis
axis(side = 2, at = c(0, PVE), las = 1, col.axis = "black", col = "grey62", tick = T, cex.axis = 0.8)
## frame plot
box( col = "grey62")

## Cumulative Frequency Lines 
px <- cumsum(PVE) * max(PVE, na.rm = T)
lines(pc, px, type = "b", cex = 0.7, pch = 19, col="cyan4")
## Annotate Right Axis
axis(side = 4, at = c(0, px), labels = paste(c(0, round(cumsum(PVE)* 100)) ,"%",sep=""), 
     las = 1, col.axis = "grey62", col = "cyan4", cex.axis = 0.8, col.axis = "cyan4")




library(tidyverse)

# List of target VI columns
vi_cols <- c("kNDVI", "NDVI", "EVI", "LSWI", "NDWI", "NIRv", "NIR")

# Initialize list to store PCA results
pca_results <- list()

# Loop through each data frame in vi_list_gt20
for (i in seq_along(vi_list_gt20)) {
  
  data <- vi_list_gt20[[i]] %>%
    select(where(is.numeric)) %>%
    select(-any_of("Date_1")) %>%
    select(any_of(vi_cols))
  
  # Skip iteration if less than 2 valid columns
  if (ncol(data) < 2) {
    warning(paste("Skipping index", i, "- not enough valid columns for PCA"))
    next
  }
  
  # Run PCA
  pr.out <- prcomp(data, scale = FALSE)
  
  # Store the PCA result in the list
  pca_results[[i]] <- pr.out
  
  # (Optional) Display summary or scree plot here if needed
  # summary(pr.out)
  # biplot(pr.out)
}

# Now you can access PCA results like:
# pca_results[[1]], pca_results[[2]], etc.
pca_results

##################################################################
####### Bring the PCA results to linear regression  ############
##################################################################
pca_results1<-as.data.frame(pca_results[[1]]$rotation)
pca_results1$PC1[[1]]

pca_results[[256]]

# Summarize PDDOY from each data frame in vi_list
summary_df <- bind_rows(lapply(seq_along(vi_list_gt20), function(i) {
  df <- vi_list_gt20[[i]]
  
  # Check if column exists and is numeric
  if ("PDDOY" %in% names(df) && is.numeric(df$PDDOY)) {
    tibble(ID = i, PDDOY = mean(df$PDDOY, na.rm = TRUE))
  } else {
    tibble(ID = i, PDDOY = NA_real_)
  }
}))
View(summary_df)
pca_results[[1]]
# List of variable names you want PC1 loadings for
vars <- c("kNDVI", "NDVI", "EVI", "LSWI", "NDWI", "NIRv")

# Extract PC1 loadings for each variable across all pca_results
pc1_loadings <- lapply(vars, function(var) {
  sapply(pca_results, function(pca) {
    if (!is.null(pca) && var %in% rownames(pca$rotation)) {
      pca$rotation[var, "PC1"]
    } else {
      NA_real_
    }
  })
})

# Combine into a data.frame and rename columns
pc1_df <- as.data.frame(pc1_loadings)
colnames(pc1_df) <- paste0(vars, "_PC1")

# Combine with summary_df
summary_df <- bind_cols(summary_df, pc1_df)
View(summary_df)


n = nrow(summary_df)
train.case = sample(n, 450, replace=FALSE)
test.case = c(1:n)[-train.case]

train.data = summary_df[train.case,]
test.data = summary_df[test.case,]
# Build the model
fit.pcr = lm(PDDOY~., train.data)
fit.pcr

summary(fit.pcr)

pred = predict(fit.pcr, test.data)
pred = predict(fit.pcr, test.data)

obs = test.data[,"PDDOY"] # the true lpsa values

plot(obs$PDDOY, pred,
     xlab = "Observed PDDOY",
     ylab = "Predicted PDDOY",
     main = "Observed vs. Predicted PDDOY")
abline(0, 1, col = "red", lty = 2)  # Add 1:1 line

# Predict and observe
pred <- predict(fit.pcr, test.data)
obs <- test.data[,"PDDOY"]

# Compute R² and RMSE
r2 <- cor(obs, pred)^2
rmse <- sqrt(mean((obs - pred)^2))

# Plot
plot(obs, pred,
     xlab = "Observed PDDOY",
     ylab = "Predicted PDDOY",
     main = "Observed vs. Predicted PDDOY")
abline(0, 1, col = "red", lty = 2)  # 1:1 line

# Add R² and RMSE to the plot
text(x = min(obs), y = max(pred), 
     labels = paste0("R² = ", round(r2, 2)),
     pos = 4, cex = 0.9)


mse[i] = mean( (pred-obs)^2 )
boxplot(mse)
summary(mse)

mse


plot(summary_df$PDDOY, summary_df$PDDOY)
