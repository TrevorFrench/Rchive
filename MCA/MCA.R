data = read.csv("modified_census_data.csv", header = TRUE, stringsAsFactors = FALSE)

require(FactoMineR)
require(ggplot2)

scope1 = data[, c("education", "marital_status", "native_country", "occupation", "race", "relationship", "sex", "workLABEL")]

cats1 = apply(scope1, 2, function(x) nlevels(as.factor(x)))

mca2 = MCA(scope, graph = FALSE)

mca2$eig

# data frame with variable coordinates
mca2_vars_df = data.frame(mca2$var$coord, Variable = rep(names(cats1), cats1))

# data frame with observation coordinates
mca2_obs_df = data.frame(mca2$ind$coord)

# plot of variable categories
ggplot(data=mca2_vars_df, 
       aes(x = Dim.1, y = Dim.2, label = rownames(mca2_vars_df))) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_text(check_overlap=TRUE, aes(colour=Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR")

# MCA plot of observations and categories
ggplot(data = mca2_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray80", alpha = 0.7) +
  geom_density2d(colour = "gray50") +
  geom_text(check_overlap=TRUE,
            data = mca2_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca2_vars_df), colour = Variable)) +
  ggtitle("Multiple Correspondence Analysis of Social Conditions") +
  scale_colour_discrete(name = "Variable")
