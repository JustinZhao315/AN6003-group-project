library(data.table)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)

library(clustMixType)
library(tidyverse)
library(cluster)


dt = fread("football injury.csv",stringsAsFactors = TRUE)
dt$Warmup_Routine_Adherence=factor(dt$Warmup_Routine_Adherence,levels = c(0,1),labels = c("not adhered","adhered"))
dt$Injury_Next_Season =factor(dt$Injury_Next_Season ,levels = c(0,1),labels = c("not injured","injured"))
dt

summary(dt)

### DESCRIPTIVE STATISTICS

## Physical Attributes
ggplot(dt,aes(x=Age,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Age") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Height_cm,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Height") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Weight_kg,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Weight") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=BMI,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by BMI") + theme_bw() + theme(panel.border = element_blank())

# --- Clean labels for consistency ---
dt <- dt %>%
  mutate(Injury_Next_Season = case_when(
    Injury_Next_Season %in% c("injured", "Injured") ~ "Injured",
    Injury_Next_Season %in% c("not injured", "Not Injured") ~ "Not Injured",
    TRUE ~ as.character(Injury_Next_Season)
  )) %>%
  mutate(Injury_Next_Season = factor(Injury_Next_Season,
                                     levels = c("Not Injured","Injured")))

base_theme <- theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  )

# 1) Age
p1 <- ggplot(dt, aes(x = Injury_Next_Season, y = Age, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Age",
       x = NULL, y = "Age") +
  base_theme + theme(legend.position = "none")

# 2) Height
p2 <- ggplot(dt, aes(x = Injury_Next_Season, y = Height_cm, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Height",
       x = NULL, y = "Height (cm)") +
  base_theme + theme(legend.position = "none")

# 3) Weight
p3 <- ggplot(dt, aes(x = Injury_Next_Season, y = Weight_kg, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Weight",
       x = NULL, y = "Weight (kg)") +
  base_theme + theme(legend.position = "none")

# 4) BMI
p4 <- ggplot(dt, aes(x = Injury_Next_Season, y = BMI, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "BMI",
       x = NULL, y = "BMI") +
  base_theme + theme(legend.position = "none")

# Combine into 2x2 grid
combined_physical <- (
  p1 | p2
) / (
  p3 | p4
) + plot_layout(guides = "collect", heights = c(1, 1)) & theme(legend.position = "bottom")

combined_physical


## Football-specific Attributes
ggplot(dt, aes(x = Position, fill = Injury_Next_Season)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Injured vs Not Injured by Position") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Training_Hours_Per_Week,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Weekly Training Hours") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Matches_Played_Past_Season,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Matches Played Past Season") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Previous_Injury_Count,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Previous Injury Count") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt, aes(x = Warmup_Routine_Adherence, fill = Injury_Next_Season)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of Injured vs Not Injured by Warmup Routine Adherence") + theme_bw() + theme(panel.border = element_blank())


# --- Clean and prep data ---
dt <- dt %>%
  mutate(Injury_Next_Season = case_when(
    Injury_Next_Season %in% c("injured", "Injured") ~ "Injured",
    Injury_Next_Season %in% c("not injured", "Not Injured") ~ "Not Injured",
    TRUE ~ as.character(Injury_Next_Season)
  )) %>%
  mutate(Injury_Next_Season = factor(Injury_Next_Season, levels = c("Not Injured","Injured")))

base_theme <- theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  )

# 1) Proportion by Position
p1 <- ggplot(dt, aes(x = Position, fill = Injury_Next_Season)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Position", x = NULL, y = "Share") +
  base_theme

# 2) Training Hours per Week
p2 <- ggplot(dt, aes(x = Injury_Next_Season, y = Training_Hours_Per_Week, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Weekly Training Hours", x = NULL, y = "Hours / Week") +
  base_theme + theme(legend.position = "none")

# 3) Matches Played Past Season
p3 <- ggplot(dt, aes(x = Injury_Next_Season, y = Matches_Played_Past_Season, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Matches Played Past Season", x = NULL, y = "Matches") +
  base_theme + theme(legend.position = "none")

# 4) Previous Injury Count
p4 <- ggplot(dt, aes(x = Injury_Next_Season, y = Previous_Injury_Count, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Previous Injury Count", x = NULL, y = "Count") +
  base_theme + theme(legend.position = "none")

# Combine 4 charts (2 rows × 2 columns)
combined <- (
  p1 | p2
) / (
  p3 | p4
) + plot_layout(guides = "collect", heights = c(1, 1)) & theme(legend.position = "bottom")

combined


## Physical Fitness Attributes
attrs <- c(
  "Knee_Strength_Score","Hamstring_Flexibility","Reaction_Time_ms",
  "Balance_Test_Score","Sprint_Speed_10m_s","Agility_Score"
)

dt_long <- dt %>%
  # --- make a clean, robust injury label ---
  mutate(Injury_Next_Season = case_when(
    Injury_Next_Season %in% c("injured") ~ "Injured",
    Injury_Next_Season %in% c("not injured") ~ "Not Injured",
    TRUE ~ NA_character_
  )) %>%
  mutate(Injury_Next_Season = factor(Injury_Next_Season,
                                     levels = c("Not Injured","Injured"))) %>%
  # --- long format for facets ---
  pivot_longer(all_of(attrs), names_to = "Attribute", values_to = "Score") %>%
  mutate(Attribute = recode(Attribute,
                            Knee_Strength_Score   = "Knee Strength",
                            Hamstring_Flexibility = "Hamstring Flexibility",
                            Reaction_Time_ms      = "Reaction Time (ms)",
                            Balance_Test_Score    = "Balance Test",
                            Sprint_Speed_10m_s    = "Sprint Speed (10m, s)",
                            Agility_Score         = "Agility"
  ))

# two boxes per facet (Not Injured vs Injured)
ggplot(dt_long, aes(x = Injury_Next_Season, y = Score, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  facet_wrap(~ Attribute, scales = "free_y", ncol = 3) +
  labs(title = "Physical Fitness Attributes by Next-Season Injury Status",
       x = NULL, y = "Score / Units") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# Health Attributes
ggplot(dt,aes(x=Sleep_Hours_Per_Night,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Sleep Hours Per Night") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Stress_Level_Score,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Stress Level Score") + theme_bw() + theme(panel.border = element_blank())
ggplot(dt,aes(x=Nutrition_Quality_Score,fill=Injury_Next_Season))+
  geom_boxplot() + labs(title = "Distribution of Injured vs Not Injured by Nutrition Quality Score") + theme_bw() + theme(panel.border = element_blank())

# --- Clean labels for consistency ---
dt <- dt %>%
  mutate(Injury_Next_Season = case_when(
    Injury_Next_Season %in% c("injured","Injured") ~ "Injured",
    Injury_Next_Season %in% c("not injured","Not Injured") ~ "Not Injured",
    TRUE ~ as.character(Injury_Next_Season)
  )) %>%
  mutate(Injury_Next_Season = factor(Injury_Next_Season,
                                     levels = c("Not Injured","Injured")))

base_theme <- theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title   = element_text(face = "bold"),
    legend.title = element_blank()
  )

# 1️⃣ Sleep Hours per Night
p1 <- ggplot(dt, aes(x = Injury_Next_Season, y = Sleep_Hours_Per_Night, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Distribution of Injured vs Not Injured by Sleep Hours Per Night",
       x = NULL, y = "Sleep Hours") +
  base_theme + theme(legend.position = "none")

# 2️⃣ Stress Level Score
p2 <- ggplot(dt, aes(x = Injury_Next_Season, y = Stress_Level_Score, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Distribution of Injured vs Not Injured by Stress Level Score",
       x = NULL, y = "Stress Level Score") +
  base_theme + theme(legend.position = "none")

# 3️⃣ Nutrition Quality Score
p3 <- ggplot(dt, aes(x = Injury_Next_Season, y = Nutrition_Quality_Score, fill = Injury_Next_Season)) +
  geom_boxplot(outlier.alpha = 0.25, width = 0.7) +
  labs(title = "Distribution of Injured vs Not Injured by Nutrition Quality Score",
       x = NULL, y = "Nutrition Quality Score") +
  base_theme + theme(legend.position = "none")


# give generous space under each title
p1=p1 + theme(plot.title = element_text(margin = ggplot2::margin(b = 40)))
p2 =p2+ theme(plot.title = element_text(margin = ggplot2::margin(b = 40)))

p3 <- p3 + theme(plot.title = element_text(margin = ggplot2::margin(b = 40)))

library(patchwork)

# shorten titles a bit for clarity
p1 <- p1 + labs(title = "Sleep Hours per Night") +
  theme(plot.title = element_text(margin = ggplot2::margin(b = 35)))
p2 <- p2 + labs(title = "Stress Level Score") +
  theme(plot.title = element_text(margin = ggplot2::margin(b = 35)))
p3 <- p3 + labs(title = "Nutrition Quality Score") +
  theme(plot.title = element_text(margin = ggplot2::margin(b = 35)))

# combine them with proper spacing
combined_lifestyle <- (p1 | p2 | p3) +
  plot_layout(
    guides = "collect",
    widths = c(1, 1, 1),
    design = "
    ABC
    "
  ) &
  theme(
    legend.position = "bottom",
    plot.margin = ggplot2::margin(t = 25, r = 25, b = 25, l = 25)
  )

combined_lifestyle



library(caTools)
set.seed(2025)
train=sample.split(dt$Injury_Next_Season, SplitRatio = 0.7)
trainset <- dt[train == TRUE,]
trainset

testset <- dt[train == FALSE,]

logit=glm(Injury_Next_Season~.,data=trainset,family=binomial)
summary(logit)
pred_prob <- predict(logit, newdata = testset, type = "response")

pred_lab <- ifelse(pred_prob > 0.5, "Injured", "Not Injured")


pred_fac <- factor(pred_lab, levels = levels(testset$Injury_Next_Season))

# Confusion matrix (base R)
cm <- table(Predicted = pred_fac, Actual = testset$Injury_Next_Season)
cm
accuracy <- mean(pred_lab == testset$Injury_Next_Season)
accuracy



library(rpart)
library(rpart.plot)
set.seed(2025)
cart <- rpart(Injury_Next_Season ~ . , data = trainset, method = 'class', cp = 0)

printcp(cart, digits = 3)  ## Turning Point exists.
plotcp(cart)


rpart.plot(
  cart,
  type = 2, extra = 104, fallen.leaves = TRUE,
  tweak = 0.7,     # < 1 shrinks boxes & spacing
  cex   = 0.70,    # smaller fonts
  varlen = 0,    # wrap long split labels
  faclen = -10,
  branch = 0.5
)

# Begin pruning the CART decision tree
CVerror.cap <- cart$cptable[which.min(cart$cptable[,"xerror"]), "xerror"] + cart$cptable[which.min(cart$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap in maximal tree m2.
i <- 1; j<- 4
while (cart$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart$cptable[i,1] * cart$cptable[i-1,1]), 1)

# Get best tree based on 10 fold CV with 1 SE
cart.best <- prune(cart, cp = cp.opt)

printcp(cart.best, digits = 3)

# ----------------------------------------------------------------------------

cart.best$variable.importance
cart.best.scaledVarImpt <- round(100*cart.best$variable.importance/sum(cart.best$variable.importance))
cart.best.scaledVarImpt

# Convert raw variable importance to a dataframe
imp_cart_df <- data.frame(
  Feature = names(cart.best$variable.importance),
  Importance = as.numeric(cart.best$variable.importance)
)

# Sort by importance descending
imp_cart_df <- imp_cart_df[order(imp_cart_df$Importance, decreasing = TRUE), ]

# Plot raw variable importance
p_imp_cart <- ggplot(imp_cart_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "CART Variable Importance",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),   # white panel
    plot.background  = element_rect(fill = "white", color = NA),   # overall background
    panel.grid.major = element_line(color = "gray80"),             # gray major gridlines
    panel.grid.minor = element_line(color = "gray80")              # gray minor gridlines
  )

print(p_imp_cart)



p_imp_cart <- ggplot(imp_cart_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(
    title = "CART Variable Importance (Raw Values)",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal(base_size = 12)

print(p_imp_cart)


cart.best.rules <- rpart.rules(cart.best, nn = T, cover = T)
View(cart.best.rules)  ## View just the decision rules.
print(cart.best) ## View the tree on the console.

printcp(cart.best)  ## View Overall Errors at last row.
rpart.plot(cart.best)

rpart.plot(
  cart.best,
  type = 2, extra = 104, fallen.leaves = TRUE,
  tweak = 0.7, cex = 0.71, varlen = 0, faclen = -10, branch = 0.50
)

# To view surrogates
summary(cart.best)

pred_tree <- predict(cart.best, newdata = testset, type = "class")

# 2) Confusion matrix
cm_tree <- table(Predicted = pred_tree, Actual = testset$Injury_Next_Season)
cm_tree

# 3) Accuracy (and a couple extras)
acc  <- sum(diag(cm_tree)) / sum(cm_tree)
acc

# Pretty confusion matrix
cm_df <- as.data.frame(cm_tree)
p_cm <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Confusion Matrix (Test Set)", x = "Predicted", y = "Actual") +
  theme_minimal()
print(p_cm)

# Pretty confusion matrix with accuracy label
accuracy_text <- paste0("Accuracy: ", round(acc * 100, 1), "%")

p_cm <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(
    title = "Confusion Matrix (Test Set)",
    subtitle = accuracy_text,
    x = "Predicted",
    y = "Actual"
  ) +
  theme_minimal()

print(p_cm)





#bulid random forest model on train set and test set
library(randomForest)

#build model using train set
set.seed(2025)  
RF_train <- randomForest(Injury_Next_Season ~ .,
                         data = trainset,
                         importance = TRUE)
print(RF_train)
var.impt <- importance(RF_train)
var.impt

#predict using test set
RF_predict <- predict(RF_train, newdata = testset)
#Confusion Matrix
confusion_matrix <- table(Predicted = RF_predict, Actual = testset$Injury_Next_Season)
print(confusion_matrix)

#calculate accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Test Accuracy:", accuracy, "\n")

#visualization for Random Forest Model
# (A) OOB error curve from the model trained on TRAIN
plot(RF_train, main = "Random Forest OOB Error vs. Number of Trees (Train/OOB)")
legend("topright", legend = colnames(RF_train$err.rate),
       lty = 1, col = 1:ncol(RF_train$err.rate), cex = 0.8, bty = "n")

# (B) Variable importance (built-in)
varImpPlot(RF_train, main = "Random Forest Variable Importance (Train/OOB)")

# (B.1) ggplot-style variable importance (Top 15 by MeanDecreaseAccuracy)
imp_df <- data.frame(
  Feature = rownames(importance(RF_train)),
  MDA     = importance(RF_train)[, "MeanDecreaseAccuracy"],
  MDG     = importance(RF_train)[, "MeanDecreaseGini"],
  row.names = NULL
)
imp_top <- imp_df[order(imp_df$MDA, decreasing = TRUE), ][1:min(15, nrow(imp_df)), ]
p_imp <- ggplot(imp_top, aes(x = reorder(Feature, MDA), y = MDA)) +
  geom_col(fill="lightblue") + 
  coord_flip() +
  labs(title = "Top Features by Mean Decrease in Accuracy",
       x = "Feature", y = "Mean Decrease Accuracy")
print(p_imp)  # needed when running via source()

# (C) Predict on TEST (class + prob) for fair hold-out evaluation
RF_prob <- predict(RF_train, newdata = testset, type = "prob")

# pick the positive-class column robustly
pos_level <- if ("injured" %in% colnames(RF_prob)) "injured" else colnames(RF_prob)[2]
pred_prob <- RF_prob[, pos_level]

# (D) Confusion-matrix heatmap (ggplot)
cm_df <- as.data.frame(confusion_matrix)
p_cm <- ggplot(cm_df, aes(x = Predicted, y = Actual, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq)) +
  scale_fill_gradient(low = "white", high = "lightblue") +
  labs(title = "Confusion Matrix (Test Set)", x = "Predicted", y = "Actual") +
  theme_minimal()
print(p_cm)


library(clustMixType)
library(cluster)
x <- subset(dt, select = -Injury_Next_Season)
x[] <- lapply(x, function(v) if (is.character(v) || is.logical(v)) factor(v) else v)

d  <- daisy(x, metric = "gower")
K  <- 2:min(10, nrow(x)-1)

avg_sil <- sapply(K, function(k) {
  cl <- kproto(x, k = k, type = "gower", nstart = 5)$cluster
  mean(silhouette(cl, d)[, "sil_width"], na.rm = TRUE)
})

plot(K, avg_sil, type = "b",
     xlab = "K", ylab = "Average silhouette",
     main = "k-prototypes — Silhouette vs K")
set.seed(2025)



mod <- kproto(subset(dt, select = -Injury_Next_Season), k = 2, lambda = NULL, nstart = 5)

mod$cluster; mod$size; mod$centers
table(mod$cluster, dt$Injury_Next_Season)


dt[, cluster := factor(mod$cluster)]



dt[, .N, by = cluster][order(cluster)]                                      

dt[, .(n=.N,
       injuries=sum(Injury_Next_Season=="Injured"),
       rate=mean(Injury_Next_Season=="Injured")),
   by=cluster][order(cluster)]
overall <- dt[, lapply(.SD, mean), .SDcols = sapply(dt, is.numeric)]
bycl    <- dt[!is.na(cluster), lapply(.SD, mean), by=cluster,
              .SDcols = sapply(dt, is.numeric)]
abs_diff <- bycl[, lapply(.SD, function(x) abs(x - overall)), .SDcols = -'cluster']
# Columns with large abs differences = defining features.

# Mix of positions per cluster
dt[!is.na(cluster), .N, by=.(cluster, Position)][order(cluster, -N)]

table(mod$cluster, dt$Position)

# % within each cluster (rows sum to 1)
round(prop.table(table(mod$cluster, dt$Position), margin = 1), 3)
ggplot(dt, aes(Sleep_Hours_Per_Night, Stress_Level_Score, color = cluster)) +
  geom_point(alpha = 0.7, size=3) +
  labs(title = "Clusters using sleep hours and stress level", x = "Sleep hours per night", y = "Stress level score") +
  theme_minimal()
ggplot(dt, aes(Hamstring_Flexibility,Balance_Test_Score, color = cluster)) +
  geom_point(alpha = 0.7, size=3) +
  labs(title = "Clusters using hamstring flexibility and balance test score", x = "Hamstring flexibility", y = "Balance Test score") +
  theme_minimal()

