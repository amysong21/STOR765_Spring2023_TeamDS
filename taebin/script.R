### 00_manage_packages
if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
} 
p_load(tidyr,
       tidyverse,
       dplyr,
       ggplot2,
       readxl,
       naniar,
       GGally,
       ggcorrplot,
       FactoMineR,
       factoextra,
       CJIVE)

### 01_make_data
wm_df_raw <- readxl::read_excel("RData/cognit_stor765_data.xlsx",
                        sheet = "ROI_Nback_2vs0_block4")
# make long data wide
wm_df <- tidyr::pivot_wider(wm_df_raw, names_from = c("ROI", "Status"),
                         values_from = "Mean")
wm_df <- wm_df %>%
  remove_rownames %>%
  column_to_rownames(var="Subject")

### 02_EDA
# check missing data
vis_miss(wm_df) # no missing data

# scatterplot matrix
GGally::ggpairs(wm_df)
GGally::ggcorr(wm_df)
corr_matrix <- cor(wm_df)
ggcorrplot(corr_matrix)
wm_df_pca <- princomp(corr_matrix)
factoextra::fviz_eig(wm_df_pca, addlabels = TRUE)
wm_pc_scores <- data.frame(wm_df_pca$scores[,1:3])
GGally::ggpairs(wm_pc_scores)
GGally::ggcorr(wm_pc_scores)

### 03_AJIVE
# load cortisol data
cortisol_df_raw <- readxl::read_excel("RData/cognit_stor765_data.xlsx",
                                      sheet = "Cortisol_MRI_block1")
# preprocess cortisol data
cortisol_df <- cortisol_df_raw %>%
  remove_rownames %>%
  column_to_rownames(var="StudyID")

# merge cortisol data and working memory data
cortisol_df %>%
  dplyr::inner_join(wm_df, by=0)
merged_df <- merge(cortisol_df, wm_df, by = 0)
merged_df <- merged_df %>%
  remove_rownames %>%
  column_to_rownames(var="Row.names")

# split data
cortisol_df_inter <- merged_df[, 1:ncol(cortisol_df)]
wm_df_inter <- merged_df[, (ncol(cortisol_df)+1):(ncol(cortisol_df)+ncol(wm_df))]

df_list <- list(cortisol_df_inter, wm_df_inter)

jive <- CJIVE::sjive(df_list, signal_ranks = c(3, 3), joint.rank = 2)
jive$joint_matrices









```{r}
# load cortisol data
cortisol_df_raw <- readxl::read_excel("data/cognit_stor765_data.xlsx",
                                      sheet = "Cortisol_MRI_block1")
# preprocess cortisol data
cortisol_df <- cortisol_df_raw %>%
  remove_rownames %>%
  column_to_rownames(var = "StudyID")
```

```{r}
# merge cortisol data and working memory data
merged_df <- merge(cortisol_df, wm_df, by = 0)
merged_df <- merged_df %>%
  remove_rownames %>%
  column_to_rownames(var="Row.names")

# split data
cortisol_df_inter <- merged_df[, 1:ncol(cortisol_df)]
wm_df_inter <- 
  merged_df[, (ncol(cortisol_df)+1):(ncol(cortisol_df)+ncol(wm_df))]
```

```{r}
cortisol_corr_matrix <- cor(cortisol_df_inter)
cortisol_df_pca <- princomp(cortisol_corr_matrix)
factoextra::fviz_eig(cortisol_df_pca, addlabels = TRUE)
cortisol_pc_scores <- data.frame(cortisol_df_pca$scores[,1:3])
GGally::ggpairs(cortisol_pc_scores)
```

```{r}
wm_corr_matrix <- cor(wm_df_inter)
wm_df_pca <- princomp(wm_corr_matrix)
factoextra::fviz_eig(wm_df_pca, addlabels = TRUE)
wm_pc_scores <- data.frame(wm_df_pca$scores[,1:3])
GGally::ggpairs(wm_pc_scores)
```

```{r}
data_blocks <- list(as.matrix(cortisol_df_inter), as.matrix(wm_df_inter))

jive <- CJIVE::sjive(data_blocks, signal_ranks = c(3, 4), joint.rank = 2)
```

```{r}
initial_signal_ranks <- c(3, 4) # set by looking at scree plots
jive_results <- ajive::ajive(data_blocks, initial_signal_ranks, 
                             n_wedin_samples = 100, n_rand_dir_samples = 100)

# estimated joint rank
jive_results$joint_rank
```

```{r}
cortisol_joint_scores <- jive$joint_matrices[[1]]$u
wm_joint_scores <- jive$joint_matrices[[2]]$u

cortisol_joint <- as.data.frame(cortisol_joint_scores)
wm_joint <- as.data.frame(wm_joint_scores)

cortisol_joint %>%
  mutate(rn = row_number()) %>% 
  pivot_longer(cols = -rn) %>%
  ggplot(aes(x = rn, y = value, color = name)) + 
  geom_line()

wm_joint %>%
  mutate(rn = row_number()) %>% 
  pivot_longer(cols = -rn) %>%
  ggplot(aes(x = rn, y = value, color = name)) + 
  geom_line()

joint_1 <- as.data.frame(
  cbind(cortisol_joint_scores[, 1], wm_joint_scores[, 1]))

joint_1 %>%
  mutate(rn = row_number()) %>% 
  pivot_longer(cols = -rn) %>%
  ggplot(aes(x = rn, y = value, color = name)) + 
  geom_line()

joint_2 <- as.data.frame(
  cbind(cortisol_joint_scores[, 2], wm_joint_scores[, 2]))

joint_2 %>%
  mutate(rn = row_number()) %>% 
  pivot_longer(cols = -rn) %>%
  ggplot(aes(x = rn, y = value, color = name)) + 
  geom_line()
```
