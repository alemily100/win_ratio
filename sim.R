setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")
set.seed(11600)

n.sim<-1000

n.sample_vec<- c(30,30)
vec_dlt_rate<- c(0.2, 0.45)
overall_efficacy<- c(0.2, 0.6)
dlt_efficacy<- c(0.1, 0.4)
correlation_pro_exp<- -0.6
correlation_pro_dlt<- 0.5
vec_exposure_shape<- c(9,9)
vec_exposure_rate<- c(1,2)
mat_fact<- matrix(c(0.059, 0.029, 0.265, 0.382, 0.265, 0.059, 0.029, 0.265, 0.382, 0.265), nrow=2, byrow=TRUE) 

exposure_threshold<- 0.8

#expected utility
M<-comparison_dataset(c(1000,1000), vec_dlt_rate, overall_efficacy, dlt_efficacy, 
                              correlation_pro_exp, correlation_pro_dlt, vec_exposure_shape, vec_exposure_rate, mat_fact, exposure_threshold)
M<-data.frame(M)%>%
  mutate(utility = case_when(
    dlt == 1 & response == 0 ~ 0,
    dlt == 0 & response == 0 ~ 30,
    dlt == 1 & response == 1 ~ 50,
    dlt == 0 & response == 1 ~ 100
  ))
utility_vals<-M %>% group_by(dose) %>% summarise(mean = mean(utility))

test<-replicate(n.sim,
  analysis(n.sample_vec, vec_dlt_rate, overall_efficacy, dlt_efficacy , correlation_pro_exp, correlation_pro_dlt,
           vec_exposure_shape, vec_exposure_rate, mat_fact,  exposure_threshold),
  simplify = FALSE)

four_endpoints <- do.call(rbind, lapply(test, `[[`, "four"))
utility_endpoints <- do.call(rbind, lapply(test, `[[`, "utility"))

wr_four<-sapply(1:n.sim, function (k) four_endpoints[k,1]/four_endpoints[k,2])
wr_utility<-sapply(1:n.sim, function (k) utility_endpoints[k,1]/utility_endpoints[k,2])



#######CREATE FIGURE ########
df <- bind_rows(
  data.frame(value = wr_four, type = "wr_four"),
  data.frame(value = wr_utility, type = "wr_utility")
)

empirical<- df%>%group_by(type)%>%summarise(mean(value<=1))

colnames(empirical)<- c("Num_endpoints", "prob_dose_a")
empirical[1,1]<- "Four"
empirical[3,1]<- "Three \n (Toxicity+Efficacy Utility)"
# Plot densities
density<-ggplot(df, aes(x = value, color = type)) +
  geom_density(size = 1.2) +  # Density lines
  xlim(0, 4) +
  labs(title = " ", x = "Win Ratio", y = "Density", colour="Number of endpoints") +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black", size = 0.8)+
  scale_color_manual(values = c("wr_four" = "blue", "wr_utility" = "red"))+
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = alpha("white", 0.6)),  # semi-transparent background
    legend.title = element_text(face = "bold")
  )+
  annotate("segment", x = 1, xend = 0, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 0.5, y = -0.08, label = "Dose A has more winners", hjust = 0.5) +
  annotate("segment", x = 1, xend = 4, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 1.5, y = -0.08, label = "Dose B has more winners", hjust = 0.5)+ expand_limits(y = -0.3)
density

#table_grob <- tableGrob(empirical, rows = NULL, theme=white_theme)

# Add table inside the plot


df_sum <- data.frame(
  dose = c("Dose A", "Dose B"),
  prob_no_DLT = round(1-vec_dlt_rate,2),
  prob_response = round(overall_efficacy,2),
  prob_inten = round(c(1-pbeta(0.8, vec_exposure_shape[1], vec_exposure_rate[1]),
                                           1-pbeta(0.8, vec_exposure_shape[2], vec_exposure_rate[2])),2),
  exp_fact = round(c(sum(mat_fact[1,]*(1:5)), sum(mat_fact[2,]*(1:5))),2),
  emp = c(empirical$prob_dose_a[1], 1-empirical$prob_dose_a[1])
)

df_utility <- data.frame(
  dose = c("Dose A", "Dose B"),
  utility = round(utility_vals[,2]$mean,2),
  prob_inten = round(c(1-pbeta(0.8, vec_exposure_shape[1], vec_exposure_rate[1]),
                 1-pbeta(0.8, vec_exposure_shape[2], vec_exposure_rate[2])),2),
  exp_fact = round(c(sum(mat_fact[1,]*(1:5)), sum(mat_fact[2,]*(1:5))),2),
  emp = c(empirical$prob_dose_a[2], 1-empirical$prob_dose_a[2])
)

df_sum_tidy <- df_sum %>%
  pivot_longer(-dose, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = dose, values_from = Value)

# Flip df_utility
df_utility_tidy <- df_utility %>%
  pivot_longer(-dose, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = dose, values_from = Value)

# highlight_lower <- function(df) {
#   apply(df, 1, function(row) {
#     ifelse(row == max(row), "lightgreen", "white")
#   })
# }

highlight_cells <- function(df, small_better_rows = NULL, color = "lightgreen") {
  apply(df, 1, function(row) row) -> bg  # placeholder
  bg <- t(sapply(1:nrow(df), function(i) {
    if (i %in% small_better_rows) {
      ifelse(df[i, ] == min(df[i, ]), color, "white")   # smaller is better
    } else {
      ifelse(df[i, ] == max(df[i, ]), color, "white")   # larger is better
    }
  }))
  bg
}

bg_table1 <- highlight_cells(df_sum_tidy[,-1], small_better_rows = 4)
bg_table2 <- highlight_cells(df_utility_tidy[,-1], small_better_rows = 3)


red_theme <- ttheme_default(
  core = list(fg_params = list(
    fontface = rep(c("plain", "plain", "plain","bold"), times = ncol(df_sum_tidy[,-1]))
  ),
  bg_params = list(fill=as.vector(bg_table2),  col = "black")),
  colhead = list(bg_params = list(fill = "red", col = NA))
)

blue_theme <- ttheme_default(
  core = list(fg_params = list(
    fontface = rep(c("plain", "plain", "plain", "plain", "bold"), times = ncol(df_sum_tidy[,-1]))
  ),
  bg_params = list(fill=as.vector(bg_table1),  col = "black")),
  colhead = list(bg_params = list(fill = "blue", col = NA))
)


table1 <- tableGrob(df_sum_tidy[,-1], rows = c("(1) Prob no DLT", "(2) Prob response", "(3) Prob sufficient intensity", "(4) Expected FACT-GP5 (larger is worse)", "Probability dose selected `best`"),theme = blue_theme)
table2 <- tableGrob(df_utility_tidy[,-1], rows = c("(1) Utility", "(2) Prob sufficient intensity", "(3) Expected FACT-GP5 (larger is worse)", "Probability dose selected `best`"), theme= red_theme)

p <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void()
fig2<- p + annotation_custom(table1, xmin = 1.5, xmax = 3, ymin = 2.5, ymax = 4)+
  annotation_custom(table2, xmin = 1.5, xmax = 3, ymin = 0.5, ymax = 2)

#pdf("scenario4_diff.pdf", width=15, height=5)
plot_grid(fig2, density, ncol = 2, rel_widths = c(1, 1.5))  
#dev.off()
