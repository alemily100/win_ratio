setwd("C:/Users/ealger/OneDrive - The Institute of Cancer Research/M/PhD/Trial Designs/win_ratio")
source("functions.R")
source("figure_sourcefile.R")
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

test<-replicate(n.sim,
  analysis(n.sample_vec, vec_dlt_rate, overall_efficacy, dlt_efficacy , correlation_pro_exp, correlation_pro_dlt,
           vec_exposure_shape, vec_exposure_rate, mat_fact,  exposure_threshold),
  simplify = FALSE)

response_priority <- do.call(rbind, lapply(test, `[[`, "response"))
dlt_priority <- do.call(rbind, lapply(test, `[[`, "dlt"))
tie_priority <- do.call(rbind, lapply(test, `[[`, "tie"))

wr_response_priority<-sapply(1:n.sim, function (k) response_priority[k,1]/response_priority[k,2])
wr_dlt_priority<-sapply(1:n.sim, function (k) dlt_priority[k,1]/dlt_priority[k,2])
wr_tie_priority<-sapply(1:n.sim, function (k) tie_priority[k,1]/tie_priority[k,2])



#######CREATE FIGURE ########
df <- bind_rows(
  data.frame(value = wr_response_priority, type = "response_priority"),
  data.frame(value = wr_dlt_priority, type = "dlt_priority"),
  data.frame(value = wr_tie_priority, type = "tie_priority"),
)

empirical<- df%>%group_by(type)%>%summarise(mean(value<=1))

colnames(empirical)<- c("priority", "prob_dose_a")
empirical[1,1]<- "dlt"
empirical[2,1]<- "response"
empirical[3,1]<- "tie"
# Plot densities
density<-ggplot(df, aes(x = value, color = type)) +
  geom_density(size = 1.2) +  # Density lines
  xlim(0, 4) +
  labs(title = " ", x = "Win Ratio", y = "Density", colour="Rank") +
  theme_minimal() +
  geom_vline(xintercept = 1, linetype = "dotted", color = "black", size = 0.8)+
  scale_color_manual(values = c("response_priority" = "steelblue3", "dlt_priority" = "red3", "tie_priority"="darkgoldenrod2"))+
  theme(
    legend.position = "none",
    legend.background = element_rect(fill = alpha("white", 0.6)),  # semi-transparent background
    legend.title = element_text(face = "bold")
  )+
  annotate("segment", x = 1, xend = 0, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 0.5, y = -0.08, label = "Win ratio favors Dose A", hjust = 0.5) +
  annotate("segment", x = 1, xend = 4, y = -0.15, yend = -0.15,
           arrow = arrow(length = unit(0.2, "cm")), color = "black") +
  annotate("text", x = 1.5, y = -0.08, label = "Win ratio favors Dose B", hjust = 0.5)+ expand_limits(y = -0.3)
density

df_sum <- data.frame(
  dose = c("Dose A", "Dose B"),
  prob_no_DLT = round(vec_dlt_rate,2),
  prob_response = round(overall_efficacy,2),
  prob_inten = round(c(1-pbeta(0.8, vec_exposure_shape[1], vec_exposure_rate[1]),
                                           1-pbeta(0.8, vec_exposure_shape[2], vec_exposure_rate[2])),2),
  exp_fact = round(c(sum(mat_fact[1,]*(1:5)), sum(mat_fact[2,]*(1:5))),2),
  dlt = c(empirical$prob_dose_a[1], 1-empirical$prob_dose_a[1]),
  tie = c(empirical$prob_dose_a[3], 1-empirical$prob_dose_a[3]),
  response = c(empirical$prob_dose_a[2], 1-empirical$prob_dose_a[2])
)


df_sum_tidy <- df_sum %>%
  pivot_longer(-dose, names_to = "Metric", values_to = "Value") %>%
  pivot_wider(names_from = dose, values_from = Value)




bg_table1 <- highlight_cells(df_sum_tidy[,-1], small_better_rows = c(1,4))


# Expand to a matrix to match table shape
border_matrix <- matrix(rep(border_colors, each = 2), nrow = 7, ncol = 2, byrow = TRUE)

lwd_matrix <- matrix(1, nrow = 7, ncol = 2)
lwd_matrix[5:7, ] <- 4

fontface_matrix <- matrix("plain", nrow = 7, ncol = 2)
for (i in 5:7) {
  j <- which.max(df_sum_tidy[i, -1])  # column with largest value
  fontface_matrix[i, j] <- "bold"
}

blue_theme <- ttheme_default(
  core = list(fg_params = list(
    fontface =as.vector(fontface_matrix)),
    bg_params = list(fill=as.vector(bg_table1),  
                     col = as.vector(border_matrix),lwd  = as.vector(lwd_matrix))),
  #,colhead = list(bg_params = list(fill = "blue", col = NA))
  colhead = list(
    fg_params = list(fontface = "bold"),         # bold headers
    bg_params = list(fill = "white", col = "white")  # white fill + black border
  )
)

table1 <- tableGrob(df_sum_tidy[,-1], rows = c("(1) DLT rate", "(2) Response rate", "(3a) Sufficient intensity rate", "(3b) Expected symptom bother", expression(atop(bold("Rank prioritising DLTs:"), "Probability dose selected `best`:")),
                                               expression(atop(bold("Rank with intermediate ties:"), "Probability dose selected `best`:")), expression(atop(bold("Rank prioritising response:"), "Probability dose selected `best`:"))),theme = blue_theme)

p <- ggplot() + xlim(0, 4) + ylim(0, 4) + theme_void()
fig2<- p + annotation_custom(table1, xmin = 0, xmax = 4, ymin = 0, ymax = 4)
#pdf("scenario4_diff.pdf", width=15, height=5)
plot_grid(fig2, density, ncol = 2, rel_widths = c(1, 1.5))  
#dev.off()
