full_table <- data.frame(model = c("RE ARIMA(1,0,0)", "RE ARIMA(2,0,0)", "FE ARIMA(1,0,0)", "FE ARIMA(2,0,0)", "Two-Way FE ARIMA(1,0,0)"),
           coef = c(.0002, 0.05, 0.0405, -0.0069, 0.0867),
           se = c(0.07, 0.07, 0.0488, 0.0496, 0.0473),
           plot_order = c(1:5))

gg_single <- ggplot(full_table, aes(x = coef, y = reorder(model, -plot_order))) +
  geom_point() +
  geom_vline(xintercept = 0, alpha = 0.5, col = "red") +
  geom_errorbar(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), width = 0) +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha = 0.5, col = "red") +
  labs(title = "Single State DVRO",
       y = "",
       x = "DVRO Point Estimate")

full_table_x <- data.frame(model = c("RE ARIMA(1,0,0)", "RE ARIMA(2,0,0)", "FE ARIMA(1,0,0)", "FE ARIMA(2,0,0)", "Two-Way FE ARIMA(1,0,0)"),
                           coef = c(0.0291, 0.0528, 0.0232, 0.0381, 0.1249),
                           se = c(0.0998, 0.0972, 0.0635, 0.0624, 0.0575),
                           plot_order = c(1:5))

gg_multi <- ggplot(full_table_x, aes(x = coef, y = reorder(model, -plot_order))) +
  geom_point() +
  geom_vline(xintercept = 0, alpha = 0.5, col = "red") +
  geom_errorbar(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se), width = 0) +
  theme_minimal() +
  geom_vline(xintercept = 0, alpha = 0.5, col = "red") +
  labs(title = "Neighboring State DVRO",
       y = "",
       x = "DVRO Point Estimate")

gg_multi
plot_multi <- ggarrange(gg_single, gg_multi)
annotate_figure(plot, top = text_grob("Dive depths (m)", 
                                      color = "red", face = "bold", size = 14))

full_table <- data.table(full_table)[, outcome := "Single State DVRO"]
full_table_x <- data.table(full_table_x)[, outcome := "Neighboring State DVRO"]
table_comb <- rbind(full_table, full_table_x)
gg_comb <- ggplot(table_comb, aes(x = coef, y = reorder(model, -plot_order), color = outcome, vjust = outcome)) +
  geom_vline(xintercept = 0, alpha = 0.5, col = "black") +
  geom_point(position = position_dodge(width=0.5)) +
  geom_errorbar(aes(xmin = coef - 1.96*se, xmax = coef + 1.96*se, color = outcome), width = 0, position = position_dodge(width=0.5)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(y = "",
       x = "DVRO Point Estimate",
       color = "Policy",
       title = "DVRO Coefficient Estimates")
gg_comb
