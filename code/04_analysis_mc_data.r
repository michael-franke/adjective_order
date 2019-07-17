mc_results = read_csv("03_mc_results.csv") %>% 
  mutate(EU_diff = tall_brown - brown_tall,
         sd_diff = sd_tall - sd_brown)

## prepare to export to reproducible LaTeX
myvars = list()
myvars["EU_big_brown"] = mean(mc_results$tall_brown) %>% round(2)
myvars["EU_brown_big"] = mean(mc_results$brown_tall) %>% round(2)

#print success
print(paste0("average EU of 'tall brown': ", mean(mc_results$tall_brown) %>% round(4)))
print(paste0("average EU of 'brown tall': ", mean(mc_results$brown_tall) %>% round(4)))

x = t.test(mc_results$tall_brown, mc_results$brown_tall)
myvars["Tstatistic"] = x$statistic %>% round(3)
myvars["Pvalue"] = x$p.value %>% round(3)



model_saturated = glm(EU_diff ~ n_obj *  sd_diff * theta,
                       data = mc_results)
summary(model_saturated)

model_simpler = step(model_saturated)
summary(model_simpler)


myTable = cbind(tibble(Rowname = row.names(summary(model_simpler)$coef)), 
                summary(model_simpler)$coef %>% as_tibble()) %>% 
          mutate(Rowname = case_when(Rowname == "n_obj" ~ "$n_{\\text{obj}}$",
                                     Rowname == "sd_diff" ~ "$\\sigma_{\\text{diff}}$",
                                     Rowname == "theta" ~ "$\\theta$",
                                     Rowname == "n_obj:theta" ~ "$n_{\\text{obj}}$:$\\theta$",
                                     Rowname == "sd_diff:theta" ~ "$\\sigma_{\\text{diff}}$:$\\theta$",
                                     T ~ Rowname)) %>% 
  select(- "Std. Error")
myTable$sign = ifelse(myTable[,"Pr(>|t|)"] > 0.05, " ",
                      ifelse(myTable[,"Pr(>|t|)"] > 0.1, "*", 
                             ifelse(myTable[,"Pr(>|t|)"] > 0.01, "**", "***")))



readr::write_csv(myTable, path = "../writing/2019 CogSci/R_data_4_TeX/regression_results.csv", col_names = T)

## save collected variables

myvars = as_tibble(myvars)
readr::write_csv(myvars, path = "../writing/2019 CogSci/R_data_4_TeX/myvars.csv", col_names = T)
