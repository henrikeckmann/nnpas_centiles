library(gamlss)
library(gamlss.ggplots)
library(gamlssTools)

rm(list = ls())
source("2_wrangling/2_1_data_wrangling.R")

for (gender in c(1, 2)){
  for (aa_ig in c("AvAcc", "IG")){
    for (ESM in c("ENMO", "ENMOa", "LFENMO", "MAD", "NeishabouriCount_vm")){

      ESM_aa = ESM
      ESM_ig = paste0("ig_gradient_", ESM)

      gender_df = all_df_participant |>
        filter(gender == !!gender) |>
        rename(
          pa_measure = if_else(
            aa_ig == "IG",
            paste0("M14_", ESM_ig, "_mg_0-24hr"),
            paste0("M14_", ESM_aa, "_mg_0-24hr")
          )
        )

      m1 = gamlss(
        pa_measure ~ pb(age),
        sigma.formula = ~ pb(age),
        nu.formula = ~ pb(age),
        tau.formula = ~ pb(age),
        family=NO,
        data=gender_df,
        control = gamlss.control(n.cyc = 50)
      )

      # test distributions
      f_type = if_else(aa_ig == "AvAcc", "realplus", "realline")

      t1 = chooseDist(m1, type = f_type, parallel = "snow", ncpus = detectCores()-1)

      if (aa_ig == "IG"){t1 = t1[-2,]} #GU family errors

      best_family = names(getOrder(t1, 1)[1])

      # update based on best AIC (can do chi-square or BIC and more)
      m1.update = update(
        m1,
        family = best_family,
        control = gamlss.control(n.cyc = 100)
      )

      sim_df = list("df" = data.frame("age" = 20:80))


      centiles_df = centile_predict(
        m1.update,
        sim_df,
        "age",
        desiredCentiles = seq(0.01, 0.99, 0.01)
      )

      centiles_df = centiles_df$fanCentiles_df

      write.csv(centiles_df, paste0("centile_csv/", ESM, aa_ig, gender, ".csv"))

      }
    }
  }


