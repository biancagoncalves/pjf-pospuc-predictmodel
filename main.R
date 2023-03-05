require(dplyr)
require(prophet)
require(forecast)
require(ggplot2)
require(zoo)
require(fs)
require(purrr)

set.seed(45)

fs::dir_ls('./src',type='file',glob='*.R') |> 
  purrr::walk(~source(.x, encoding='UTF-8'))

df_base = readRDS('./data/base.rds')


tbl_notificacao = df_base %>% fun_prep_data_notificacao()
  
tbl_obito  = df_base %>% fun_prep_data_obito()
  

boxplot(tbl_obito$y)
boxplot(tbl_notificacao$y)


# Regra para retirar os ultimos 8 valores devido a recalculo --------------

.tail = 8
tbl_obito = tbl_obito |> head(nrow(tbl_obito)-.tail)
tbl_notificacao = tbl_notificacao |> head(nrow(tbl_notificacao)-.tail)

boxplot(tbl_obito$y)
boxplot(tbl_notificacao$y)


# Modelo para notificacao -------------------------------------------------



output_notificacao = fun_create_arima_model(tbl. = tbl_notificacao,h. = 10)

plot(decompose(output_notificacao$ts_train))
monthplot(output_notificacao$ts_train)
output_notificacao$ts_test
output_notificacao$plot_overview
output_notificacao$modelo_summary

x = output_notificacao$tbl_final %>% 
  tidyr::pivot_wider(id_cols = ds,
                     names_from = "type",
                     values_from = "value") 
MLmetrics::MAPE(x$predict, x$real)

output_notificacao$plot_real_predict



meta_h = seq(as.Date('2021-07-01'),as.Date('2025-12-01'), 'month') 
meta_forecast = forecast(output_notificacao$modelo, h = length(meta_h))


tibble::tibble(
  ds = meta_h,
  value = meta_forecast$mean,
  type = 'forecast'
) %>% 
  dplyr::bind_rows(
    tibble::tibble(
      ds = output_notificacao$original_tbl$ds,
      value = output_notificacao$original_tbl$y,
      type = 'real'
    )
  ) %>% 
  ggplot(aes(x=ds, y=value, group=type)) +
  geom_line(aes(color=type)) 


# Modelo para obito -------------------------------------------------------



output_obito = fun_create_arima_model(tbl_obito, h.=10)

plot(decompose(output_obito$ts_train))
monthplot(output_obito$ts_train)
output_obito$plot_overview
output_obito$modelo_summary

x = output_obito$tbl_final %>% 
  tidyr::pivot_wider(id_cols = ds,
                     names_from = "type",
                     values_from = "value") 
MLmetrics::MAPE(x$predict, x$real)

output_obito$plot_real_predict



meta_h = seq(as.Date('2021-07-01'),as.Date('2022-12-01'), 'month') 
meta_forecast = forecast(output_obito$modelo, h = length(meta_h))


tibble::tibble(
  ds = meta_h,
  value = meta_forecast$mean,
  type = 'forecast'
) %>% 
  dplyr::bind_rows(
    tibble::tibble(
      ds = output_obito$original_tbl$ds,
      value = output_obito$original_tbl$y,
      type = 'real'
    )
  ) %>% 
  ggplot(aes(x=ds, y=value, group=type)) +
  geom_line(aes(color=type)) 

