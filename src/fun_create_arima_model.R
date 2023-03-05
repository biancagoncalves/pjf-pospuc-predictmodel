fun_create_arima_model <- function(tbl.,h. = 10){
  
  ts. = ts(tbl.$y, start = c(2001,01,01), frequency=12)
  
  plot_overview = ggplot2::ggplot(tbl., aes(x=ds, y=y)) +
    ggplot2::geom_line() +
    ggplot2::geom_point()
  
  
  #plot_ts_overview = ggtsdisplay(ts.)
  
  ts_train = head(ts.,length(ts.) - h. )
  ts_test = tail(ts.,h. )
  
  
  
  modelo <- auto.arima(ts_train, stepwise = FALSE)
  summary. = summary(modelo)
  
  predict_values <- forecast(modelo,h = h.)
  
  
  #plot_predict_values = plot(predict_values)
  
  
  tbl_final = tibble::tibble(
    ds = zoo::as.Date(ts_test),
    real = as.numeric(ts_test),
    predict = as.numeric(predict_values$mean)
  ) |> 
    tidyr::pivot_longer(
      cols =  c("real","predict"),
      names_to = 'type',
      values_to = 'value'
    )
  
  plot_real_predict = ggplot(tbl_final, aes(x=ds, y=value, group=type)) +
    geom_line(aes(color=type))+
    geom_point(aes(color=type))
  
  output = list(
    original_tbl = tbl.,
    ts_train = ts_train,
    ts_test = ts_test,
    h = h.,
    plot_overview = plot_overview,
    modelo = modelo,
    modelo_summary = summary.,
    #plot_predict_values = plot_predict_values,
    tbl_final = tbl_final,
    plot_real_predict = plot_real_predict
  )
  
  return(output)
  
}