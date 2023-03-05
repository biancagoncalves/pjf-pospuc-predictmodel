fun_prep_data_notificacao <- function(tbl.){
  output = tbl. %>% 
    #dplyr::filter(SITUA_ENCE != 1) %>% 
    dplyr::mutate(
      month_notificacao = lubridate::floor_date(DT_NOTIFIC,'month')
    ) |> 
    #dplyr::mutate(NU_ANO = as.integer(NU_ANO)) %>% 
    dplyr::group_by(month_notificacao) %>% 
    count() %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(ds = month_notificacao,y = n) |> 
    dplyr::arrange(ds) |> 
    dplyr::filter(ds >= '2000-01-01')
  
  return(output)
}

fun_prep_data_obito <- function(tbl.){
  output  = df_base %>% 
    dplyr::filter(SITUA_ENCE == 3) %>% 
    dplyr::mutate(
      month_notificacao = lubridate::floor_date(DT_NOTIFIC,'month')
    ) |> 
    dplyr::group_by(month_notificacao) %>% 
    count() %>% 
    dplyr::ungroup() %>% 
    dplyr::rename(ds = month_notificacao,y = n) |> 
    dplyr::arrange(ds) |> 
    dplyr::filter(ds >= '2000-01-01') 
  
  
  return(output)
}