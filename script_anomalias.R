################################################################################################
# MEET-UP DE DADOS ZÉ DELIVERY                                                                 #
# @César Lemos - Uma abordagem não supervisionada para detecção de anomalias                   #
################################################################################################

# Pacotes -------------------------------------------------------------------------------------
pacotes <- c("tidyverse", "outliers", "funModeling")
lapply(pacotes, require, character.only = T)

# Carregando o dataset ------------------------------------------------------------------------

dataframe <- read.csv(choose.files())

# Verificando o metodo mais ajustado ----------------------------------------------------------

## Calculando os precos ajustados por Hampel ----
base_normalizada <- dataframe %>%
  dplyr::group_by(produto) %>%
  dplyr::summarise(
    lim_inf_hampel = funModeling::hampel_outlier(preco, k_mad_value = 2.58)[[1]],
    lim_sup_hampel = funModeling::hampel_outlier(preco, k_mad_value = 2.58)[[2]],
    mediana = stats::median(preco)
  ) %>%
  dplyr::right_join(dataframe, by = "produto") %>%
  dplyr::mutate(preco_hampel = dplyr::if_else(preco > lim_inf_hampel &
                                                     preco < lim_sup_hampel, preco, mediana))

## Calculando os precos ajustados pelo IQR ----
base_normalizada <- base_normalizada %>% 
  dplyr::group_by(produto) %>% 
  dplyr::summarise(lim_inf_iqr = stats::quantile(preco, .25) - 1.5 * stats::IQR(preco),
                   lim_sup_iqr = stats::quantile(preco, .75) + 1.5 * stats::IQR(preco)) %>% 
  dplyr::right_join(base_normalizada, by = "produto") %>%
  dplyr::mutate(preco_iqr = dplyr::if_else(preco > lim_inf_iqr &
                                                     preco < lim_sup_iqr, preco, mediana))

## Realizando o teste de Grubbs
base_normalizada %>% 
  dplyr::group_by(produto) %>% 
  dplyr::summarise(grubbs_test_hamp =
                     dplyr::if_else(outliers::grubbs.test(preco_hampel)$p.value > .05 |
                                      outliers::grubbs.test(preco_hampel, opposite = T)$p.value > .05, 0, 1),
                   grubbs_test_iqr =
                     dplyr::if_else(outliers::grubbs.test(preco_iqr)$p.value > .05 |
                                      outliers::grubbs.test(preco_iqr, opposite = T)$p.value > .05, 0, 1)) %>%
  dplyr::select(grubbs_test_hamp, grubbs_test_iqr) %>% 
  base::sapply(function(x) sum(x))
