fnc.SaldosBP <- function(arg1,Plano_de_Contas){
  unique(arg1$BP_DRE)
  names(arg1)

  arg1a <- arg1 %>% 
    mutate(conta_ctb = case_when(
      !D1 %in% 1:2 ~ '2040300200001',
      T ~ conta_ctb)) %>%
    select(-HIST,-(LOTE_LCTO:BP_DRE)) %>%
    mutate(Data_Lanc = paste0(AnoMes,'00')) %>%
    group_by(conta_ctb,Data_Lanc,Ano,AnoMes) %>%
    summarise(VAL_DEB = sum(VAL_DEB),VAL_CRD = sum(VAL_CRD),Valor = sum(Valor)) %>%
    ungroup()
  
length(unique(arg1a$conta_ctb))*length(unique(arg1a$Data_Lanc))  

  CONTAXDATA <- merge(
    dplyr::tibble(conta_ctb = unique(arg1a$conta_ctb)), 
    dplyr::tibble(Data_Lanc =  unique(arg1a$Data_Lanc)))  
    
  arg1b <- CONTAXDATA %>% left_join(arg1a) %>%
    mutate(BP_DRE = 'Saldo',Ano = str_sub(Data_Lanc,1,4),AnoMes = str_sub(Data_Lanc,1,6)) %>%
    mutate_at(c('VAL_DEB','VAL_CRD','Valor'),.funs = 
                function(x){if_else(is.na(x),0,x)}) %>%
    arrange(conta_ctb,AnoMes) %>% group_by(conta_ctb) %>% 
    mutate(Valor = cumsum(Valor)) %>%
   
    left_join(Plano_de_Contas, by =c('conta_ctb'='conta_ctb'))
  
  arg1b %>% group_by(Ano) %>% filter(str_sub(AnoMes,-2,-1) == max(str_sub(AnoMes,-2,-1))) %>% ungroup() %>% 
    group_by(D1) %>% group_by(Ano) %>% summarise(Valor = sum(Valor))

  Razao_Contabil <- arg1 %>% bind_rows(arg1b)
  
  
  
  return(Razao_Contabil)  
}

