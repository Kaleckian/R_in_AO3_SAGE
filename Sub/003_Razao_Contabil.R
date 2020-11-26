fnc.Razao_Contabil <- function(arg1){
  vec_names_razao <- c('DATA', 'HIST', 'DEL_1', 'LOTE_LCTO', 'DEL_2', 'VAL_DEB', 'VAL_CRD', 'SALDO')
  colnames(arg1) <- vec_names_razao
  
  ### Eliminam-se campos inuteis que foram renomeados com o prefixo "DEL".
  ### Ainda, partindo do campo "DATA", classifica-se o que eh cabecalho de conta no razao 
  # e o que eh valor de lancamento. Isso eh realizado por um right join que discrimina as 
  # Contas Analiticas. Ainda, como redundancia, adiciona-se o pressuposto de que 
  # teremos um DW na Vitto antes do planeta Terra adentrar uma era glacial ou ser 
  # atingido por Desdemona e Cressida, duas das varias luas de Urano.
  
  arg1 <- arg1 %>% mutate(DATA = as.numeric(DATA)) %>% 
  ### Flag para marcar os lançamentos de Resultado!
  ### O flag eh utilizado para separar lançamentos de DRE e BP.
    mutate(Fechamento = if_else(grepl(pattern = 'TRANSF RESULTADO',x = str_trim(HIST)),'X','0')) %>%
    mutate(Conta_Lanc = if_else(DATA > 18250000, 'conta_ctb','lancamento'),
           DATA = as.numeric(DATA)) %>%
    select(-starts_with('DEL')) %>% mutate(conta_ctb='') %>%
    select(conta_ctb,everything())
  
  for(i in 1:nrow(arg1)){
    if(arg1$Conta_Lanc[i]=='conta_ctb'){
      ledger <- arg1$DATA[i]
    }
    arg1$conta_ctb[i] <- ledger
  }
  
  if(nrow(arg1 %>% filter(conta_ctb!=DATA, Conta_Lanc=='conta_ctb'))==0){
    print('Sem problemas ate tratamento de arg1 para for e bind_rows.')
  }else{
    paste0('Aviso: problemas com tratamento de arg1: ',
           nrow(arg1 %>% filter(conta_ctb!=DATA, Conta_Lanc=='conta_ctb')),
           ' registros fora de conformidade.')
    stop()}
  
  arg1 <- arg1 %>% filter(Conta_Lanc != 'conta_ctb') %>%
    select(-Conta_Lanc) %>%
    mutate_at(c('VAL_DEB','VAL_CRD','SALDO'),.funs = as.numeric) %>%
    mutate_at(c('VAL_DEB','VAL_CRD','SALDO'),.funs = 
                function(x){if_else(is.na(x),0,x)}
    ) %>%
    ### Mais uma vez o Excel mostrando sua influencia neste codigo.
    ### Eh ridiculo ter 1899-12-30 como argumento referencial.
    mutate(DATA = str_replace_all(as.Date(DATA,origin='1899-12-30'),'[-]','')) %>%
    rename(Data_Lanc = DATA) %>% mutate(AnoMes_Lanc = str_sub(Data_Lanc,1,6)) %>%
    mutate(Ano_Lanc = str_sub(Data_Lanc,1,4))
  
  arg1 <- arg1 %>%  
    
    ### O join estabelece a data de competencia partindo do prefixo do lote
    # contabil. A "data de lancamento" nao serve para consolidar o balancete!!!
    left_join(
      arg1 %>% mutate(Lote = LOTE_LCTO) %>%
        select(Data_Lanc, conta_ctb, Ano_Lanc,AnoMes_Lanc,LOTE_LCTO, Lote) %>%
        separate(Lote, c('Lote_Prf','Lote_Slf'),sep = '[/]') %>%
        group_by(Ano_Lanc,Lote_Prf) %>% mutate(AnoMes_CPTC = min(AnoMes_Lanc)) %>% 
        ungroup(), 
      by = c('LOTE_LCTO'='LOTE_LCTO','conta_ctb'='conta_ctb',
             'Data_Lanc'='Data_Lanc',
             'Ano_Lanc'='Ano_Lanc','AnoMes_Lanc'='AnoMes_Lanc')
    ) %>%

    mutate(AnoMes = AnoMes_CPTC) %>%
    mutate(Ano = str_sub(AnoMes,1,4),Mes= str_sub(AnoMes,-2,-1)) %>%
    mutate(TT = if_else(Mes <= '03','T1',
                        if_else(Mes <= '06','T2',
                                if_else(Mes<='09','T3','T4'))),
           TTAA = paste0(TT,'-',str_sub(Ano,3,4))) %>%
    mutate(Valor = VAL_CRD-VAL_DEB) %>% 
    select(conta_ctb,Data_Lanc,Ano,AnoMes,HIST,VAL_DEB,VAL_CRD,Valor,
           LOTE_LCTO, Lote_Prf, Lote_Slf,everything()) %>% 
    left_join(Plano_de_Contas, by =c('conta_ctb'='conta_ctb')) %>%
    mutate(BP_DRE = case_when(
      Fechamento == '0' & D1 %in% 3 ~ 'DRE',
      Fechamento == 'X' & D1 %in% 1:2 ~ 'BP',
      Fechamento == '0' & D1 %in% 1:2 ~ 'BP_Fech',
      Fechamento == 'X' & D1 %in% 3 ~ 'DRE_Fech',
      D1 %in% 4 ~ 'Fech',
      T ~'Erro'
    ))
  
  Razao_Contabil <- arg1 %>% select(-AnoMes_Lanc,-Ano_Lanc,-AnoMes_CPTC)
  
  return(Razao_Contabil)  

}
