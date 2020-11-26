fnc.Plano_de_Contas <- function(arg1){

  names(arg1) <- c('a1','a2','a3','a4','a5')
  
### O filtro is.na(a1) retira os espacos em branco que sao inuteis;
  arg1 <- arg1 %>% filter(!is.na(a1))
  
### Estabelece os níveis maximo (analitico) e minimo do Plano de contas.
  vminPC <- min(arg1$a4);vmaxPC <- max(arg1$a4)
  
  print(paste0('O primeiro nivel hierarquico do Plano eh ', vminPC,'.'))
  print(paste0('O nivel analitico do Plano eh ', vmaxPC,'.'))
  
### Estrutura as contas analiticas hierarquizadas para qualquer nivel.
  for(NivelPC in vmaxPC:vminPC){
    
###Aqui o unico destaque particular sao as contas analiticas, o primeiro grupo
# abordado para, entao, realizar os join subsequentes.
    if(NivelPC == vmaxPC){
      
      df <- arg1 %>% filter(a4 == vmaxPC)
      
      names(df) <- c('conta_ctb', 'cod', 'desc_ctb', 'nivel_ctb', 'tipo_ctb')


      str_len_analitica <<- str_length(df$conta_ctb[sample(x = 1:nrow(df),size = 1)])
      df <- df %>% mutate(key_tojoin = conta_ctb)  
      
    }else{
      
      df_tojoin <- arg1 %>% filter(a4 == NivelPC)
      
      str_len_RHS <- str_length(df_tojoin$a1[sample(x = 1:nrow(df_tojoin),
                                                    size = 1)])
      
      eval(parse(text= paste0('df <- df %>% mutate(D',NivelPC,' = str_sub(key_tojoin,1,',str_len_RHS,'))')))
      
      names(df_tojoin) <- 
        c(paste0('D',NivelPC),paste0('cod','_',NivelPC),paste0('desc_',NivelPC),
          paste0('nivel_',NivelPC),paste0('tipo_',NivelPC))
      
      df_tojoin %>% select(-starts_with(match='cod'))
      
      df <- df %>% left_join(df_tojoin,by=setNames(paste0('D',NivelPC),paste0('D',NivelPC)))    
    } 
  }
  df <- df %>% arrange(conta_ctb) %>% select(-key_tojoin)
  
  return(df)
}

