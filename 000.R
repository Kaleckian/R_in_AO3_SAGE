rm(list = ls()); gc();

options(java.parameters = "-Xmx48g",scipen=999)
# 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

vec.pkg <- c("rstudioapi",'tictoc',"lubridate",'tidyverse','writexl','readxl')
vec.newpkg <- vec.pkg[!(vec.pkg %in% installed.packages()[,"Package"])]
if(length(vec.newpkg)) install.packages(vec.newpkg)
lapply(vec.pkg, require, character.only = TRUE)
rm(vec.pkg,vec.newpkg)

tic('Script')

ZZZ_Stuff <- list()

##### CHART OF ACCOUNTS ##### 

### Assumptions
# (1) relative path: 'chart_of_accounts/';
# (2) first sheet has to be the chart of accounts;
# (3) data on the chart of accounts extraction starts at row number:

skip_lines_Chart_of_Accounts <- 5

ZZZ_Stuff[['chart']] <- 
  readxl::read_excel(path = 'chart_of_accounts/chart_of_accounts.xlsx',
                     # Attention to skip = skip_lines_Chart_of_Accounts
                     skip = skip_lines_Chart_of_Accounts) %>%
  mutate_all(str_trim)

# (4) relative path of the subroutine:
source('Sub/001_Plano_de_Contas.R')

arg1 <- ZZZ_Stuff[['chart']]

tic('function: Plano de Contas')
Plano_de_Contas <- fnc.Plano_de_Contas(arg1 = ZZZ_Stuff[['chart']])
toc()

##### GENERAL LEDGER ##### 

### Assumptions:
# (1) relative path: 'GLs/', with several files starting with "GL".
# (2) files are aggregated incrementally;
# (3) first sheet has to be the general ledger;
# (4) actual data starts at row number:

skip_lines_ledger <- 6

ZZZ_Stuff[['GL']] <- bind_rows(
  lapply(list.files(path = 'GLs/',pattern = 'GL', full.names = T), 
    FUN = function(x){
      # Attention to skip = skip_lines_ledger argument.
      readxl::read_excel(path = x,skip = skip_lines_ledger) %>% filter(!is.na(Data)) %>%
        mutate(Data = str_replace_all(pattern = '[Conta:]',replacement = '',string = Data)) %>%
        mutate_all(str_trim)
    }
  )
)

# (5) relative path of the subroutine:
source('Sub/003_Razao_Contabil.R')

arg1 <- ZZZ_Stuff[['GL']]

tic('function: Razao Contabil')
Razao_Contabil <- fnc.Razao_Contabil(arg1 = ZZZ_Stuff[['GL']])
toc()

### Compor saldos do BP para analise gerencial apenas (cumulative sum).
source('Sub/004_SaldosBP.R')

arg1 <- Razao_Contabil

Razao_Contabil <- fnc.SaldosBP(arg1 = arg1, Plano_de_Contas = Plano_de_Contas)


writexl::write_xlsx(list(DadosRazao = Razao_Contabil,
                         Plano_de_Contas = Plano_de_Contas),
                    'R_Razao.xlsx')

toc()



unique(Razao_Contabil$BP_DRE)

Razao_Contabil %>% group_by(Ano) %>% summarise(sum(Valor))

Razao_Contabil %>% filter(D1 %in% 1:2,!grepl(pattern = 'TRANSF RESULTADO',x = HIST)) %>%
  group_by(Ano) %>% summarise(sum(Valor))

Razao_Contabil %>% filter(D1 %in% 3,!grepl(pattern = 'TRANSF RESULTADO',x = HIST)) %>%
  group_by(Ano) %>% summarise(sum(Valor))

Razao_Contabil %>% filter(D1 %in% 1:3,!grepl(pattern = 'TRANSF RESULTADO',x = HIST)) %>%
  group_by(Ano) %>% summarise(sum(Valor))

Razao_Contabil %>% filter(D1 == '3',!grepl(pattern = 'TRANSF RESULTADO',x = HIST)) %>%
  group_by(Ano) %>% summarise(sum(Valor))

