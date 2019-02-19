library(data.table)
library(zoo)
library(DescTools)

formato_data <- '%d/%m/%Y'
DT.processPeriodo <- function(dt) {
  dt[, Ano := year(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Mes := month(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Dia := mday(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Trimestre := quarter(as.Date(`Data de Criação`, format = formato_data))]
  dt[, DATA_CRIACAO := as.Date(`Data de Criação`, format = formato_data)]
}

colunas <- c("Data de Criação", "Fornecedor (Código)", "Nota Final", "Nota Macrocriterio", 
             "Nota Rev Macrocriterio", "Peso Macrocriterio", "Valor Bruto", "Macrocritério")

MB_DT <- fread("P.B.A.MM.007CA_PBAMMP004.csv", select = colunas, dec=',')
setnames(MB_DT, old = "Fornecedor (Código)", new = "Fornecedor")
DT.processPeriodo(MB_DT)

#setkeyv(MB_DT, c("Fornecedor", "DATA_CRIACAO", "Ano", "Mes", "Dia"))

media.ponderada <- function(x, w) {
  return ((sum(x * w))/sum(w))
}

getNumFields <- function(dt) {
  tokeepNum <- sapply(head(dt, 1), is.numeric)
  tokeepInt <- sapply(head(dt, 1), is.integer)
  tokeep <- names(dt[1:5 , which(tokeepNum & !tokeepInt), with=FALSE])
  return (tokeep)
}

get_data_intervals <- function(dt) {
  ymin = min(dt$Ano)
  ymax = max(dt$Ano)
  mmin = min(dt[Ano == ymin]$Mes)
  mmax = max(dt[Ano == ymax]$Mes)
  return (data.table(ano_min=ymin, ano_max=ymax, mes_min=mmin, mes_max=mmax))
}

get_date_seq <- function(ano_min, mes_min = 1, ano_max, mes_max = 12, janela = 3) {
  data1 = as.Date(paste(c("01", mes_min, ano_min), collapse = "/"), format = formato_data)
  data2 = as.Date(paste(c("01", mes_max, ano_max), collapse = "/"), format = formato_data)
  sequencia = seq(AddMonths(data1, -1 * (janela-1)), data2, "months")
  return (data.table(Ano=year(sequencia), Mes=month(sequencia)))
}

get_dt_full <- function(dt, grp = c("Fornecedor"), janela = 3) {
  years_chave <- dt[, get_data_intervals(.SD), by = grp]
  return (years_chave[, get_date_seq(ano_min = ano_min, ano_max = ano_max, 
                                     mes_min = mes_min, mes_max = mes_max, 
                                     janela = janela), by = grp])
}

get_grp_agg <- function(dt, grp = c("Fornecedor", "Ano", "Mes")) {
  DT_GRP <- dt[, .("Nota Final" = weighted.mean(x = `Nota Final`, w = `Valor Bruto`, na.rm = FALSE),
                   "Valor Bruto" = sum(`Valor Bruto`)), by = grp]
  
  DT_GRP <- DT_GRP[, Produto := `Nota Final` * `Valor Bruto`]
  
  setkeyv(x = DT_GRP, cols = grp)
  return (DT_GRP)
}

get_grp_final <- function(dt, grp = c("Fornecedor"), janela = 3, fill_date = TRUE) {
  DT_GRP_MES <- get_grp_agg(dt, grp = c("Fornecedor", "Ano", "Mes"))
  
  if (fill_date) {
    full_combination <- get_dt_full(MB_DT, grp, janela)
    DT_GRP_MES <- merge(DT_GRP_MES, full_combination, all = TRUE, on = c(grp, c("Ano", "Mes")))
    DT_GRP_MES[, getNumFields(DT_GRP_MES) := lapply(.SD, function(x){ifelse(is.na(x),0,x)}), .SDcols = getNumFields(DT_GRP_MES)]
  }
  
  DT_GRP_MES[, SomaProdutos := as.numeric(rollsum(Produto, k=janela, fill = NA, align = "right", na.rm = TRUE)), by = grp]
  DT_GRP_MES[, SumPesos := as.numeric(rollsum(`Valor Bruto`, k=janela, fill = NA, align = "right", na.rm = TRUE)), by = grp]
  DT_GRP_MES[, "Nota Final" := (SomaProdutos/SumPesos)]
  
  DT_GRP_MES[, SomaProdutos := NULL]
  DT_GRP_MES[, SumPesos := NULL]
  DT_GRP_MES[, Produto := NULL]
  DT_GRP_MES[, `Valor Bruto` := NULL]
  
  return (DT_GRP_MES)
}

DT_GRP <- get_grp_final(MB_DT, grp = c("Fornecedor"), janela = 3, fill_date = TRUE)