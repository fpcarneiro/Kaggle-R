library(data.table)
library(zoo)

# Group <- c(rep("a",5), rep("b",5))
# Sales <- c(2,4,3,3,5,9,7,8,10,11)
# Result <- c(2,3,3,3,3.4,9,8,8,8.5,9)
# df <- data.frame(Group, Sales, Result)
# 
# dt <- as.data.table(df)
# dt[, res := cumsum(Sales)/(1:.N), by = Group]
# 
# dt[, res_1 := cumsum(Sales), by = Group]
# 
# dt[, res_2 := rollapplyr(Sales, 1:.N, mean), by = Group]

formato_data <- '%d/%m/%Y'
DT.processPeriodo <- function(dt) {
  dt[, Ano := year(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Mes := month(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Dia := day(as.Date(`Data de Criação`, format = formato_data))]
  dt[, Trimestre := quarter(as.Date(`Data de Criação`, format = formato_data))]
  dt[, DATA_CRIACAO := as.Date(`Data de Criação`, format = formato_data)]
}

colunas <- c("Data de Criação", "Fornecedor (Código)", "Nota Final", "Nota Macrocriterio", 
             "Nota Rev Macrocriterio", "Peso Macrocriterio", "Valor Bruto", "Macrocritério")

MB_DT <- fread("D:/R Stuff/MB/P.B.A.MM.007CA_PBAMMP004.csv", select = colunas)
DT.processPeriodo(MB_DT)

campos_num = c("Nota Final", "Nota Macrocriterio", "Nota Rev Macrocriterio", "Peso Macrocriterio", "Valor Bruto")

MB_DT[, `Nota Final` := as.double(sub(",", ".", `Nota Final`, fixed = TRUE))]
MB_DT[, `Nota Macrocriterio` := as.double(sub(",", ".", `Nota Macrocriterio`, fixed = TRUE))]
MB_DT[, `Nota Rev Macrocriterio` := as.double(sub(",", ".", `Nota Rev Macrocriterio`, fixed = TRUE))]
MB_DT[, `Peso Macrocriterio` := as.double(sub(",", ".", `Peso Macrocriterio`, fixed = TRUE))]
MB_DT[, `Valor Bruto` := as.double(sub(",", ".", `Valor Bruto`, fixed = TRUE))]

setnames(MB_DT, old = "Nota Final", new = "NOTA_FINAL")
setnames(MB_DT, old = "Valor Bruto", new = "VALOR_BRUTO")

MB_DT[VALOR_BRUTO == 0.00, VALOR_BRUTO := 1]

MB.getMediaPonderada <- function(dt, campos_grp = c("Fornecedor (Código)", "DATA_CRIACAO", "Ano", "Mes"), removeNA = TRUE) {
  MB_DT_GRP <- dt[, .(NUM_REGISTROS = .N, 
                          NOTA_FINAL = weighted.mean(NOTA_FINAL, VALOR_BRUTO, na.rm = removeNA), 
                          VALOR_BRUTO = sum(VALOR_BRUTO, na.rm = removeNA)), by = campos_grp]
  setkeyv(MB_DT_GRP, campos_grp)
  return (MB_DT_GRP)
}

MB_DT_GRP_DIA <- MB.getMediaPonderada(dt = MB_DT, campos_grp = c("Fornecedor (Código)", "DATA_CRIACAO", "Ano", "Mes"))
MB_DT_GRP_MES <- MB.getMediaPonderada(dt = MB_DT_GRP_DIA, campos_grp = c("Fornecedor (Código)", "Ano", "Mes"))

MB_DT_GRP_DIA[, MOV_SUM := roll_sum(x=NOTA_FINAL, n=3, align="right", fill = NA), by = c("Fornecedor (Código)")]
MB_DT_GRP_DIA[, MOV_AVG := roll_mean(x=NOTA_FINAL, n=3, align="right", fill = NA), by = c("Fornecedor (Código)")]
MB_DT_GRP_DIA[, MOV_WAVG := roll_mean(x=NOTA_FINAL, n=3, weights = VALOR_BRUTO, align="right", fill = NA), by = c("Fornecedor (Código)")]


DT[, roll_x := rollapplyr(x, 3, mean, fill = NA), y]
library(caTools)
MB_DT_GRP_DIA[, roll_x := runmean(NOTA_FINAL, 3, align = 'right', endrule = 'NA')]

MB_DT_GRP_DIA[, rollapplyr(data = NOTA_FINAL, width = 3, FUN = mean, fill = NA, align="right"), by = c("Fornecedor (Código)")]

fwrite(MB_DT_GRP_DIA, "MB.csv", sep=";")














setkeyv(MB_DT, c("Fornecedor (Código)", "DATA_CRIACAO"))

window <- 3

MB_DT[, "SEQ" := 1:.N, by = campos_grp]
MB_DT[, "Moving Mean" := rollapplyr(data = `Nota Final`, width = 1:.N, FUN = mean), by = campos_grp]
MB_DT[, "Moving Weigthed Mean" := rollapplyr(data = `Nota Final`, width = 1:.N, FUN = weighted.mean, x = `Nota Final`, w = `Valor Bruto`, na.rm = FALSE), by = campos_grp]

MB_DT[, "Moving Weigthed Mean 2" := rollapplyr(`Nota Final`, 3, mean, na.rm = TRUE, fill = NA), by = campos_grp]


library(RcppRoll)
v=1:10
data.frame(v, c2=roll_mean(x=v,n=3, fill=NA, align="right"))
