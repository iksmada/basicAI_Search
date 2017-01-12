debugSource("Canibais.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

## No problema do aspirador temos o caso onte existe uma matriz 2x2 representando os lugares 
## a serem limpados e assim de início pelo menos dois devem estar sujos

inicial <- Aspirador(desc = c(A = 3, Q1 = 1, Q2 = 0, Q3 = 1, Q4 = 0))

objetivo <- Aspirador()
objetivo$desc <- c(A = 0, Q1 = 0, Q2 = 0, Q3 = 0, Q4 = 0)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))