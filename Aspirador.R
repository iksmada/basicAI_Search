source("Estado.R")

## Classe e métodos para o problema do Aspirador de pó 2x2
Aspirador <- function(desc = NULL, pai = NULL){
  
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat(" __ __  \n|Q1|Q2|\n|Q3|Q4|\n ¯¯ ¯¯\n")
  cat("(A Q1 Q2 Q3 Q4): (", obj$desc, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = Q1 + Q2 + Q3 + Q4
  return(sum(atual$desc[2:5]))
}

geraFilhos.atual <- function(obj) {
  
  filhos <- list()
  
  filhosDesc <- list()
  
  custo <-c()
  
  desc <- obj$desc
  
  sAtual <- as.numeric(desc[desc[1]+1]) ##sujo atualmente??? desc[1] da quadrado atual
  
  ## gera filhos usando todos os operadores 
    ##caso pode limpar
  if(sAtual == 1){
    
    novo <-desc
    
    novo[novo[1]+1]<- 0 ## atribui limpo a posicao que esta o aspirador
    
    filhosDesc <- list(novo)
    
    custo <- c(2)
  }
  ##se posicao atual é 1
  if(desc[1]==1){
    
    filhosDesc <- c(filhosDesc,list( c(2,desc[2:5]), c(3,desc[2:5])))
    
    custo<- c(custo,1,3)
    
  }
  
  if(desc[1]==2){
    
    filhosDesc <- c(filhosDesc,list( c(4,desc[2:5]), c(1,desc[2:5])))
    
    custo<- c(custo,3,1)
  }
  
  if(desc[1]==3){
    
    filhosDesc <- c(filhosDesc,list( c(4,desc[2:5]), c(1,desc[2:5])))
    
    custo<- c(custo,1,3)
  }
  
  if(desc[1]==4){
    
    filhosDesc <- c(filhosDesc,list( c(3,desc[2:5]), c(2,desc[2:5])))
    
    custo<- c(custo,1,3)
    
  }
  
  
  ## gera os objetos Aspiradores para os filhos
  for(i in c(1:length(filhosDesc))){
    filho <- Aspirador(desc = filhosDesc[i], pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + custo[i]
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}