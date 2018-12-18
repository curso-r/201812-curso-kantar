
reg_tree <- function(formula, data, minsize) {
  
  sse_var <- function(x, y) {
    splits <- sort(unique(x))
    sse <- c()
    for (i in seq_along(splits)) {
      sp <- splits[i]
      sse[i] <- sum((y[x < sp] - mean(y[x < sp]))^2) +
        sum((y[x >= sp] - mean(y[x >= sp]))^2) 
    }
    split_at <- splits[which.min(sse)]
    return(c(sse = min(sse), split = split_at))
  }
  
  # garante que é data.frame
  data <- as.data.frame(data)
  
  # componentes da formula
  formula <- terms.formula(formula)
  
  # matriz de planejamento
  X <- model.matrix(formula, data)[,-1]
  
  # variavel resposta
  y <- data[, as.character(formula)[2]]
  
  # inicializa loop while
  do_splits <- TRUE
  
  # cria o output data.frame com as quebras e as observaçoes
  tree_info <- data.frame(
    NODE = 1, 
    NOBS = nrow(data), 
    FILTER = NA,
    TERMINAL = "SPLIT",
    stringsAsFactors = FALSE
  )
  
  # continua a quebrar até só sobrar folhas
  while(do_splits) {
    
    # qual nó tem que ser quebrado
    to_calculate <- which(tree_info$TERMINAL == "SPLIT")
    
    for (j in to_calculate) {
      
      # tratamento especial da raiz
      if (!is.na(tree_info[j, "FILTER"])) {
        # subset dos dados conforme a quebra
        this_data <- subset(data, eval(parse(text = tree_info[j, "FILTER"])))
        # pega a matriz de planejamento
        X <- model.matrix(formula, this_data)[,-1]
      } else {
        this_data <- data
      }
      
      # calcula o ganho em SSE
      splitting <- apply(X,  MARGIN = 2, FUN = sse_var, y = y)
      
      # pega a quebra do SSE mínimo
      tmp_splitter <- which.min(splitting[1,])
      
      # define maxnode
      mn <- max(tree_info$NODE)
      
      # escreve as regras de quebra
      tmp_filter <- c(paste(names(tmp_splitter), ">=", splitting[2, tmp_splitter]),
                      paste(names(tmp_splitter),  "<", splitting[2, tmp_splitter]))
      
      # checa se a quebra já foi feita antes
      split_here  <- !sapply(tmp_filter,
                             FUN = function(x,y) any(grepl(x, x = y)),
                             y = tree_info$FILTER)
      
      # anexa a quebra nova
      if (!is.na(tree_info[j, "FILTER"])) {
        tmp_filter  <- paste(tree_info[j, "FILTER"], 
                             tmp_filter, sep = " & ")
      } 
      
      # pega o número de observacoes da quebra
      tmp_nobs <- sapply(tmp_filter,
                         FUN = function(i, x) {
                           nrow(subset(x = x, subset = eval(parse(text = i))))
                         },
                         x = this_data)  
      
      # verifica se tem tamanho suficiente
      if (any(tmp_nobs <= minsize)) {
        split_here <- rep(FALSE, 2)
      }
      
      # cria um data.frame com os "filhos"
      children <- data.frame(NODE = c(mn+1, mn+2),
                             NOBS = tmp_nobs,
                             FILTER = tmp_filter,
                             TERMINAL = rep("SPLIT", 2),
                             row.names = NULL)[split_here,]
      
      # sobrescreve o nó atual
      tree_info[j, "TERMINAL"] <- ifelse(all(!split_here), "LEAF", "PARENT")
      
      # concatena tudoo
      tree_info <- rbind(tree_info, children)
      
      # checa se tem quebras em aberto ainda
      do_splits <- !all(tree_info$TERMINAL != "SPLIT")
    } # end for
  } # end while
  
  # calcula os valores ajustados
  leafs <- tree_info[tree_info$TERMINAL == "LEAF", ]
  fitted <- c()
  for (i in seq_len(nrow(leafs))) {
    # extrai os indices
    ind <- as.numeric(rownames(subset(data, eval(parse(text = leafs[i, "FILTER"])))))
    # estimativa é a media de y na folha
    fitted[ind] <- mean(y[ind])
  }
  
  # returna tudo
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data))
}



teste <- reg_tree(Sepal.Length ~ Sepal.Width + Petal.Length, data = iris, minsize = 1)
