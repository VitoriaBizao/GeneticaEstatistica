passo_a_passo_soma <- function(A, B, nome_A = "A", nome_B = "B") {
  if (!all(dim(A) == dim(B))) stop("As matrizes devem ter a mesma dimensão.")
  
  # Função auxiliar para formatar matriz em LaTeX
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # Cria a matriz de strings com a operação (ex: "1 + (-1)")
  operacao_mat <- matrix(
    paste0(A, " + (", B, ")"), 
    nrow = nrow(A), ncol = ncol(A)
  )
  
  # Resultado final
  resultado <- A + B
  
  # Monta a string final em LaTeX (LINHA CORRIGIDA)
  cat("$$ \\mathbf{", nome_A, "} + \\mathbf{", nome_B, "} = ", 
      to_latex(A), " + ", to_latex(B), " = ", 
      to_latex(operacao_mat), " = ", to_latex(resultado), " $$\n", sep = "")
}


A <- matrix(c(1, 2, 0, 2, 3, -1), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 2, 0, -2, 2, -3), nrow = 3, byrow = TRUE)

# Chamada da função que gera o texto LaTeX
passo_a_passo_soma(A, B)


passo_a_passo_mult <- function(A, B, nome_A = "A", nome_B = "B") {
  # Verificação da regra fundamental da multiplicação de matrizes
  if (ncol(A) != nrow(B)) {
    stop("Erro: O número de colunas da 1ª matriz deve ser igual ao número de linhas da 2ª matriz.")
  }
  
  # Função auxiliar para formatar matriz em LaTeX
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # Cria a matriz de strings com as operações de linha x coluna
  operacao_mat <- matrix("", nrow = nrow(A), ncol = ncol(B))
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(B)) {
      # Cria os termos no formato (a)(b) e os une com " + "
      termos <- paste0("(", A[i, ], ")(", B[, j], ")")
      operacao_mat[i, j] <- paste(termos, collapse = " + ")
    }
  }
  
  # Calcula o resultado final usando o operador de multiplicação de matrizes do R
  resultado <- A %*% B
  
  # Monta a string final em LaTeX
  cat("$$ \\mathbf{", nome_A, "}\\mathbf{", nome_B, "} = ", 
      to_latex(A), " ", to_latex(B), " = ", 
      to_latex(operacao_mat), " = ", to_latex(resultado), " $$\n", sep = "")
}

#| results: asis

# Matrizes da Questão 4
A <- matrix(c(1, 0, 2, -1), nrow = 2, byrow = TRUE)
B <- matrix(c(3, -2, 0, 1), nrow = 2, byrow = TRUE)

# Chamada da função
passo_a_passo_mult(A, B)

# Função para Passo a Passo (Subtração)
passo_a_passo_subtracao <- function(A, B, nome_A = "A", nome_B = "B") {
  if (!all(dim(A) == dim(B))) stop("As matrizes devem ter a mesma dimensão.")
  
  # Função auxiliar para formatar matriz em LaTeX
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # Cria a matriz de strings com a operação detalhada (ex: "1 - (-1)")
  # Usamos parênteses no segundo termo para evitar confusão de sinais
  operacao_mat <- matrix(
    paste0(A, " - (", B, ")"), 
    nrow = nrow(A), ncol = ncol(A)
  )
  
  # Resultado final
  resultado <- A - B
  
  # Monta a string final em LaTeX
  cat("$$ \\mathbf{", nome_A, "} - \\mathbf{", nome_B, "} = ", 
      to_latex(A), " - ", to_latex(B), " = ", 
      to_latex(operacao_mat), " = ", to_latex(resultado), " $$\n", sep = "")
}

#| results: asis

# Definindo as matrizes do exemplo
A <- matrix(c(1, 2, 2, 3, 0, -1), nrow = 3, byrow = TRUE)
B <- matrix(c(-1, 2, 0, -2, 2, -3), nrow = 3, byrow = TRUE)

# Executando o passo a passo
passo_a_passo_subtracao(A, B, nome_A = "A", nome_B = "B")
passo_a_passo_subtracao(B, A, nome_A = "B", nome_B = "A")

passo_a_passo_kronecker <- function(A, B, nome_A = "A", nome_B = "B") {
  
  # Função auxiliar para formatar matriz em LaTeX
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # 1. Matriz de Blocos: mostra cada elemento de A multiplicando a matriz B
  # Exemplo: a11 B & a12 B
  blocos_mat <- matrix("", nrow = nrow(A), ncol = ncol(A))
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      blocos_mat[i, j] <- paste0(A[i, j], " \\mathbf{", nome_B, "}")
    }
  }
  
  # 2. Resultado Real (Kronecker)
  resultado <- A %x% B
  
  # Monta a string final em LaTeX com quebras de linha para não estourar a tela
  cat("$$ \\mathbf{", nome_A, "} \\otimes \\mathbf{", nome_B, "} = ", 
      to_latex(A), " \\otimes \\mathbf{", nome_B, "} $$ \n\n",
      "$$= ", to_latex(blocos_mat), "$$ \n\n",
      "$$= ", to_latex(resultado), "$$ \n", sep = "")
}

A <- matrix(c(1, 2, 0, 1), nrow = 2, byrow = TRUE)
B <- matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)

passo_a_passo_kronecker(A, B)

passo_a_passo_traco <- function(A, nome_A = "A") {
  # 1. Verificação: A matriz precisa ser quadrada
  if (nrow(A) != ncol(A)) {
    stop("O traço só é definido para matrizes quadradas.")
  }
  
  # 2. Extrair elementos da diagonal
  elementos_diag <- diag(A)
  
  # 3. Calcular o valor final
  resultado_traco <- sum(elementos_diag)
  
  # 4. Formatar a soma (ex: "a11 + a22 + a33")
  soma_detalhada <- paste(elementos_diag, collapse = " + ")
  
  # 5. Formatar a matriz para exibição em LaTeX
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # 6. Output em LaTeX
  cat("$$ \\text{Tr}(\\mathbf{", nome_A, "}) = \\text{Tr}", to_latex(A), "$$ \n\n",
      "$$\\text{Tr}(\\mathbf{", nome_A, "}) = ", soma_detalhada, " = ", resultado_traco, "$$ \n", sep = "")
}

M <- matrix(c(10, 2, 3,
              4, 5, 6,
              7, 8, -2), nrow = 3, byrow = TRUE)

passo_a_passo_traco(M, nome_A = "M")

passo_a_passo_particionada <- function(A, B, split_row_A, split_col_A, split_row_B, split_col_B) {
  
  # 1. Extração dos blocos de A
  A11 <- A[1:split_row_A, 1:split_col_A, drop=F]
  A12 <- A[1:split_row_A, (split_col_A+1):ncol(A), drop=F]
  A21 <- A[(split_row_A+1):nrow(A), 1:split_col_A, drop=F]
  A22 <- A[(split_row_A+1):nrow(A), (split_col_A+1):ncol(A), drop=F]
  
  # 2. Extração dos blocos de B
  B11 <- B[1:split_row_B, 1:split_col_B, drop=F]
  B12 <- B[1:split_row_B, (split_col_B+1):ncol(B), drop=F]
  B21 <- B[(split_row_B+1):nrow(B), 1:split_col_B, drop=F]
  B22 <- B[(split_row_B+1):nrow(B), (split_col_B+1):ncol(B), drop=F]
  
  # 3. Verificação de conformidade dos blocos internos
  # A conformidade exige que o número de colunas do bloco à esquerda 
  # seja igual ao número de linhas do bloco à direita na soma.
  
  # 4. Formatação LaTeX
  cat("### Passo 1: Representação Simbólica por Blocos\n")
  cat("$$ \\mathbf{A} = \\begin{bmatrix} \\mathbf{A}_{11} & \\mathbf{A}_{12} \\\\ \\mathbf{A}_{21} & \\mathbf{A}_{22} \\end{bmatrix}, \\quad \\mathbf{B} = \\begin{bmatrix} \\mathbf{B}_{11} & \\mathbf{B}_{12} \\\\ \\mathbf{B}_{21} & \\mathbf{B}_{22} \\end{bmatrix} $$ \n\n")
  
  cat("### Passo 2: Fórmula da Multiplicação em Blocos\n")
  cat("$$ \\mathbf{AB} = \\begin{bmatrix} 
      \\mathbf{A}_{11}\\mathbf{B}_{11} + \\mathbf{A}_{12}\\mathbf{B}_{21} & \\mathbf{A}_{11}\\mathbf{B}_{12} + \\mathbf{A}_{12}\\mathbf{B}_{22} \\\\ 
      \\mathbf{A}_{21}\\mathbf{B}_{11} + \\mathbf{A}_{22}\\mathbf{B}_{21} & \\mathbf{A}_{21}\\mathbf{B}_{12} + \\mathbf{A}_{22}\\mathbf{B}_{22} 
      \\end{bmatrix} $$ \n\n")
  
  # 5. Cálculo dos blocos resultantes
  C11 <- A11 %*% B11 + A12 %*% B21
  C12 <- A11 %*% B12 + A12 %*% B22
  C21 <- A21 %*% B11 + A22 %*% B21
  C22 <- A21 %*% B12 + A22 %*% B22
  
  # 6. Resultado Final concatenado
  C_final <- rbind(cbind(C11, C12), cbind(C21, C22))
  
  cat("### Passo 3: Resultado Numérico Final\n")
  # Função auxiliar para imprimir matriz
  to_latex <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  cat("$$ \\mathbf{AB} = ", to_latex(C_final), "$$ \n")
}
#| results: asis

A <- matrix(c(2, 1, 2,
              3, 2, 0,
              1, 0, 1), nrow = 3, byrow = TRUE)

B <- matrix(c(1, 1, 1, 0,
              2, 1, 1, 2,
              2, 3, 1, 2), nrow = 3, byrow = TRUE)

passo_a_passo_particionada(A, B, split_row_A = 2, split_col_A = 2, 
                           split_row_B = 2, split_col_B = 3)

passo_a_passo_particionada_detalhado <- function(A, B, split_row_A, split_col_A, split_row_B, split_col_B) {
  
  # 1. Extração dos blocos
  A11 <- A[1:split_row_A, 1:split_col_A, drop=F]; A12 <- A[1:split_row_A, (split_col_A+1):ncol(A), drop=F]
  A21 <- A[(split_row_A+1):nrow(A), 1:split_col_A, drop=F]; A22 <- A[(split_row_A+1):nrow(A), (split_col_A+1):ncol(A), drop=F]
  
  B11 <- B[1:split_row_B, 1:split_col_B, drop=F]; B12 <- B[1:split_row_B, (split_col_B+1):ncol(B), drop=F]
  B21 <- B[(split_row_B+1):nrow(B), 1:split_col_B, drop=F]; B22 <- B[(split_row_B+1):nrow(B), (split_col_B+1):ncol(B), drop=F]
  
  # Função auxiliar para LaTeX
  to_l <- function(mat) {
    inner <- apply(mat, 1, paste, collapse = " & ")
    paste0("\\begin{bmatrix} ", paste(inner, collapse = " \\\\ "), " \\end{bmatrix}")
  }
  
  # 2. Cálculos Intermediários
  C11_a <- A11 %*% B11; C11_b <- A12 %*% B21; C11 <- C11_a + C11_b
  C12_a <- A11 %*% B12; C12_b <- A12 %*% B22; C12 <- C12_a + C12_b
  C21_a <- A21 %*% B11; C21_b <- A22 %*% B21; C21 <- C21_a + C21_b
  C22_a <- A21 %*% B12; C22_b <- A22 %*% B22; C22 <- C22_a + C22_b
  
  # 3. Output das contas por bloco
  cat("### 1. Cálculo dos Blocos Resultantes\n\n")
  
  cat("Para o bloco $\\mathbf{C}_{11} = \\mathbf{A}_{11}\\mathbf{B}_{11} + \\mathbf{A}_{12}\\mathbf{B}_{21}$:\n")
  cat("$$", to_l(C11_a), " + ", to_l(C11_b), " = ", to_l(C11), "$$\n\n")
  
  cat("Para o bloco $\\mathbf{C}_{12} = \\mathbf{A}_{11}\\mathbf{B}_{12} + \\mathbf{A}_{12}\\mathbf{B}_{22}$:\n")
  cat("$$", to_l(C12_a), " + ", to_l(C12_b), " = ", to_l(C12), "$$\n\n")
  
  cat("Para o bloco $\\mathbf{C}_{21} = \\mathbf{A}_{21}\\mathbf{B}_{11} + \\mathbf{A}_{22}\\mathbf{B}_{21}$:\n")
  cat("$$", to_l(C21_a), " + ", to_l(C21_b), " = ", to_l(C21), "$$\n\n")
  
  cat("Para o bloco $\\mathbf{C}_{22} = \\mathbf{A}_{21}\\mathbf{B}_{12} + \\mathbf{A}_{22}\\mathbf{B}_{22}$:\n")
  cat("$$", to_l(C22_a), " + ", to_l(C22_b), " = ", to_l(C22), "$$\n\n")
  
  # 4. Junção Final
  C_final <- rbind(cbind(C11, C12), cbind(C21, C22))
  cat("### 2. Resultado Final Consolidado\n")
  cat("$$ \\mathbf{AB} = ", to_l(C_final), "$$ \n")
}
#| results: asis

A <- matrix(c(2, 1, 2,
              3, 2, 0,
              1, 0, 1), nrow = 3, byrow = TRUE)

B <- matrix(c(1, 1, 1, 0,
              2, 1, 1, 2,
              2, 3, 1, 2), nrow = 3, byrow = TRUE)

# A: corte após linha 2 e coluna 2
# B: corte após linha 2 e coluna 3
passo_a_passo_particionada_detalhado(A, B, 2, 2, 2, 3)
