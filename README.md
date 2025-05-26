# 🔗 vitallinkage2

**vitallinkage2** é um pacote R voltado para o **pareamento determinístico de registros** (record linkage) utilizando regras baseadas em igualdade exata entre variáveis-chave. 
Ele é ideal para quem precisa identificar e consolidar registros duplicados ou correspondentes em bases administrativas, censitárias ou, especialmente, bases de saúde, com foco em **desempenho, simplicidade e extensibilidade**.

## ✨ Motivação

Em bases de dados reais, especialmente grandes conjuntos administrativos ou bases de vigilância epidemiológica, é comum encontrar registros duplicados ou que representam a mesma pessoa/entidade em diferentes sistemas.
O `vitallinkage2` fornece ferramentas automatizadas para aplicar **regras de linkage determinístico**, atribuir identificadores de grupo e garantir fechamento transitivo, tudo de forma modular e escalável.

---

## ⏬ Instalação

Instale diretamente via GitHub:

```r
# Instale o pacote devtools se necessário
install.packages("devtools")

# Instale o vitallinkage2
devtools::install_github("erikluancosta/vitallinkage2")
```

## 🛠️ Funcionalidades principais
iniciar_pareamento()
Aplica uma etapa inicial de pareamento determinístico com base em igualdade exata entre variáveis. Retorna um data.frame com identificadores atribuídos (par_1 e par_c1), preservando os registros originais.

```r
# aplicação direta do pacote
df <- iniciar_pareamento(df, variaveis = c("var1", "var2", ...))

# com dplyr
df <- df %>% iniciar_pareamento(c("var1", "var2", ...))

```

regra_pareamento()
Executa regras adicionais de linkage determinístico sobre um data.table, atribuindo identificadores consistentes aos grupos detectados. Pode ser usada em sequência com diferentes regras (par_c2, par_c3, etc.).

```r
# aplicação direta do pacote
df <- regra_pareamento(df, variaveis = c("var1", "var2", ...), num_regra=2)

# com dplyr
df <- df %>% regra_pareamento(c("var1", "var2", ...), 2)
```

meio_de_campo()
Aplica fechamento transitivo nos pares de linkage, consolidando identificadores em casos de ligações indiretas (ex: A-B-C → todos com o mesmo código).
Esta função não precisa ser aplicada separadamente pois já está contida na função `regras_pareamento()`

  
## 🧱 Estrutura das colunas utilizadas

| Coluna   | Descrição                                             |
| -------- | ----------------------------------------------------- |
| `par_1`  | Código do grupo principal consolidado                 |
| `par_2`  | Código temporário do linkage                          |
| `par_cX` | Código da regra `X` aplicada (ex: `par_c1`, `par_c2`) |
| `flag`   | Indicador temporário usado durante as regras          |




## 📚 Dependências
data.table -> estrutura geral do código
dplyr      -> Parte do processo
tictoc     -> Cronometrar o tempo de execução

📄 Licença
Este projeto está licenciado sob a licença MIT. Veja o arquivo LICENSE para mais detalhes.

*Caso as funções do pacote para gerar um linkage sejam utilizadas, o pacote deverá ser referenciado na metodologia*

