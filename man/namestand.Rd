\name{namestand}
\alias{namestand}
\docType{data}
\title{
Dicionário de Padronização de Variáveis por Fonte de Dados
}
\description{
O dataset \code{namestand} contém um dicionário com a padronização de nomes de variáveis
provenientes de diferentes fontes de dados de saúde pública. Ele auxilia em processos
de harmonização de nomes de campos para integração e linkage de bases como SINAN, SIM, SIH e ESUS-APS.
}
\usage{data("namestand")}
\format{
  A data frame with 562 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{a numeric vector (identificador interno; pode conter \code{NA})}
    \item{\code{var_names_orig}}{a character vector (nome da variável na base original)}
    \item{\code{fonte}}{a character vector (nome padronizado da base de origem, ex: "SIM", "SIH")}
    \item{\code{stanard_name}}{a character vector (nome padronizado da variável)}
    \item{\code{observação}}{a character vector (observações adicionais sobre a variável)}
  }
}
\details{
O objeto \code{namestand} foi criado a partir da base \code{vitallinkage::namestand} com complementações manuais,
incluindo a adição de variáveis essenciais não presentes no original. Foram incluídas variáveis de sistemas como:
SINAN_VIOL, SIM, SIH, ESUS_APS e SINAN_EXO. Também houve ajustes de padronização em nomes como "SINAN" para "SINAN_VIOL"
e "ESUS_AB" para "ESUS_APS".

Adicionalmente, variáveis de intoxicação exógena (\code{SINAN_EXO}) foram adicionadas a partir de um arquivo externo Excel.
}
\source{
Base original proveniente de \code{vitallinkage::namestand}, complementada com dados do arquivo:
\\ \code{nm_stand_sinan_intox_exogena.xlsx}, localizado em \code{linkage_recife/1_base_bruta/dados/}
}
\references{
Vital Strategies Brasil. Projeto de Integração e Linkage de Dados de Saúde.
}
\examples{
data(namestand)
str(namestand)
subset(namestand, fonte == "SIM")
}
\keyword{datasets}
