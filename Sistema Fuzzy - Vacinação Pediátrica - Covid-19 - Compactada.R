#Instala��o do pacote
install.packages("sets", dependencies=T)

#Carregamento do pacote
library(sets)

#Defini��o do Universo
#O primeiro passo � definir o intervalo do sistema fuzzy, em que todos os valores devem pertencer.
#sets_options("universe", seq(from = 0, to = 100, by = 0.01))
sets_options("universe", seq(0, 100, 0.01))

#Cria��o de vari�veis
#O pr�ximo passo � definir as vari�veis do sistema fuzzy.
#Nesse passo, � preciso especificar os atributos das vari�veis e atribuir um valor a eles.
#fuzzy_partition � um objeto da classe fuzzy_variable
#sd � um fator de escala num�rica (desvio padr�o) usado para a fun��o de pertin�ncia resultante, no caso Situa��o
#O desvio padr�o � uma medida que expressa o grau de dispers�o de um conjunto de dados, indicando o quanto um conjunto de dados � uniforme. Quanto mais pr�ximo de 0 for o desvio padr�o, mais homog�neos s�o os dados.
#A vari�vel lingu�stica Situa��o � a resultante, e nela � que se dar� a defini��o da fun��o utilizada para retorno da an�lise dos dados. No caso estamos utilizando fuzzy_cone que � um caso especial de fuzzy_triangular, definindo um tri�ngulo is�sceles, sendo necess�rio estabelecer tamb�m o raio (radius = 25), para obter os cantos da linha de base do cone
variaveis <- set(
  Vacinados = fuzzy_partition(varnames = c(Zero_VinteQuatro = 0, VinteCinco_QuarentaNove = 25, Cinquenta_SetentaQuatro = 50, SetentaCinco_Cem = 75), sd = 10),
  Situacao = fuzzy_partition(varnames = c(Inicial = 0, Intermedi�ria = 25, Avan�ada = 50, Concluindo = 75), FUN = fuzzy_cone, radius = 25)
)

#Defini��o das regras
#As regras difusas s�o as liga��es entre as vari�veis, definindo um resultado final que � atribu�do na vari�vel resultante
regras <-
set(
  fuzzy_rule(Vacinados %is% Zero_VinteQuatro, Situacao %is% Inicial),
  fuzzy_rule(Vacinados %is% VinteCinco_QuarentaNove, Situacao %is% Intermedi�ria),
  fuzzy_rule(Vacinados %is% Cinquenta_SetentaQuatro, Situacao %is% Avan�ada),
  fuzzy_rule(Vacinados %is% SetentaCinco_Cem, Situacao %is% Concluindo)
)

#Constru��o do sistema
sistema <- fuzzy_system(variaveis, regras)
sistema
plot(sistema)

#Fazendo infer�ncia
#Neste passo ocorrem as operações com conjuntos fuzzy propriamente ditas, de acordo com os valores atribu�dos �s vari�veis lingu�sticas
inferencia <- fuzzy_inference(sistema, list(Vacinados = 0.0))
inferencia
plot(inferencia)

#Defuzifica��o
#method = c("meanofmax", "smallestofmax", "largestofmax", "centroid"))
#Neste passo � definido o m�todo para o c�lculo do resultado: "centroid" calcula a m�dia aritm�tica dos elementos do conjunto, usando os valores de pertin�ncia como pesos. "smallestofmax" / "meanofmax" / "largestofmax" retorna o m�nimo / m�dia / m�ximo de todos os elementos do conjunto com grau de ades�o m�ximo
def = gset_defuzzify(inferencia, "centroid")
def

#Analisando o resultado
#Neste passo � registrado o resultado de acordo com as an�lises estabelecidas
plot(sistema$variables$Situacao)
lines(inferencia, col = "red", lwd=4)

#Redefini��o do Universo
sets_options("universe", NULL)

