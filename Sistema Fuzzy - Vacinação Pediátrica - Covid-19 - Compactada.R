#Instalação do pacote
install.packages("sets", dependencies=T)

#Carregamento do pacote
library(sets)

#Definição do Universo
#O primeiro passo é definir o intervalo do sistema fuzzy, em que todos os valores devem pertencer.
#sets_options("universe", seq(from = 0, to = 100, by = 0.01))
sets_options("universe", seq(0, 100, 0.01))

#Criação de variáveis
#O próximo passo é definir as variáveis do sistema fuzzy.
#Nesse passo, é preciso especificar os atributos das variáveis e atribuir um valor a eles.
#fuzzy_partition é um objeto da classe fuzzy_variable
#sd é um fator de escala numérica (desvio padrão) usado para a função de pertinência resultante, no caso Situação
#O desvio padrão é uma medida que expressa o grau de dispersão de um conjunto de dados, indicando o quanto um conjunto de dados é uniforme. Quanto mais próximo de 0 for o desvio padrão, mais homogêneos são os dados.
#A variável linguística Situação é a resultante, e nela é que se dará a definição da função utilizada para retorno da análise dos dados. No caso estamos utilizando fuzzy_cone que é um caso especial de fuzzy_triangular, definindo um triângulo isósceles, sendo necessário estabelecer também o raio (radius = 25), para obter os cantos da linha de base do cone
variaveis <- set(
  Vacinados = fuzzy_partition(varnames = c(Zero_VinteQuatro = 0, VinteCinco_QuarentaNove = 25, Cinquenta_SetentaQuatro = 50, SetentaCinco_Cem = 75), sd = 10),
  Situacao = fuzzy_partition(varnames = c(Inicial = 0, Intermediária = 25, Avançada = 50, Concluindo = 75), FUN = fuzzy_cone, radius = 25)
)

#Definição das regras
#As regras difusas são as ligações entre as variáveis, definindo um resultado final que é atribuído na variável resultante
regras <-
set(
  fuzzy_rule(Vacinados %is% Zero_VinteQuatro, Situacao %is% Inicial),
  fuzzy_rule(Vacinados %is% VinteCinco_QuarentaNove, Situacao %is% Intermediária),
  fuzzy_rule(Vacinados %is% Cinquenta_SetentaQuatro, Situacao %is% Avançada),
  fuzzy_rule(Vacinados %is% SetentaCinco_Cem, Situacao %is% Concluindo)
)

#Construção do sistema
sistema <- fuzzy_system(variaveis, regras)
sistema
plot(sistema)

#Fazendo inferência
#Neste passo ocorrem as operaÃ§Ãµes com conjuntos fuzzy propriamente ditas, de acordo com os valores atribuídos às variáveis linguísticas
inferencia <- fuzzy_inference(sistema, list(Vacinados = 0.0))
inferencia
plot(inferencia)

#Defuzificação
#method = c("meanofmax", "smallestofmax", "largestofmax", "centroid"))
#Neste passo é definido o método para o cálculo do resultado: "centroid" calcula a média aritmética dos elementos do conjunto, usando os valores de pertinência como pesos. "smallestofmax" / "meanofmax" / "largestofmax" retorna o mínimo / média / máximo de todos os elementos do conjunto com grau de adesão máximo
def = gset_defuzzify(inferencia, "centroid")
def

#Analisando o resultado
#Neste passo é registrado o resultado de acordo com as análises estabelecidas
plot(sistema$variables$Situacao)
lines(inferencia, col = "red", lwd=4)

#Redefinição do Universo
sets_options("universe", NULL)

