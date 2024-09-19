
attach(train) 
#fixação da base de dados para ser possivel usar no R

#ativivação das bibliotecas
library(ggplot2)
library(magrittr)
library(tidyverse) 



#Grafico 1
# Gráfico de barras agrupadas, comparando a média de minutos por dia entre gêneros para cada plataforma
ggplot(train, aes(x = Plataforma, y = `Tempo de uso díario(minutos)`, fill = Gênero)) +
  geom_col(position = "dodge") +
  labs(x = "Plataforma", y = "Média de Minutos por Dia", fill = "Gênero") +
  scale_fill_manual(values = c("Feminino"= "#ff2c97","Masculino"= "#4176e1", "Não-binário" = "#6b3fa0"))+
  ggtitle("Comparação de Tempo Gasto por Plataforma e Gênero") +
  theme(legend.text = element_text(size = 14))+
   theme(
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_line(color = "#A8BAC4", size = 0.3),
    axis.ticks.length = unit(0, "mm"),
    axis.line.y.left = element_line(color = "black"),
    axis.text.x = element_text(family = "Econ Sans Cnd", size = 14))


#Grafico 2

### Criando o Gráfico de Pizza 

# Agrupando os dados por plataforma e calculando o tempo total de uso
dados_agrupados <- train %>%
  group_by(Plataforma) %>%
  summarize(tempo_total = sum(`Tempo de uso díario(minutos)`))

# Criando o gráfico de pizza
ggplot(dados_agrupados, aes(x = "", y = tempo_total, fill = Plataforma)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  scale_fill_manual(values = c("Facebook" = "blue", "Instagram" = "#ff007f", "Snapchat" = "yellow", 	
"LinkedIn" = "#0e76a8", "Telegram" = "orange", "Twitter" = "#00acee","Whatsapp" ="#00bb2d")) +
  geom_text(aes(label = paste0(round(tempo_total/sum(tempo_total)*100, 1), "%")), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size = 7,
            fontface= "bold") +
  labs(title = "Distribuição do Tempo de Uso por Plataforma",
       fill = "Plataforma") +
  theme(legend.text = element_text(size = 20))+
  theme_void()



### GRAFICO 3
# Criando o histograma
#Distribuição do Tempo de Uso Diário dos usuarios 
ggplot(data = train, aes(x = `Tempo de uso díario(minutos)`)) +
  geom_histogram(binwidth = 30, color = "black", fill = "lightblue") +
  labs(x = "Tempo de uso diário (minutos)", y = "Frequência", title = "Distribuição do Tempo de Uso Diário")

ggplot(data = train, aes(x = `Tempo de uso díario(minutos)`)) +
  geom_histogram(binwidth = 30, fill = "lightblue", color = "black") +
  labs(x = "Tempo de uso diário (minutos)", y = "Frequência", 
       title = "Distribuição do Tempo de Uso Diário") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#Distribuição das Idades dos usuarios
ggplot(data = train, aes(x = Idade)) +
  geom_histogram(binwidth = 5, color = "black", fill = "lightblue") +
  labs(x = "Idade", y = "Frequência", title = "Distribuição das Idades")

#Distribuição do Tempo de Uso por Gênero
ggplot(data = train, aes(x = `Tempo de uso díario(minutos)`, fill = Gênero)) +
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 30) +
  geom_density(alpha = 0.2) +
  facet_wrap(~ Gênero) +
  labs(x = "Tempo de uso diário (minutos)", y = "Densidade", title = "Distribuição do Tempo de Uso por Gênero")


#grafico 4 boxplot
ggplot(data = train, aes(x = Gênero, y = `Tempo de uso díario(minutos)`, fill = Gênero)) +
  geom_boxplot(notch = TRUE) +
  labs(
    x = "Gênero",
    y = "Tempo de Uso Diário (minutos)",
    title = "Distribuição do Tempo de Uso por Gênero"
  ) +
  scale_fill_manual(values = c("pink", "blue","red")) +
  theme_classic()


# GRAFICO 5 agora sim é dispersão

# Criando o gráfico de dispersão
ggplot(data = train, aes(x = Idade, y = `Tempo de uso díario(minutos)`)) +
  geom_point(alpha = 0.5,) +
  labs(
    x = "Idade",
    y = "Tempo de Uso Diário (minutos)",
    title = "Relação entre Idade e Tempo de Uso Diário")+
   geom_point(alpha = 0.5)+
  theme_classic() 


# Criando um dataframe de exemplo
dados <- data.frame(Idade = seq(18, 60, by = 1),
      `Tempo de uso díario(minutos)` = rnorm(43, mean = 60, sd = 15))

#Barra
# Criando o gráfico com os eixos ajustados
ggplot(data = train, aes(x = Idade, y = `Tempo de uso díario(minutos)`)) +
  geom_col()+
  labs(
    x = "Idade",
    y = "Tempo de Uso Diário (minutos)",
title = "Relação entre Idade e Tempo de Uso Diário")+ 
  scale_x_continuous(breaks = seq(21, 35, by = 1))+
  theme_minimal()
theme(
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size = 14),
  panel.grid.major = element_line(color = "gray80"))


###G5
# Criando o gráfico de dispersão
ggplot(data = train, aes(x = `Emoção dominante`, y = `Curtidas Recebidas Por Dia`, colors = "Tristeza" = "blue", "Tédio" = "#ff007f", "Felicidade" = "yellow", 	
                         "Neutro" = "#0e76a8", "Ansiedade" = "orange")) +
  geom_boxplot(notch = TRUE) +
  labs(
    x = "Emoção dominante",
    y = "Curtidas Recebidas Por Dia",
    title = "Relação entre Número de Curtidas e Emoções")+
theme_classic()





