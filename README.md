<center> <h1>Simulacion estocastica de coronavirus-2019-ncov (under construction)  </h1> </center>
<p align="center">
  <img width="600" height="200" src="https://www.shock.co/sites/default/files/styles/apertura_desktop/public/content_files/2018_11/image_article/los-zombies-shock-disfraces.jpg?itok=uzbgMUBm&timestamp=1541358745">
</p>


### Datos y Parametrizacion 
Este es un modelo estocástico es decir vamos a simular diferentes curvas possibles  que podrían acontecer en un evento epidémico.

###  Algunas cosideraciones
 Este es tan solo un modelo de muchos no **representa necesariamente la realidad** y asume que **no existe ninguna medida de control** como cuarentenas, restriccion de movimientos entre personas es meramente didáctico.
aqui utilizamos un modelo simple  que considera tres grupos: **susceptible** ->  **Expuestos** -> **infectados** -> **recuperados**
![fuente: researchgate.net](https://www.researchgate.net/profile/Benjamin_Ivorra/publication/318394911/figure/fig9/AS:614332840833042@1523479771923/SEIR-model-flowchart_W640.jpg)
fuente: Mathematical models for introduction, spread and early detection of infectious diseases in veterinary epidemiology
July 2017, Thesis for: PhD. MathematicsAdvisor: Ángel M. Ramos; Benjamin Ivorra; Beatriz Martinéz López


### Simulamos diferentes escenarios para la enfermedad  
Cada línea azul (20 en total) representa una simulación que es un escenario posible para una curva epidémica.
<p align="center">
  <img width="600" height="400" src="https://github.com/ncespedesc/Coronavirus_bogota/blob/master/bogota1.png?raw=true">
</p>

### Media de los casos graves 
A continuacion media de los casos graves (consideramos 1% de los  infectados como graves)
<p align="center">
  <img width="600" height="400" src="https://github.com/ncespedesc/Coronavirus_bogota/blob/master/bogota2.png?raw=true">
</p>


 ### Codigo del modelo en R (en contruccion.... todavia )
 
Modelo en construccion ire acutalizando ... algun dia por ahi 
 
```markdown
##################################
# Coronavirus en bogota @nicolas
#################################
# pacotes 
library('nCov2019')
library(tidyverse)
library(ggpubr)
library(deSolve)
library(SimInf)
library(doParallel)


#carregando os dados  
dxy = load_nCov2019(lang = 'en', source = 'dxy')
head(summary(dxy))
bancoaux <- summary(dxy) %>% as.data.frame() %>% filter(province == "Hubei")
dxy_china <- aggregate(cum_confirm ~ + time, bancoaux, sum)
# ajustamos pro plot 
dxy_china$data <- as.numeric(dxy_china$time)-min(as.numeric(dxy_china$time))+1

#plotando a curva epidemica 

plot1 <- ggplot(dxy_china,
       aes(data,cum_confirm)) +
    geom_line(size = 1 , colour = "red")+ #+ scale_x_date(date_labels = "%d-%m-%Y") + 
    ylab('Confirmed Cases in China') + xlab('Time') + theme_bw() +
    theme(axis.text.x = element_text(hjust = 1)); plot1

ggsave("epidemic_curve.png", width = 6, height = 3)

# carregando as condicoes iniciais 
Infected <- as.numeric(dxy_china$cum_confirm)
    
Day <- 1:(length(Infected))
N <- 7413000 # population of Bogota 
banco <- data.frame(Infected, Day, N)


########################################
#    Parametrizando o modelo           #
########################################

#https://arxiv.org/pdf/2002.06563.pdf
#https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(20)30074-7/fulltext

model <- SEIR(u0 = data.frame(S = N, E = 0, I = 1, R = 0),
              tspan = 1:100,
              beta = 1.75,
              epsilon = 0.2,
              gamma = 0.5)

mod.result <- run(model)
plot(mod.result)

# creamos funcion ----

coroneme_esta_siminf <- function (model1){
    
    model <- model1
    media_infect_df2 <- c()
    for (i in 1:individual_sim) {
        # run simulation 
        result <- run(model = model, threads = 1)
        tr <- trajectory(model = result)
        tr$simulation <- i
        # agrupamos pra poupar memoria 
        media_infect_df2 <- rbind(media_infect_df2, tr)
    }
    return(media_infect_df2)
    
}


# simulamos  rapidinho em paralelo 

c_memory <- 10 # numero de nucleos do computador  
cl <- makeCluster(c_memory) #not to overload your computer
registerDoParallel(cl)


individual_sim <- 20


corona_sir_bog <- foreach(
    
    individual_sim= 1:individual_sim, #n_removed nao ultrapasar o numero de nos rankeados 
    .combine=rbind,
    .packages= c("SimInf", "tidyverse")) %dopar%
    {
        coroneme_esta_siminf(model) }


stopCluster(cl)

# agora plotamos a galera 

corona_sir_bog <- corona_sir_bog%>% mutate(mortality = I*0.01) # assumindo a mortalidade de 1 %

plotb1 <- ggplot()
for (i in 1:individual_sim) {
    temp <- corona_sir_bog %>% filter(simulation == i) %>% 
        group_by(node, time,simulation) %>% summarise(I= mean(mortality))
    
    plotb1 <- plotb1 + geom_line(data = temp,
                                 mapping = aes(x= time, y = I),
                                 colour = "#00a8cc",
                                 alpha = 0.7)+labs(x= "Dias", y = "Infectados")+
        ggtitle("2019-nCoV Bogota")
}



ggsave("bogota.png", width = 6, height = 3)

```
