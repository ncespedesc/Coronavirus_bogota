<center> <h1>Simulacion estocastica de coronavirus-2019-ncov (under construction)  </h1> </center>
<p align="center">
  <img width="600" height="200" src="https://www.shock.co/sites/default/files/styles/apertura_desktop/public/content_files/2018_11/image_article/los-zombies-shock-disfraces.jpg?itok=uzbgMUBm&timestamp=1541358745">
</p>


### Datos y Parametrizacion 
Aqui utilizamos los casos de muertes reportados globalmente para la parametrización del modelo es realizada de acuerdo a los casos reportados en China, parametros fueron calulados a partir de un modelo deterministico [blog](https://www.r-bloggers.com/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov//). El modelo aqui es un poco diferente este es un modelo estocástico es decir vamos a simular diferentes realidades alternativas que podría tener un evento epidémico.

###  Algunas cosideraciones
 Este es tan solo un modelo de muchos no **representa necesariamente la realidad** y asume que **no existe ninguna medida de control** como cuarentenas, restriccion de movimientos entre personas es meramente didáctico.
aqui utilizamos un modelo simple  que considera tres grupos: **susceptible** -> **infectados** -> **recuperados**
![fuente: institutefordiseasemodeling](https://institutefordiseasemodeling.github.io/Documentation/malaria/_images/SIR-SIRS.png)
fuente: institutefordiseasemodeling


### Simulamos diferentes escenarios para la enfermedad  
Cada línea azul (20 en total) representa una simulación que es un escenario posible para una curva epidémica, presentando una probabilidad diferente en el numero de infectados finales, los puntos rojos(a narajas son los los datos  de China). la escala de tiempo esta en dias.
<p align="center">
  <img width="600" height="400" src="https://github.com/ncespedesc/Coronavirus_bogota/blob/master/bogota1.png?raw=true">
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
library(GillespieSSA)
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

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta/N * I * S
        dI <- beta/N * I * S - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}

init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions


Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par

# Parametros

beta <- as.numeric(Opt_par[1]); # taxa de infecao 
gama <- as.numeric(Opt_par[2]);  # taxa de recuperacao 

par.SIRsd <- c(beta = beta, gama = gama)
# calculando R0
R0 <- as.numeric(beta/gama)
R0  


########################################
# Modelo modelo estocastico gillespe  ----
########################################


# vamos  simular que vai acontecer nos priximos x dias 

model  <- mparse(transitions = c("S -> beta*S*I/(S+I+R) -> I",
                                 "I -> gamma*I -> R"),
                 compartments = c("S", "I", "R"),
                 gdata = c(beta = beta, gamma = gama), # ajsutamos 
                 u0 = data.frame(S = N, I = banco$Infected[2],  R = 0),
                 tspan = 1:35)




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

c_memory <- 19 # numero de nucleos do computador  
cl <- makeCluster(c_memory) #not to overload your computer
registerDoParallel(cl)


individual_sim <- 10


corona_sir_bog <- foreach(
    
    individual_sim= 1:individual_sim, #n_removed nao ultrapasar o numero de nos rankeados 
    .combine=rbind,
    .packages= c("SimInf", "tidyverse")) %dopar%
    {
        coroneme_esta_siminf(model) }


stopCluster(cl)

# agora plotamos a galera 


plotb1 <- plot1
for (i in 1:individual_sim) {
    temp <- corona_sir_bog %>% filter(simulation == i) %>% group_by(node, time,simulation) %>% summarise(I= sum(I))
    plotb1 <- plotb1 + geom_line(data = temp,
                                 mapping = aes(x= time, y = I),
                                 colour = "#00a8cc",
                                 alpha = 0.7)+labs(x= "Dias", y = "Infectados")+
        ggtitle("2019-nCoV Bogota")
}


plotb1+xlim(0,35)


ggsave("bogota.png", width = 6, height = 3)

```
