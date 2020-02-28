#
<center> <h1>Simulacion de coronavirus-2019-ncov aplicado a la ciudad de Bogota </h1> </center>
<p align="center">
  <img width="600" height="200" src="https://www.shock.co/sites/default/files/styles/apertura_desktop/public/content_files/2018_11/image_article/los-zombies-shock-disfraces.jpg?itok=uzbgMUBm&timestamp=1541358745">
</p>

### De donde salieron los datos y la parametrizacion 

 Aqui utilizamos los casos de muertes reportatos globalmente para por infeccion estos pueden ser accesados 
 [aqui](https://www.worldometers.info/coronavirus/) son actualizados diaramente. 
 Necesariamente debo dar los creditos a este [blog](https://www.r-bloggers.com/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov//) de donde salio parte del codigo para calcular tasas de infeccion y recuperacion. Es 
 super recomendado. El modelo aqui  es un poco diferente este es un modelos estocastico es decir vamos a simular diferentes realidades alternativas que podria tener un evento epidemico.
 
 ###  Algunas cosideraciones
  Este es tan solo un modelo de muchos  no **representa necesariamente la realidad** y asume  que **no existe ninguna medida de control** es meramente didactico.
  (si es lo que hace un epidemiologo en sus tiempo libres ... ver como se infectan las ciudades) aqui utilizamos un modelo  simple  que consiera tres grupos: **suceptible** -> **infectados** -> **recuperados**
![fuente: institutefordiseasemodeling](https://institutefordiseasemodeling.github.io/Documentation/malaria/_images/SIR-SIRS.png)
fuente: institutefordiseasemodeling

### Que acontecio en China y otros paises ? 
Este grafico muestra los datos reales de la curva epidemica 
<p align="center">
  <img width="600" height="400" src="https://github.com/ncespedesc/Coronavirus_bogota/blob/master/plot_corona_china.png?raw=true">
</p>

### Simulamos diferentes escenarios para la enfermedad  
Cada linea azul representa una simulacion que es un escenario posible para una curva epidemica 
<p align="center">
  <img width="600" height="400" src="https://github.com/ncespedesc/Coronavirus_bogota/blob/master/bogota.png?raw=true">
</p>


 ### Codigo del modelo en R 
 
Apenas es un preview  se que va a tener muchos ajustes todavia ... si estas  leyendo esto y sabes R no dudes en enviar comentarios y/o sujerencias 
 
```markdown

# Modelo SIR sem demografia (estocastico)
# Utilizando o algoritmo de Gillespie (pacote GillespieSSA)
# Gillespie Stochastic Simulation Algorithm 
#packages 

# pacotes necessarios para as analises 
library(GillespieSSA)
library(ggpubr)
library(deSolve)
library(tidyverse)
library(SimInf)
library(doParallel)

# creamos o dataframe com os dados do outbreeak 

################## creditos aqui : ###################################################
# dados e parametros de https://www.r-bloggers.com/epidemiology-how-contagious-is-novel-coronavirus-2019-ncov/
######################################################################################
setwd("~/COISAS NERDS/coronavirus model")

# number of infected----

# https://www.worldometers.info/coronavirus/  #  numero de mortes 
Infected <-c(45, 62, 121, 198, 291,579,845,1317,2015,2800,4581,6058,7813,9821,11948,14552,
             17389,20628,24553,28276,31439,34876,37552,40553,43099,45170, 59283, 64437)

# setamos algun paramentos do modelo ----

Day <- 1:(length(Infected))
N <- 11000000 # population of wohan china
banco <- data.frame(Infected, Day, N)

# plotamos os casos ----

plot1 <- ggplot()+
    geom_path(banco, mapping = aes(x = Day, y = Infected , colour = (Infected)))+
    geom_point(banco, mapping = aes(x = Day, y =Infected , colour = (Infected)), size= 3)+
    scale_colour_gradient(name = "Cases China", low = "yellow", high = "red", na.value = NA, trans= "log10")+
    labs(x= "Dias", y = "Infectados") ; plot1


ggsave("plot_china.png", width = 8, height = 6,  units= "cm")

#############################
# para bogota               # -----
#############################


N <- 7413000 # population of Bogota 
banco <- data.frame(Infected, Day, N)

SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
        dS <- -beta/N * I * S
        dI <- beta/N * I * S - gamma * I
        dR <- gamma * I
        list(c(dS, dI, dR))
    })
}


# model

library(deSolve)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((Infected - fit)^2)
}

Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"

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


# vamos  simular que vai acontecer nos priximos 15 dias 

model  <- mparse(transitions = c("S -> beta*S*I/(S+I+R) -> I",
                                 "I -> gamma*I -> R"),
                 compartments = c("S", "I", "R"),
                 gdata = c(beta = beta+0.045, gamma = gama+.147),
                 u0 = data.frame(S = N, I = banco$Infected[2],  R = 0),
                 tspan = 1:28)


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

c_memory <- 8 # numero de nucleos do computador  
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

#plotando



plotb1 <- plot1
for (i in 1:individual_sim) {
    temp <- corona_sir_bog %>% filter(simulation == i) %>% group_by(node, time,simulation) %>% summarise(I= sum(I))
    plotb1 <- plotb1 + geom_line(data = temp,
                               mapping = aes(x= time, y = I),
                               colour = "#00a8cc",
                               alpha = 0.7)+labs(x= "Dias", y = "Infectados")+
        ggtitle("2019-nCoV Bogota")
}


```


