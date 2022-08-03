library(tidyverse)
library(deSolve)

parameters <- c(r = 0.3, a = 0.1, b = 0.4, e = 0.5)

state <- c(N = 2, P = 2)

lotka_volterra <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <-  r*N - a*N*P
    dP <-  e*a*N*P - b*P
    list(c(dN, dP))
  })
}

times <- seq(0, 100, by = 0.2)

sol = as.data.frame(
  ode(y = state, times = times, func = lotka_volterra, parms = parameters)
)

sol2 = gather(sol, species, individuals, -time)

ggplot(sol2) + 
  geom_line(aes(time, individuals, colour = species))+
   theme_minimal() +
  theme(
    plot.background = element_rect(fill = rgb(.2,.21,.27)),
    text = element_text(colour = 'grey'), 
    axis.text = element_text(colour = 'grey'), 
    panel.grid = element_line(colour = 'grey')
  )

ggplot(sol) + 
  geom_path(aes(N, P), colour = 'red', size = 1)+
   theme_minimal() +
  theme(
    plot.background = element_rect(fill = rgb(.2,.21,.27)),
    text = element_text(colour = 'grey'), 
    axis.text = element_text(colour = 'grey'), 
    panel.grid = element_line(colour = 'grey')
  )

parameters <- c(r = 0.3, a = 0.1, b = 0.4, e = 0.5, K = 15)
state <- c(N = 2, P = 2)
lotka_volterra_K <- function(t, state, parameters) {
  with(as.list(c(state, parameters)), {
    dN <-  (1 - N/K)*r*N - a*N*P
    dP <-  e*a*N*P - b*P
    list(c(dN, dP))
  })
}

times <- seq(0, 100, by = 0.2)
sol = as.data.frame(
  ode(y = state, times = times, func = lotka_volterra_K, parms = parameters)
)


soll = gather(sol, species, individuals, -time)

ggplot(soll) + 
  geom_line(aes(time, individuals, colour = species))+
   theme_minimal() +
  theme(
    plot.background = element_rect(fill = rgb(.2,.21,.27)),
    text = element_text(colour = 'grey'), 
    axis.text = element_text(colour = 'grey'), 
    panel.grid = element_line(colour = 'grey')
  )

ggplot(sol) + 
  geom_path(aes(N, P, colour = time), size = 1)+
   theme_minimal() +
  theme(
    plot.background = element_rect(fill = rgb(.2,.21,.27)),
    text = element_text(colour = 'grey'), 
    axis.text = element_text(colour = 'grey'), 
    panel.grid = element_line(colour = 'grey')
  ) + 
  scale_color_gradientn(colours = rainbow(9))

sol[nrow(sol),]
