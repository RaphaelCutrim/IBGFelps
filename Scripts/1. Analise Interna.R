# SETUP ------------------------------------------------------------------------

if(!require('pacman')) install.packages('pacman')

p_load(tidyverse,
       rio,
       janitor,
       paletteer,
       gridExtra,
       cowplot,
       gt,
       summarytools)

# IMPORTANDO DADOS TRATADOS ----------------------------------------------------

ibgfelps <- import("Data/IBGFelps2026_tratado.csv")


# HEATMAP DO PREENCHIMENTO DO FORMULARIO ---------------------------------------

hm <- ibgfelps %>% 
  mutate(hora=as.numeric(hora)+1,
         dia=as.numeric(dia)) %>% 
  tabyl(hora, dia) %>% 
  pivot_longer(cols=2:10,
               names_to='dia',
               values_to='n') %>% 
  mutate(hora=factor(hora)) %>% 
  arrange(dia, hora)

ggplot(hm, aes(dia, hora)) +
  geom_tile(aes(fill=n)) +
  geom_text(aes(label=n)) +
  theme_minimal() +
  scale_fill_gradient(high ='white',
                      low = '#FE3867') +
  labs(title = 'Mapa de Calor da Frequência de Respostas',
       x = 'Dia', y = 'Hora')
  
rm(hm)


## aplicando indicador de bapo Twitch e bapo youtube

ibgfelps <- ibgfelps %>% 
  mutate(hora = as.numeric(hora),
         dia = as.numeric(dia),
         bapo = case_when(dia <= 21 ~ 1,
                          dia == 22 & hora < 12 ~ 1,
                          dia == 22 & hora >= 12 ~ 2,
                          dia >= 23 ~ 2),
         bapo = factor(bapo,
                       levels=1:2,
                       labels=c('Bapo Twitch', 'Bapo YouTube')))

# IDADE ------------------------------------------------------------------------
## vs microondas ---------------------------------------------------------------

ibgfelps <- ibgfelps %>% 
  mutate(idade=case_when(idade == 'Até 13 Anos' ~ 1,
                         idade == '14 - 17 Anos' ~ 2,
                         idade == '18 - 23 Anos' ~ 3,
                         idade == '24 - 28 Anos' ~ 4,
                         idade == '29 - 34 Anos' ~ 5,
                         idade == 'Mais de 35 Anos' ~ 6),
         idade = factor(idade,
                        levels=1:6,
                        labels=c('Até 13 Anos',
                                 '14 - 17 Anos',
                                 '18 - 23 Anos',
                                 '24 - 28 Anos',
                                 '29 - 34 Anos',
                                 'Mais de 35 Anos')))
  

tabyl(ibgfelps, idade, microondas) %>% 
  adorn_percentages('row') %>%
  pivot_longer(cols=2:4,
               names_to='microondas',
               values_to='prop') %>% 
  mutate(prop=round(prop*100, 1),
         microondas = case_when(microondas == 'Sim' ~ 1,
                                microondas == 'Não' ~ 2,
                                microondas == 'Não tenho micro-ondas' ~ 3),
         microondas = factor(microondas,
                             levels=1:3,
                             labels=c('Sim',
                                      'Não',
                                      'Não tenho microondas'))) %>% 
  rename(`Microondas Mutado?`=microondas) %>% 
  ggplot(aes(idade, prop)) +
  geom_col(aes(fill=`Microondas Mutado?`)) +
  theme_minimal() +
  labs(title = 'Porcentagem de microondas mutados por faixa etária',
       x='Faixa Etária',
       y='Proporção de Microondas Mutados (em %)') +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999'))


## vs pedra papel tesoura ------------------------------------------------------

tabyl(ibgfelps, idade, ppt) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols = 2:4,
               names_to='Pedra, Papel ou Tesoura?',
               values_to='prop') %>% 
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(idade, prop)) + 
  geom_col(aes(fill=`Pedra, Papel ou Tesoura?`)) +
  geom_hline(yintercept = 66.6) +
  geom_hline(yintercept = 33.3) +
  theme_minimal() +  
  labs(title = 'Porcentagem de microondas mutados por faixa etária',
       x='Faixa Etária',
       y='Proporção de Microondas Mutados (em %)') +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999'))


## vs dia de semana ------------------------------------------------------------

tabyl(ibgfelps, idade, diaSemana) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:8,
               names_to = 'diaSemana',
               values_to = 'prop') %>% 
  mutate(prop=round(prop*100,1),
         diaSemana = case_when(diaSemana == 'Segunda-Feira' ~ 1,
                               diaSemana == 'Terça-Feira' ~ 2,
                               diaSemana == 'Quarta-Feira' ~ 3,
                               diaSemana == 'Quinta-Feira' ~ 4,
                               diaSemana == 'Sexta-Feira' ~ 5,
                               diaSemana == 'Sábado' ~ 6,
                               diaSemana == 'Domingo' ~ 7),
         diaSemana=factor(diaSemana,
                          levels=1:7,
                          labels=c('Segunda-Feira',
                                   'Terça-Feira',
                                   'Quarta-Feira',
                                   'Quinta-Feira',
                                   'Sexta-Feira',
                                   'Sábado',
                                   'Domingo'))) %>% 
  rename(`Faixa Etária` = idade) %>% 
  ggplot(aes(diaSemana, prop)) +
  geom_col(position = 'dodge',
           aes(group=`Faixa Etária`,
               fill=`Faixa Etária`)) +
  scale_fill_manual(values = c('#fc2357','#FE3867','#f74570',
                               '#ff5c83','#fc7999','#fcbdcc')) +
  theme_minimal() +
  labs(title='Dia preferido de cada faixa etária',
       x = 'Dia Preferido da Semana',
       y = 'Proporção de pessoas de cada faixa etária que escolheram o dia (em %)')

## vs arroz por cima ou por baixo ----------------------------------------------

tabyl(ibgfelps, idade, arrozFeijao) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:4,
               names_to = 'arroz',
               values_to = 'prop') %>% 
  mutate(prop=round(prop*100,1),
         arroz=case_when(arroz == 'Baixo' ~ 1,
                         arroz == 'Lado' ~ 2,
                         arroz == 'Cima' ~ 3),
         arroz = factor(arroz,
                        levels=1:3,
                        labels=c('Baixo', 'Lado', 'Cima'))) %>%
  rename(`Arroz por Cima, por Baixo ou do Lado do Feijão?` = arroz) %>% 
  ggplot(aes(idade, prop)) +
  geom_col(aes(fill=`Arroz por Cima, por Baixo ou do Lado do Feijão?`,
               group = `Arroz por Cima, por Baixo ou do Lado do Feijão?`),
           position='dodge') +
  theme_minimal() +
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999')) +
  labs(x='Faixa Etária',
       y = 'Onde as pessoas de cada faixa etária colocam o arroz (em %)')
  
## vs sim/nao ------------------------------------------------------------------

tabyl(ibgfelps, idade, simNao) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='Sim ou Não?',
               values_to='prop') %>% 
  mutate(prop=round(prop*100,1)) %>% 
  ggplot(aes(idade, prop)) +
  geom_col(aes(fill=`Sim ou Não?`)) +
  theme_minimal() +
  scale_fill_manual(values = c('#FE3867','#ff5c83')) +
  labs(x='Faixa Etária',
       y = 'Proporção de respostas sim e não (em %)')


## vs letra --------------------------------------------------------------------

tabyl(ibgfelps, idade, letra) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:27,
               names_to = 'letra',
               values_to = 'prop') %>% 
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(idade, letra)) +
  geom_tile(aes(fill=prop)) +
  geom_text(aes(label=prop)) +
  scale_fill_gradient(low ='white',
                      high = '#FE3867') +
  theme_minimal() +
  labs(x = 'Faixa Etária', y = 'Em Qual Letra Moraria')


## vs numero -------------------------------------------------------------------

tabyl(ibgfelps, idade, numero) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:11,
               names_to = 'numero',
               values_to = 'prop') %>% 
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(idade, numero)) +
  geom_tile(aes(fill=prop)) +
  geom_text(aes(label=prop)) +
  scale_fill_gradient(low ='white',
                      high = '#FE3867') +
  theme_minimal() +
  labs(x = 'Faixa Etária', y = 'Em Qual Número Moraria')

## vs hamburguer ---------------------------------------------------------------

### n de ingredientes

tabyl(ibgfelps, idade, nHamburguer) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:16,
               names_to = 'ingredientes',
               values_to = 'prop') %>% 
  mutate(prop = round(prop*100,1),
         ingredientes = as.character(ingredientes),
         ingredientes = factor(ingredientes,
                               levels=1:15,
                               labels=as.character(1:15))) %>% 
  ggplot(aes(idade, ingredientes)) +
  geom_tile(aes(fill=prop)) +
  geom_text(aes(label=prop)) +
  scale_fill_gradient(low ='white',
                      high = '#FE3867') +
  theme_minimal() +
  labs(x = 'Faixa Etária', y = 'Quantos Ingredientes coloca no seu hamburguer')


### quais ingredientes
PaoCima <- tabyl(ibgfelps, idade, hPaoCima) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pão de Cima')

Ketchup <- tabyl(ibgfelps, idade, hKetchup) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Ketchup')

Mostarda <- tabyl(ibgfelps, idade, hMostarda) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Mostarda')

Maionese <- tabyl(ibgfelps, idade, hMaionese) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Maionese')

Pimenta <- tabyl(ibgfelps, idade, hPimenta) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pimenta')

Queijo <- tabyl(ibgfelps, idade, hQueijo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Queijo')

Hamburguer <- tabyl(ibgfelps, idade, hHamburguer) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Hamburguer')

HamburguerVegano <- tabyl(ibgfelps, idade, hHamburguerVegano) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Hamburguer Vegano')

Alface <- tabyl(ibgfelps, idade, hAlface) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Alface')

Tomate <- tabyl(ibgfelps, idade, hTomate) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Tomate')

Picles <- tabyl(ibgfelps, idade, hPicles) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Picles')

Bacon <- tabyl(ibgfelps, idade, hBacon) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Bacon')

PaoBaixo <- tabyl(ibgfelps, idade, hPaoBaixo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pão de Baixo')

Ovo <- tabyl(ibgfelps, idade, hOvo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Ovo')

Cebola <- tabyl(ibgfelps, idade, hCebola) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Cebola')

rbind(PaoCima, Ketchup, Mostarda, Maionese, Pimenta, Queijo, Hamburguer,
      HamburguerVegano, Alface, Tomate, Picles, Cebola, Ovo, Bacon, PaoBaixo) %>% 
  mutate(prop=round(prop*100,1),
         pres=ifelse(pres==1, 'Sim', 'Não')) %>%
  rename(`Usou o Ingrediente?` = pres) %>% 
  ggplot(aes(prop, idade)) +
  geom_col(aes(fill=`Usou o Ingrediente?`)) +
  facet_wrap(vars(ingrediente)) +
  scale_fill_manual(values = c('#FE3867','#ff5c83')) +
  labs(y = 'Faixa Etária', x = 'Proporção de pessoas que usou cada ingrediente (em %)') +
  theme_minimal()

# ESCOLARIDADE -----------------------------------------------------------------

ibgfelps <- ibgfelps %>% 
  mutate(escolaridade = case_when(escolaridade == 'Fundamental Incompleto/Cursando' ~ 1,
                                  escolaridade == 'Fundamental Completo' ~ 2,
                                  escolaridade == 'Médio Incompleto/Cursando' ~ 3,
                                  escolaridade == 'Médio Completo' ~ 4,
                                  escolaridade == 'Superior Incompleto/Cursando' ~ 5,
                                  escolaridade == 'Superior Completo' ~ 6,
                                  escolaridade == 'Pós-graduação Incompleto/Cursando' ~ 7,
                                  escolaridade == 'Pós-graduação Completo' ~ 8),
         escolaridade = factor(escolaridade,
                               levels=1:8,
                               labels=c('Fundamental Incompleto/Cursando',
                                        'Fundamental Completo',
                                        'Médio Incompleto/Cursando',
                                        'Médio Completo',
                                        'Superior Incompleto/Cursando',
                                        'Superior Completo',
                                        'Pós-graduação Incompleto/Cursando',
                                        'Pós-graduação Completo')))

## vs arroz feijao -------------------------------------------------------------

tabyl(ibgfelps, escolaridade, arrozFeijao) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:4,
               names_to = 'arroz',
               values_to = 'prop') %>% 
  mutate(prop=round(prop*100,1),
         arroz=case_when(arroz == 'Baixo' ~ 1,
                         arroz == 'Lado' ~ 2,
                         arroz == 'Cima' ~ 3),
         arroz = factor(arroz,
                        levels=1:3,
                        labels=c('Baixo', 'Lado', 'Cima'))) %>%
  rename(`Arroz por Cima, por Baixo ou do Lado do Feijão?` = arroz) %>% 
  ggplot(aes(escolaridade, prop)) +
  geom_col(aes(fill=`Arroz por Cima, por Baixo ou do Lado do Feijão?`,
               group = `Arroz por Cima, por Baixo ou do Lado do Feijão?`),
           position='dodge') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999')) +
  labs(x='Escolaridade',
       y = 'Onde as pessoas colocam o arroz (em %)')

# REGIAO -----------------------------------------------------------------------

ibgfelps <- ibgfelps %>% 
  mutate(regiao = case_when(regiao == 'Norte' ~ 1,
                            regiao == 'Nordeste' ~ 2,
                            regiao == 'Centro-Oeste' ~ 3,
                            regiao == 'Sudeste' ~ 4,
                            regiao == 'Sul' ~ 5,
                            regiao == 'Fora do Brasil' ~ 6),
         regiao = factor(regiao,
                         levels = 1:6,
                         labels = c('Norte',
                                    'Nordeste',
                                    'Centro-Oeste',
                                    'Sudeste',
                                    'Sul',
                                    'Fora do Brasil')))

## vs microondas ---------------------------------------------------------------

tabyl(ibgfelps, regiao, microondas) %>% 
  adorn_percentages('row') %>%
  pivot_longer(cols=2:4,
               names_to='microondas',
               values_to='prop') %>% 
  mutate(prop=round(prop*100, 1),
         microondas = case_when(microondas == 'Sim' ~ 1,
                                microondas == 'Não' ~ 2,
                                microondas == 'Não tenho micro-ondas' ~ 3),
         microondas = factor(microondas,
                             levels=1:3,
                             labels=c('Sim',
                                      'Não',
                                      'Não tenho microondas'))) %>% 
  rename(`Microondas Mutado?`=microondas) %>% 
  ggplot(aes(regiao, prop)) +
  geom_col(aes(fill=`Microondas Mutado?`)) +
  theme_minimal() +
  labs(title = 'Porcentagem de microondas mutados por faixa etária',
       x='Região do País',
       y='Proporção de Microondas Mutados (em %)') +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999'))

## vs pedra papel tesoura ------------------------------------------------------

tabyl(ibgfelps, regiao, ppt) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols = 2:4,
               names_to='Pedra, Papel ou Tesoura?',
               values_to='prop') %>% 
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(regiao, prop)) + 
  geom_col(aes(fill=`Pedra, Papel ou Tesoura?`)) +
  geom_hline(yintercept = 66.6) +
  geom_hline(yintercept = 33.3) +
  theme_minimal() +  
  labs(x='Região do País',
       y='Pedra Papel ou Tesoura (em %)') +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999'))

## vs arroz feijao -------------------------------------------------------------
tabyl(ibgfelps, regiao, arrozFeijao) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:4,
               names_to = 'arroz',
               values_to = 'prop') %>% 
  mutate(prop=round(prop*100,1),
         arroz=case_when(arroz == 'Baixo' ~ 1,
                         arroz == 'Lado' ~ 2,
                         arroz == 'Cima' ~ 3),
         arroz = factor(arroz,
                        levels=1:3,
                        labels=c('Baixo', 'Lado', 'Cima'))) %>%
  rename(`Arroz por Cima, por Baixo ou do Lado do Feijão?` = arroz) %>% 
  ggplot(aes(regiao, prop)) +
  geom_col(aes(fill=`Arroz por Cima, por Baixo ou do Lado do Feijão?`,
               group = `Arroz por Cima, por Baixo ou do Lado do Feijão?`),
           position='dodge') +
  theme_minimal() +
  scale_y_continuous(limits=c(0,100)) +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999')) +
  labs(x='Região',
       y = 'Onde as pessoas colocam o arroz (em %)')
## vs coco ---------------------------------------------------------------------

tabyl(ibgfelps, regiao, coco) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=3:5,
               names_to='coco',
               values_to='prop') %>% 
  mutate(prop=round(prop*100,1)) %>% 
  rename(`O quanto você tira a roupa pra fazer cocô?` = coco) %>% 
  ggplot(aes(regiao, prop)) +
    geom_col(aes(fill=`O quanto você tira a roupa pra fazer cocô?`,
                 group=`O quanto você tira a roupa pra fazer cocô?`),
             position='dodge') +
    theme_minimal() +
    scale_y_continuous(limits=c(0,100)) +
    scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999')) +
    labs(x='Região',
         y = '%')

## vs hamburguer ---------------------------------------------------------------

### n de ingredientes

tabyl(ibgfelps, regiao, nHamburguer) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:16,
               names_to = 'ingredientes',
               values_to = 'prop') %>% 
  mutate(prop = round(prop*100,1),
         ingredientes = as.character(ingredientes),
         ingredientes = factor(ingredientes,
                               levels=1:15,
                               labels=as.character(1:15))) %>% 
  ggplot(aes(regiao, ingredientes)) +
  geom_tile(aes(fill=prop)) +
  geom_text(aes(label=prop)) +
  scale_fill_gradient(low ='white',
                      high = '#FE3867') +
  theme_minimal() +
  labs(x = 'Região', y = 'Quantos Ingredientes coloca no seu hamburguer')


### quais ingredientes
PaoCima <- tabyl(ibgfelps, regiao, hPaoCima) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pão de Cima')

Ketchup <- tabyl(ibgfelps, regiao, hKetchup) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Ketchup')

Mostarda <- tabyl(ibgfelps, regiao, hMostarda) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Mostarda')

Maionese <- tabyl(ibgfelps, regiao, hMaionese) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Maionese')

Pimenta <- tabyl(ibgfelps, regiao, hPimenta) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pimenta')

Queijo <- tabyl(ibgfelps, regiao, hQueijo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Queijo')

Hamburguer <- tabyl(ibgfelps, regiao, hHamburguer) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Hamburguer')

HamburguerVegano <- tabyl(ibgfelps, regiao, hHamburguerVegano) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Hamburguer Vegano')

Alface <- tabyl(ibgfelps, regiao, hAlface) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Alface')

Tomate <- tabyl(ibgfelps, regiao, hTomate) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Tomate')

Picles <- tabyl(ibgfelps, regiao, hPicles) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Picles')

Bacon <- tabyl(ibgfelps, regiao, hBacon) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Bacon')

PaoBaixo <- tabyl(ibgfelps, regiao, hPaoBaixo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Pão de Baixo')

Ovo <- tabyl(ibgfelps, regiao, hOvo) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Ovo')

Cebola <- tabyl(ibgfelps, regiao, hCebola) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols=2:3,
               names_to='pres',
               values_to = 'prop') %>% 
  mutate(ingrediente = 'Cebola')

rbind(PaoCima, Ketchup, Mostarda, Maionese, Pimenta, Queijo, Hamburguer,
      HamburguerVegano, Alface, Tomate, Picles, Cebola, Ovo, Bacon, PaoBaixo) %>% 
  mutate(prop=round(prop*100,1),
         pres=ifelse(pres==1, 'Sim', 'Não')) %>%
  rename(`Usou o Ingrediente?` = pres) %>% 
  ggplot(aes(prop, regiao)) +
  geom_col(aes(fill=`Usou o Ingrediente?`)) +
  facet_wrap(vars(ingrediente)) +
  scale_fill_manual(values = c('#FE3867','#ff5c83')) +
  labs(y = 'Região', x = 'Proporção de pessoas que usou cada ingrediente (em %)') +
  theme_minimal()


# LGBTQIA+ ---------------------------------------------------------------------

## vs pedra papel tesoura ------------------------------------------------------

tabyl(ibgfelps, lgbtqia, ppt) %>% 
  adorn_percentages('row') %>% 
  pivot_longer(cols = 2:4,
               names_to='Pedra, Papel ou Tesoura?',
               values_to='prop') %>% 
  mutate(prop = round(prop*100,1)) %>% 
  ggplot(aes(lgbtqia, prop)) + 
  geom_col(aes(fill=`Pedra, Papel ou Tesoura?`)) +
  geom_hline(yintercept = 66.6) +
  geom_hline(yintercept = 33.3) +
  theme_minimal() +  
  labs(title = 'Porcentagem de microondas mutados por faixa etária',
       x='LGBTQIA+ ?',
       y='Pedra papel ou tesoura?') +
  scale_fill_manual(values = c('#FE3867','#ff5c83','#fc7999'))