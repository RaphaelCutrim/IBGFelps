# SETUP ------------------------------------------------------------------------

## carregando pacotes necessarios pra fazer a analise

if(!require('pacman')) install.packages('pacman')

p_load(tidyverse,
       rio,
       janitor)

# IMPORTANDO DADOS -------------------------------------------------------------

## importando
ibgfelps <- import('Raw/IBGFelps2026.csv')

## renomeando variaveis (pra ficar mais simples de ler)
ibgfelps <- ibgfelps %>% 
  rename(dataHora = Timestamp,
         microondas = `Seu micro-ondas é mutado?`,
         idade = `Qual sua Idade?`,
         ppt = `Pedra, Papel ou Tesoura?`,
         hamburguer = `Monte seu Hambúrguer:`,
         diaSemana = `Qual dia da semana preferido?`,
         escolaridade = `Qual seu Grau de Escolaridade?`,
         arrozFeijao = `Arroz vai por Cima, por Baixo ou ao Lado do feijão? (Se não come feijão, onde iria caso comesse?)`,
         pet = `Tem Animal de Estimação? Qual?`,
         carinha = `'-'`,
         regiao = `Qual região do Brasil você mora?`,
         coco = `Quando você vai ao banheiro, fazer um cocozinho, tira toda a roupa?`,
         simNao = `Sim ou Não?`,
         letra = `Em qual LETRA você moraria?`,
         lgbtqia = `Você é LGBTQIA+?`, #botar o + atrapalha o codigo
         idGenero = `Qual sua Identidade de Gênero?`,
         numero = `Em qual NÚMERO você moraria?`,
         sexualidade = `Qual sua sexualidade?`,
         trabalho = `O que você faz/trabalha?`,
         melhorPergunta = `Qual a melhor pergunta?`)


# OPERACIONALIZANDO VARIAVEIS --------------------------------------------------

## Data e Hora -----------------------------------------------------------------

## separando data e hora
dh <- str_split(ibgfelps$dataHora, " ") # separando em duas listas


for(i in 1:length(dh)){ # transformando duas listas em colunas
  if(i == 1){
    dataTemp <- NULL
    horaTemp <- NULL
  }
  dataTemp <- rbind(dataTemp, dh[[i]][[1]])
  horaTemp <- rbind(horaTemp, dh[[i]][[2]])
  if(i == length(dh)){
    ibgfelps <- ibgfelps %>% # colocando as duas colunas na planilha original
      mutate(data = dataTemp,
             hora = horaTemp)
    rm(dataTemp, horaTemp, dh) # removendo arquivos temporarios
  }
}

## Hamburguer ------------------------------------------------------------------

## contando quantos ingredientes cada um colocou no seu hamburguer
hamb <- tokenize(ibgfelps$hamburguer)

for(i in 1:length(hamb)){
  if(i == 1){
    nHamb <- NULL
  }
  nHamb <- rbind(nHamb, length(hamb[[i]]))
  if(i == length(hamb)){
    ibgfelps <- ibgfelps %>% 
      mutate(nHamburguer = nHamb)
    rm(hamb, nHamb)
  }
}


## separando os ingredientes do hamburguer de cada um
ibgfelps <- ibgfelps %>% 
  mutate(hPaoCima = grepl('Pão de Cima', ibgfelps$hamburguer, fixed = T),
         hKetchup = grepl('Ketchup', ibgfelps$hamburguer, fixed = T),
         hMostarda = grepl('Mostarda', ibgfelps$hamburguer, fixed = T),
         hMaionese = grepl('Maionese', ibgfelps$hamburguer, fixed = T),
         hPimenta = grepl('Pimenta', ibgfelps$hamburguer, fixed = T),
         hQueijo = grepl('Queijo', ibgfelps$hamburguer, fixed = T),
         hHamburguer = grepl('Hambúrguer', ibgfelps$hamburguer, fixed = T),
         hHamburguerVegano = grepl('Hambúrguer Vegano', ibgfelps$hamburguer, fixed = T),
         hAlface = grepl('Alface', ibgfelps$hamburguer, fixed = T),
         hTomate = grepl('Tomate', ibgfelps$hamburguer, fixed = T),
         hPicles = grepl('Picles', ibgfelps$hamburguer, fixed = T),
         hCebola = grepl('Cebola', ibgfelps$hamburguer, fixed = T),
         hOvo = grepl('Ovo', ibgfelps$hamburguer, fixed = T),
         hBacon = grepl('Bacon', ibgfelps$hamburguer, fixed = T),
         hPaoBaixo = grepl('Pão de Baixo', ibgfelps$hamburguer, fixed = T))


## Pets ------------------------------------------------------------------------

## contando quantas opcoes as pessoas marcaram na pergunta dos pets
pet <- tokenize(ibgfelps$pet)

for(i in 1:length(pet)){
  if(i == 1){
    petTemp <- NULL
  }
  petTemp <- rbind(petTemp, length(pet[[i]]))
  if(i == length(pet)){
    ibgfelps <- ibgfelps %>% 
      mutate(nPet = petTemp)
    rm(petTemp, pet)
  }
}


## separando os pets que cada um tem
ibgfelps <- ibgfelps %>% 
  mutate(pCachorro = grepl('Cachorro', ibgfelps$pet, fixed = T),
         pGato = grepl('Gato', ibgfelps$pet, fixed = T),
         pHamster = grepl('Hamster', ibgfelps$pet, fixed = T),
         pPapagaio = grepl('Papagaio', ibgfelps$pet, fixed = T),
         pTartaruga = grepl('Tartaruga / Jabuti', ibgfelps$pet, fixed = T),
         pCoelho = grepl('Coelho', ibgfelps$pet, fixed = T),
         pPassaro = grepl('Passaro', ibgfelps$pet, fixed = T),
         pPeixe = grepl('Peixe', ibgfelps$pet, fixed = T),
         pNaoTem = grepl('Não tenho', ibgfelps$pet, fixed = T),
         pOutro = grepl('Outro', ibgfelps$pet, fixed=T))


## Sexualidade -----------------------------------------------------------------

## contando quantas opcoes as pessoas marcaram na pergunta de sexualidade
sex <- tokenize(ibgfelps$sexualidade)

for(i in 1:length(sex)){
  if(i == 1){
    sexTemp <- NULL
  }
  sexTemp <- rbind(sexTemp, length(sex[[i]]))
  if(i == length(sex)){
    ibgfelps <- ibgfelps %>% 
      mutate(nSexualidade = sexTemp)
    rm(sexTemp, sex)
  }
}


## separando as sexualidades que cada um tem
ibgfelps <- ibgfelps %>% 
  mutate(sLesbica = grepl('Lésbica', ibgfelps$sexualidade, fixed = T),
         sGay = grepl('Gay', ibgfelps$sexualidade, fixed = T),
         sHetero = grepl('Hétero', ibgfelps$sexualidade, fixed = T),
         sAssexual = grepl('Assexual', ibgfelps$sexualidade, fixed = T),
         sArromantico = grepl('Arromântico', ibgfelps$sexualidade, fixed = T),
         sPanBi = grepl('Pan/Bi', ibgfelps$sexualidade, fixed = T),
         sOutro = grepl('Outro', ibgfelps$sexualidade, fixed = T),
         sNaoResp = grepl('Prefiro não responder', ibgfelps$sexualidade, fixed = T),)


## Trabalho --------------------------------------------------------------------

## contando quantas opcoes de trabalho as pessoas marcaram
trab <- tokenize(ibgfelps$trabalho)

for(i in 1:length(trab)){
  if(i == 1){
    trabTemp <- NULL
  }
  trabTemp <- rbind(trabTemp, length(trab[[i]]))
  if(i == length(trab)){
    ibgfelps <- ibgfelps %>% 
      mutate(nTrabalho = trabTemp)
    rm(trabTemp, trab)
  }
}


## separando as repostas da pergunta sobre trabalho
ibgfelps <- ibgfelps %>% 
  mutate(tAdmFin = grepl('Administrativo/Financeiro', ibgfelps$trabalho, fixed = T),
         tComMkt = grepl('Comunicação/Marketing', ibgfelps$trabalho, fixed = T),
         tTechTi = grepl('Tecnologia/TI', ibgfelps$trabalho, fixed = T),
         tSaudeBio = grepl('Saúde/Biológicas', ibgfelps$trabalho, fixed = T),
         tEngTec = grepl('Engenharia/Técnica', ibgfelps$trabalho, fixed = T),
         tEducEns = grepl('Educação/Ensino', ibgfelps$trabalho, fixed = T),
         tArt = grepl('Artista', ibgfelps$trabalho, fixed = T),
         tEst = grepl('Estudante / Em busca de recolocação', ibgfelps$trabalho, fixed = T),
         tOutro = grepl('Outro', ibgfelps$trabalho, fixed = T))

# FORMATANDO VARIAVEIS ---------------------------------------------------------

## descartando variaveis desnecessarias e transformando variaveis logicas em
## binarias

## (esse vai ser grande)
ibgfelps <- ibgfelps %>% 
  select(!c(sexualidade, pet, hamburguer, dataHora, trabalho)) %>%
  mutate(numero = as.character(numero),
         hPaoCima = ifelse(hPaoCima, 1, 0),
         hKetchup = ifelse(hKetchup, 1, 0),
         hMostarda = ifelse(hMostarda, 1, 0),
         hMaionese = ifelse(hMaionese, 1, 0),
         hPimenta = ifelse(hPimenta, 1, 0),
         hQueijo = ifelse(hQueijo, 1, 0),
         hHamburguer = ifelse(hHamburguer, 1, 0),
         hHamburguerVegano = ifelse(hHamburguerVegano, 1, 0),
         hAlface = ifelse(hAlface, 1, 0),
         hTomate = ifelse(hTomate, 1, 0),
         hPicles = ifelse(hPicles, 1, 0),
         hCebola = ifelse(hCebola, 1, 0),
         hOvo = ifelse(hOvo, 1, 0),
         hBacon = ifelse(hBacon, 1, 0),
         hPaoBaixo = ifelse(hPaoBaixo, 1, 0),
         pCachorro = ifelse(pCachorro, 1, 0),
         pGato = ifelse(pGato, 1, 0),
         pHamster = ifelse(pHamster, 1, 0),
         pPapagaio = ifelse(pPapagaio, 1, 0),
         pTartaruga = ifelse(pTartaruga, 1, 0),
         pCoelho = ifelse(pCoelho, 1, 0),
         pPassaro = ifelse(pPassaro, 1, 0),
         pPeixe = ifelse(pPeixe, 1, 0),
         pNaoTem = ifelse(pNaoTem, 1, 0),
         pOutro = ifelse(pOutro, 1, 0),
         sLesbica = ifelse(sLesbica, 1, 0),
         sGay = ifelse(sGay, 1, 0),
         sHetero = ifelse(sHetero, 1, 0),
         sAssexual = ifelse(sAssexual, 1, 0),
         sArromantico = ifelse(sArromantico, 1, 0),
         sPanBi = ifelse(sPanBi, 1, 0),
         sOutro = ifelse(sOutro, 1, 0),
         sNaoResp = ifelse(sNaoResp, 1, 0),
         tAdmFin = ifelse(tAdmFin, 1, 0),
         tComMkt = ifelse(tComMkt, 1, 0),
         tTechTi = ifelse(tTechTi, 1, 0),
         tSaudeBio = ifelse(tSaudeBio, 1, 0),
         tEngTec = ifelse(tEngTec, 1, 0),
         tEducEns = ifelse(tEducEns, 1, 0),
         tArt = ifelse(tArt, 1, 0),
         tEst = ifelse(tEst, 1, 0),
         tOutro = ifelse(tOutro, 1, 0))

## simplificando a hora e a data
ibgfelps <- ibgfelps %>%
  mutate(hora = gsub(':', '', hora),
         hora = substr(hora, 1, 2),
         data = gsub('/','', data),
         dia = substr(data, 1, 2)) %>%
  select(!data)

# EXPORTANDO -------------------------------------------------------------------

## exportando dados prontos pra analise
export(ibgfelps, 'Data/IBGFelps2026_tratado.csv')
