
# JP: 
# Hei Vinjar!
# Veldig gode løsninger! Jeg har skrevet noen tips og kommentarer til deg under, 
# markert med "JP:". Håper kommentarene er nyttige :-)


# hjemmelekse 2

x <- c(10,20,30)

trekk_fra_tre <- function(x) {
  x-3
}

trekk_fra_tre(x)


#2.1 Summere to tall

x <- c(1,2,3,10)
y <- c(4,5,6,23)

sum_to <- function(x,y) {
  x+y
}

#2.2 Multipliserer to tall

mult_to <- function(x,y){
  x*y
  
}

mult_to(x,y)

#2.3 Kvadrerer


# Feilmelding "Error in kvadrer_x() : argument "x" is missing, with no default"
# Denne kommer for en prøver å kvadrere "ingenting", uten at en har gitt R beksjed om hvordan et slikt tilfelle skal behandles. 


kvadrer_x <- function(x=NA) {
  if(is.na(x)) {
    stop("Du har ikke angitt hva som skal kvadreres, prøv å legge inn et tall")
  }
  x^2
}

# JP: 
# Riktig :-)

kvadrer_x(3)

#2.4 Default verdien er "pearson". Denne angir at en skal bruke Pearson correlation cofficient for beregning, dersom et av de to andre valgene ikke er spesifisert. 

# JP: 
# Korrekt!

#2.5 Finnes to i vektoren? 

finnes_to <- function(var) {
  any({{var}} ==2)
  
}

finnes_to(x)
finnes_to(y)

# JP: 
# Veldig god bruk av "any()"! 
# 
# Tips: Funksjonen din funker veldig bra, men det er strengt tatt ikke nødvendig
# med "{{}}" rundt "var" når man ikke bruk den i tidyverse-funksjoner (slik som 
# dplyr eller ggplot). 

#2.6 Disse funksjonene finnes allerede i R, dersom en lager nye funksjoner med disse navnene overskriver man de innebygde funksjonene. 

# JP: 
# Korrekt!

# 2.7 er partall
er_partall <-  function(x) {
  x/2 == round(x/2)
  
}

er_partall(100)

# JP: 
# Dette synes jeg var en god og kul løsning - Kreativt! Se gjerne løsningsforslaget 
# for et alternativ :-)

#"FALSE" i er_partall vil alltid være oddetall, kan derfor bruke er_partall også til å stadfeste at er tall er oddetall "

# JP: 
# Korrekt! Og ved hjelp av "!" (leses: "not") kan man snu er_partall til !er_partall
# (lese "not er partall"), som vil gi TRUE for oddetall, dersom man trenger det
# i filtrering eller en if-statement. 

# 2.8 if vs if_else
# ved bruk av if(), vil det man ønsker å sammenligne/teste gjennomført dersom det er sant (TRUE), Dersom FALSE, vil ingenting skje. 
# Ved bruk av ifelse vil en også kunne evaluere/returnere et svar dersom utfallet blir "FALSE" 
# Ville brukt ifelse i en funksjon. 

# JP: 
# Det du sier er riktig, men en viktigere forskjell er at ifelse er vektorisert,
# mens if kun evaluerer ett uttrykk. 
# 
# Begge har sin plass i en funksjon. Jeg bruker if() og else mye til control flow,
# som for å bestemme valg i en funksjon. Ifelse bruker jeg mye til å transformere
# variabler siden den er vektorisert. 


#2.9 uten de dobble parantesene, {{}}, vil ikke R skjønne at det er variabelen inne i funksjonen vi ønsker å finne, men  begynne å lete etter en variabel globalt. 

library(tidyverse)

## Funksjon som virker
lag_histogram <-  function(.data, variabel) {
  .data %>% 
    ggplot(aes(x = {{variabel}}))+
    geom_histogram(bins=10, fill="brown")
}

lag_histogram(mtcars, hp)

purrr:: map(.x=mtcars, .f= ~lag_histogram(.data = mtcars, variabel=.))

# JP: 
# Nydelig!

# 3. God. 

library(lubridate)

## se denne
lubridate::hour(now())

greeting <- function(x= now(), output_som_melding = FALSE) {
  
  if(x %>%
     str_split(" ") %>%
     map_chr(2) < 09:00:00) { 
    return(paste0("God Morgen"))
  }
  
  if(x %>%
     str_split(" ") %>%
     map_chr(2) < 12:00:00) { 
    return(paste0("God formiddag"))
  }
  
  if(x %>%
     str_split(" ") %>%
     map_chr(2) < 15:00:00) { 
    return(paste0("God ettermiddag"))
  }  
  
  if(x %>%
     str_split(" ") %>%
     map_chr(2) < 18:00:00) { 
    return(paste0("God kveld"))
  }  
  
  if(x %>%
     str_split(" ") %>%
     map_chr(2) > 00:00:00) { 
    return(paste0("God natt"))
  }
  
}

greeting()

# JP: 
# Denne funker, men du får en del warnings. De har med å gjøre at
# R tolker 09:00:00 som en vektor fra 9 til 0. Siden du bruker IF så sjekker den
# kun ett element, så den sjekker om x %>% str_split(" ") %>% map_chr(2) er større
# enn 09, resten ignoreres.


###############################################################################




############## 4. #konverteringer##################


#Farenheit konvertering
farenheit_to__celcius <- function(x) {
  round((x-32)*5/9)
}

farenheit_to__celcius(64)

#celsius konvertering
celsius_to__farenheit <- function(x) {
  round((x*9/5)+32)
}

celsius_to__farenheit(23)

#funksjon i funksjon
farenheit_to__celcius(celsius_to__farenheit(23))

# JP: 
# Bra!


######### 5 DingDong #####################

dingdong <- function(x){
  
  if(x /5 == round(x/5) & x /3 == round(x/3)) { 
    return(paste0("DingDong"))
  }
  
  if(x /3 == round(x/3)) { 
    return(paste0("Ding"))
  }
  
  if(x /5 == round(x/5)) { 
    return(paste0("DingDong"))
  }
  
  x
}

dingdong(15)

# JP: 
# Fin løsning. Se løsningsforslaget for en vektorisert versjon også (en som kan ta
# flere input enn kun 15. F.eks. c(1:15)).




