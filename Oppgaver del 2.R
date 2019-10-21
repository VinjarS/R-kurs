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

kvadrer_x(3)

#2.4 Default verdien er "pearson". Denne angir at en skal bruke Pearson correlation cofficient for beregning, dersom et av de to andre valgene ikke er spesifisert. 



#2.5 Finnes to i vektoren? 

finnes_to <- function(var) {
  any({{var}} ==2)
  
}

finnes_to(x)
finnes_to(y)


#2.6 Disse funksjonene finnes allerede i R, dersom en lager nye funksjoner med disse navnene overskriver man de innebygde funksjonene. 


# 2.7 er partall
er_partall <-  function(x) {
  x/2 == round(x/2)
  
}

er_partall(100)

#"FALSE" i er_partall vil alltid være oddetall, kan derfor bruke er_partall også til å stadfeste at er tall er oddetall "

# 2.8 if vs if_else
# ved bruk av if(), vil det man ønsker å sammenligne/teste gjennomført dersom det er sant (TRUE), Dersom FALSE, vil ingenting skje. 
# Ved bruk av ifelse vil en også kunne evaluere/returnere et svar dersom utfallet blir "FALSE" 
# Ville brukt ifelse i en funksjon. 


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




