# Implementacja gry kolko i krzyzyk w jezyku R.

# Definicja listy trojka - zawierajaca wszystkie mozliwe kombinacje trzech pol, 
# ktore moga doprowadzic do wygrania 

trojka <- list(
  c(1,2,3),
  c(4,5,6),
  c(7,8,9),
  c(1,4,7),
  c(2,5,8),
  c(3,6,9),
  c(1,5,9),
  c(3,5,7)
)

# Wyœwietlanie planszy
wyswietl <- function(stan){
  cat(sprintf("\n %s | %s | %s \n ---+---+--- \n %s | %s | %s \n ---+---+--- \n %s | %s | %s \n", 
              stan[1], stan[2], stan[3], stan[4], stan[5], stan[6], stan[7], stan[8], stan[9]))
}

# Aktualizacja planszy
aktualizacja <- function(stan, kto, pozycja){
  nowy_stan <- replace(stan, pozycja, kto)
  return(nowy_stan)
}


