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

# Sprawdzenie zwyciezcy
zwyciezca <- function(stan) {
  any(sapply(trojka, function(t) all(stan[t] == "x")) | 
        sapply(trojka, function(t) all(stan[t] == "o")))
}

# Tura komputera
tura_komputera <- function(stan) {
  if (sum(stan == "x") == sum(stan == "o")) {
    komputer = "x"
    czlowiek = "o"
  } else {
    komputer = "o"
    czlowiek = "x"
  }
  
  pozycja = sample(1:9, 1)
  
  for (i in 1:8) {
    if (sum(stan[trojka[[i]]] == komputer) == 2 && sum(stan[trojka[[i]]] == czlowiek) == 0) {
      pozycja = setdiff(trojka[[i]], stan[trojka[[i]]][stan[trojka[[i]]] == komputer])
      break
    } else if (sum(stan[trojka[[i]]] == czlowiek) == 2 && sum(stan[trojka[[i]]] == komputer) == 0) {
      pozycja = setdiff(trojka[[i]], stan[trojka[[i]]][stan[trojka[[i]]] == czlowiek])
    } else {
      while (stan[pozycja] == "x" || stan[pozycja] == "o") {
        pozycja = sample(1:9, 1)
      }
    }
  }
  
  nowy_stan = aktualizacja(stan, komputer, pozycja)
  cat(komputer, "gra na pozycji", pozycja, "\n")
  return(nowy_stan)
}

