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

# Wy?wietlanie planszy
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
  # Sprawdzenie, czy komputer gra x czy o
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
      pozycja = setdiff(trojka[[i]], stan[trojka[[i]]][stan[trojka[[i]]] == komputer])[1]            # Sprawdzenie, czy komputer moze wygrac
      if (stan[pozycja] != "x" && stan[pozycja] != "o") {
        break
      }
    } else if (sum(stan[trojka[[i]]] == czlowiek) == 2 && sum(stan[trojka[[i]]] == komputer) == 0) {
      pozycja = setdiff(trojka[[i]], stan[trojka[[i]]][stan[trojka[[i]]] == czlowiek])[1]                  # Sprawdzenie, czy trzeba zablokowac przeciwnika
      if (stan[pozycja] != "x" && stan[pozycja] != "o") {
        break
      }
    } else {
      pozycja = sample(1:9, 1)
      while (stan[pozycja] == "x" || stan[pozycja] == "o") {
        pozycja = sample(1:9, 1)          # Wybor losowej pozycji
      }
      if (stan[pozycja] != "x" && stan[pozycja] != "o") {
        break
      }
    }
  }
  
  nowy_stan = aktualizacja(stan, komputer, pozycja)
  cat(komputer, "gra na pozycji", pozycja, "\n")
  return(nowy_stan)
}

# Rozpoczęcie gry
rozpocznij_gre <- function() {
  stan_poczatkowy = as.character(1:9)    # Wyswietlenie planszy
  liczba_graczy = as.integer(readline(prompt = "Ilu będzie graczy? 1 czy 2: "))
  
  if (liczba_graczy == 1) {
    kolejnosc = as.integer(readline(prompt = "Czy komputer ma grać jako pierwszy, czy jako drugi? 1 czy 2: "))
    symbol_czlowieka = ifelse(kolejnosc == 1, "o", "x")
    
    while (!zwyciezca(stan_poczatkowy)) {
      if (kolejnosc == 1) {
        stan_poczatkowy = tura_komputera(stan_poczatkowy)
        if (zwyciezca(stan_poczatkowy)) {
          cat("Komputer wygrywa!\n")
          break
        }
        kolejnosc = 2
      } else {
        wyswietl(stan_poczatkowy)
        pozycja = as.integer(readline(prompt = sprintf("Na której pozycji postawić '%s': ", symbol_czlowieka)))
        
        while (stan_poczatkowy[pozycja] == "x" || stan_poczatkowy[pozycja] == "o") {
          pozycja = as.integer(readline(prompt = "To pole jest już zajęte, wybierz inne: "))
        }
        
        stan_poczatkowy = aktualizacja(stan_poczatkowy, symbol_czlowieka, pozycja)
        if (zwyciezca(stan_poczatkowy)) {
          cat("Człowiek wygrywa! \n")
          break
        }
        kolejnosc = 1
      }
      if (sum(stan_poczatkowy == "x") + sum(stan_poczatkowy == "o") == 9 && !zwyciezca(stan_poczatkowy)) {
        cat("Gra zakończona remisem.\n")
        break
      }
    }
  } else if (liczba_graczy == 2) {
    while (!zwyciezca(stan_poczatkowy)) {
      for (gracz in c("x", "o")) {
        wyswietl(stan_poczatkowy)
        pozycja = as.integer(readline(prompt = sprintf("Na której pozycji postawić '%s': ", gracz)))
        while (stan_poczatkowy[pozycja] == "x" || stan_poczatkowy[pozycja] == "o") {
          pozycja = as.integer(readline(prompt = "To pole jest już zajęte, wybierz inne: "))
        }
        
        stan_poczatkowy = aktualizacja(stan_poczatkowy, gracz, pozycja)
        
        if (zwyciezca(stan_poczatkowy)) {
          cat(sprintf("Gracz '%s' wygrywa!\n", gracz))
          break
        }
        
        if (sum(stan_poczatkowy == "x") + sum(stan_poczatkowy == "o") == 9 && !zwyciezca(stan_poczatkowy)) {
          cat("Gra zakończona remisem.\n")
          break
        }
      }
      if (zwyciezca(stan_poczatkowy)) {
        break
      }
    }
  }
  wyswietl(stan_poczatkowy)
}

rozpocznij_gre()
                