#La classe Graph contiene la matrice di adiacenza, le liste dei vertici e degli archi, e i metodi richiesti
Graph <- setRefClass(
  Class = "Graph",
  fields = list(
    #I vertici e gli archi sono liste di environment, all'interno dei quali sono salvati dei dati aggiuntivi
    matriceadiacenza = "matrix",
    Vertici = "list",
    Archi = "list"),
  methods = list(
    #PUNTO 1a.1, verifica se c'è un arco dal vertice x al vertice y
    VerificaArco = function(x,y) {
      #Controlla che i vertici siano presenti, altrimenti restituisce un messaggio apposito
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(!(y %in% colnames(matriceadiacenza))) return(cat("Il vertice",y,"non esiste"))
      #Se il valore sulla matrice è 0, l'arco non esiste, altrimenti viene restituito
      if(matriceadiacenza[x,y] == 0) return(cat("L'arco tra il vertice",x,"e il vertice",y,"non esiste"))
      return(cat("L'arco tra il vertice",x,"e il vertice",y,"esiste ed ha valore",matriceadiacenza[x,y]))
    },
    #PUNTO 1a.2, elenca tutti i vertici y tali che vi sia un arco dal vertice x al vertice y
    ElencaVertici = function(x) {
      #Controlla che il vertice sia presente prima di andarne a cercare altri adiacenti
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      #Crea una lista vuota di vertici, per poi aggiornarla ogni qual volta ne trova uno adiacente
      elencoadiacenti <- c()
      for(colonna in colnames(matriceadiacenza)) {
        if(matriceadiacenza[x,colonna] != 0) elencoadiacenti <- append(elencoadiacenti, colonna)
      }
      #Se la lista rimane vuota, significa che non c'è nessun vertice adiacente a quello dato
      if(is.null(elencoadiacenti)) return(cat("Il vertice",x,"non ha vertici adiacenti"))
      return(elencoadiacenti)
    },
    #PUNTO 1a.3, aggiunge il vertice x, se non c'è
    AggiungiVertice = function(x,v) {
      #Controlla che il vertice non esista già prima di crearlo, e che il valore sia positivo o al limite nullo
      if(x %in% rownames(matriceadiacenza)) return(cat("Il vertice",x,"esiste già"))
      if(v < 0) return(cat("Il valore di un vertice non può essere negativo"))
      #Crea un nuovo environment dove conserva nome e valore del nuovo vertice
      vertice <- new.env()
      vertice$nome <- x
      vertice$valore <- v
      Vertici <<- append(Vertici, vertice)
      #Crea due matrici temporanee, una di una sola riga ed una di una sola colonna,
      #e le unisce a quella originale in modo da aggiungere il nuovo vertice
      nuovacolonna <- matrix(0, nrow=nrow(matriceadiacenza), ncol=1)
      colnames(nuovacolonna) <- x
      matriceadiacenza <<- cbind(matriceadiacenza, nuovacolonna)
      nuovariga <- matrix(0, nrow=1, ncol=ncol(matriceadiacenza))
      rownames(nuovariga) <- x
      matriceadiacenza <<- rbind(matriceadiacenza, nuovariga)
    },
    #PUNTO 1a.4, rimuove il vertice x, se c'è
    RimuoviVertice = function(x) {
      #Controlla che il vertice sia presente prima di cercare di rimuoverlo
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      #Controlla, prima di rimuovere il vertice, che non ci siano più archi collegati, ed eventualmente elimina quelli residui
      for(riga in rownames(matriceadiacenza)) {
        if(matriceadiacenza[riga,x] != 0) RimuoviArco(riga,x)
      }
      for(colonna in colnames(matriceadiacenza)) {
        if(matriceadiacenza[x,colonna] != 0) RimuoviArco(x,colonna)
      }
      #Rimuove l'environment dalla lista
      Vertici <<- Vertici[-(which(rownames(matriceadiacenza) == x))]
      #Aggiorna la matrice di adiacenza
      matriceadiacenza <<- matriceadiacenza[-(which(rownames(matriceadiacenza) == x)), -(which(colnames(matriceadiacenza) == x))]
    },
    #PUNTO 1a.5, aggiunge l'arco dal vertice x al vertice y, se non c'è
    AggiungiArco = function(x,y,v) {
      #Controlla che entrambi i vertici siano presenti, che l'arco non esista già e che il suo valore sia positivo non nullo
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(!(y %in% colnames(matriceadiacenza))) return(cat("Il vertice",y,"non esiste"))
      if(matriceadiacenza[x,y] != 0) return(cat("L'arco (",x,", ",y,") esiste già", sep=""))
      if(v < 1) return(cat("Il valore di un arco non può essere negativo o nullo"))
      #Crea un nuovo environment dove conserva sorgente, destinazione e valore del nuovo arco
      arco <- new.env()
      arco$sorgente <- x
      arco$destinazione <- y
      arco$valore <- v
      Archi <<- append(Archi, arco)
      #Modifica la matrice di adiacenza inserendo il nuovo valore dell'arco creato
      matriceadiacenza[x,y] <<- v
    },
    #PUNTO 1a.6, rimuove l'arco dal vertice x al vertice y, se c'è
    RimuoviArco = function(x,y) {
      #Controlla che entrambi i vertici siano presenti, e che l'arco esista
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(!(y %in% colnames(matriceadiacenza))) return(cat("Il vertice",y,"non esiste"))
      if(matriceadiacenza[x,y] == 0) return(cat("L'arco (",x,", ",y,") non esiste", sep=""))
      #Utilizza un contatore per "ricordare" la posizione dell'environment da eliminare e poi lo rimuove uscito dal ciclo
      cont <- 0
      for(arco in Archi) {
        cont <- cont + 1
        if(arco$sorgente == x && arco$destinazione == y) break
      }
      Archi <<- Archi[-cont]
      #Aggiorna la matrice di adiacenza
      matriceadiacenza[x,y] <<- 0
    },
    #PUNTO 1a.7, restituisce il valore associato al vertice x
    ValoreVertice = function(x) {
      #Controlla che il vertice sia presente
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      #Utilizza il ciclo for per cercare l'environment corretto e poi restituisce il campo valore
      for(vertice in Vertici) {
        if(vertice$nome == x) return(vertice$valore)
      }
    },
    #PUNTO 1a.8, imposta il valore assegnato al vertice x a v
    AssegnaValoreVertice = function(x,v) {
      #Controlla che il vertice sia presente, e che il valore da assegnare sia positivo o al limite nullo
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(v < 0) return(cat("Il valore di un vertice non può essere negativo"))
      #Utilizza il ciclo for per cercare l'environment corretto e poi va a modificare il campo valore
      for(vertice in Vertici) {
        if(vertice$nome == x) {
          vertice$valore <- v
          break
        }
      }
    },
    #PUNTO 1a.9, restituisce il valore associato all'arco (x, y)
    ValoreArco = function(x,y) {
      #Controlla che entrambi i vertici siano presenti, e che l'arco esista
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(!(y %in% colnames(matriceadiacenza))) return(cat("Il vertice",y,"non esiste"))
      if(matriceadiacenza[x,y] == 0) return(cat("L'arco (",x,", ",y,") non esiste", sep=""))
      #Utilizza la matrice di adiacenza per restituire il valore dell'arco
      return(matriceadiacenza[x,y])
    },
    #PUNTO 1a.10, imposta il valore assegnato all'arco (x,y) a v
    AssegnaValoreArco = function(x,y,v) {
      #Controlla che entrambi i vertici siano presenti, che l'arco esista, e che il valore da assegnare sia positivo non nullo
      if(!(x %in% rownames(matriceadiacenza))) return(cat("Il vertice",x,"non esiste"))
      if(!(y %in% colnames(matriceadiacenza))) return(cat("Il vertice",y,"non esiste"))
      if(matriceadiacenza[x,y] == 0) return(cat("L'arco (",x,", ",y,") non esiste", sep=""))
      if(v < 1) return(cat("Il valore di un arco non può essere negativo o nullo"))
      #Utilizza il ciclo for per cercare l'environment corretto e poi va a modificare il campo valore
      for(arco in Archi) {
        if(arco$sorgente == x && arco$destinazione == y) {
          arco$valore <- v
          break
        }
      }
      #Aggiorna la matrice di adiacenza
      matriceadiacenza[x,y] <<- v
    },
    #PUNTO 1b, rappresenta il grafo con una lista di adiacenza
    CreaListaAdiacenza = function() {
      #Crea la lista di adiacenza vuota, nonché una variabile che tiene conto se la matrice è vuota o meno
      listaadiacenza <- list()
      vuota <- TRUE
      for(riga in rownames(matriceadiacenza)) {
        #Se su una riga della matrice non ci sono elementi diversi da 0, il vertice non è mai sorgente di un arco,
        #e sulla lista di adiacenza viene stampato "" a simboleggiare che non ci sono archi adiacenti
        if(length(which(matriceadiacenza[riga,] != 0)) == 0) listaadiacenza <- append(listaadiacenza, "")
        #Se invece ci sono uno o più elementi diversi da 0, vengono messi in lista sotto forma di vettore, con loro nome
        else {
          listaadiacenza <- append(listaadiacenza, list(names(which(matriceadiacenza[riga,] != 0))))
          vuota <- FALSE
        }
        names(listaadiacenza)[length(listaadiacenza)] <- riga
      }
      #Se la lista di adiacenza è vuota viene restituito un messaggio apposito, altrimenti viene stampata
      if(vuota) return(cat("La lista di adiacenza è vuota"))
      return(listaadiacenza)
    },
    #PUNTO 1c, rappresenta il grafo con una edge list
    CreaEdgeList = function() {
      #Crea la edgelist vuota, poi la riempie con gli archi che si trovano nella lista di environment
      edgelist <- list()
      for(arco in Archi) edgelist <- append(edgelist, list(c(arco$sorgente, arco$destinazione)))
      #Se la edgelist è vuota viene restituito un messaggio apposito, altrimenti viene stampata
      if(length(edgelist) == 0) return(cat("La edge list è vuota"))
      return(edgelist)
    },
    #PUNTO 2, visualizza il grafo inserito
    VisualizzaGrafo = function(matrice, vertici, archi) {
      #Controlla se la matrice di adiacenza è vuota ed eventualmente restituisce un messaggio apposito
      if(length(matrice) == 0) return(cat("La matrice di adiacenza è vuota"))
      #Installa, se necessario, la libreria "networkD3", utilizzata per visualizzare il grafo inserito, e la carica
      if(!("networkD3" %in% rownames(installed.packages()))) install.packages("networkD3")
      library(networkD3)
      #Crea una matrice dei vertici, dove le colonne rappresentano nome, valore e gruppo del vertice stesso:
      #la colonna "Nome" contiene il nome che apparirà vicino al vertice,
      #la colonna "Valore" influisce lievemente sulla dimensione del vertice,
      #la colonna "Gruppo" è necessaria alla libreria, ma i vertici vengono inseriti tutti nello stesso gruppo di nome "1"
      cont <- 0
      matricevertici <- matrix(0, nrow=length(Vertici), ncol=3)
      for(vertice in vertici) {
        cont <- cont + 1
        matricevertici[cont,] <- c(vertice$nome, vertice$valore, 1)
      }
      #La matrice dei vertici deve essere convertita in dataframe per poter funzionare,
      #è inoltre necessario numerare progressivamente le righe ed assegnare alle colonne i nomi dei campi sopracitati
      dataframevertici <- data.frame(matricevertici)
      righe <- c(1:nrow(matricevertici))
      colonne <- c("Nome", "Valore", "Gruppo")
      dimnames(dataframevertici) <- list(righe, colonne)
      #Crea una matrice degli archi, dove le colonne rappresentano sorgente, destinazione e valore dell'arco stesso:
      #la colonna "Sorgente" contiene il numero della riga dove è scritto il nome della sorgente nel dataframe dei vertici,
      #la colonna "Destinazione" contiene il numero della riga dove è scritto il nome della destinazione nel dataframe dei vertici,
      #la colonna "Valore" influisce eccessivamente sulla dimensione della freccia, perciò si effettua la radice quadrata.
      #Ai valori delle colonne "Sorgente" e "Destinazione" viene sottratto 1 poiché networkD3 indicizza righe e colonne partendo da 0
      cont <- 0
      #Se il grafo non ha archi, la matrice degli archi ha una sola riga con tutti zeri
      if(length(archi) == 0) matricearchi <- matrix(0, nrow=1, ncol=3)
      else matricearchi <- matrix(0, nrow=length(archi), ncol=3)
      for(arco in archi) {
        cont <- cont + 1
        matricearchi[cont,1] <- which(rownames(matrice) == arco$sorgente) - 1
        matricearchi[cont,2] <- which(colnames(matrice) == arco$destinazione) - 1
        matricearchi[cont,3] <- sqrt(matrice[arco$sorgente, arco$destinazione])
      }
      #La matrice degli archi deve essere convertita in dataframe per poter funzionare,
      #è inoltre necessario numerare progressivamente le righe ed assegnare alle colonne i nomi dei campi sopracitati
      dataframearchi <- data.frame(matricearchi)
      #Numera correttamente il dataframe nel caso il grafo non abbia archi
      if(length(archi) == 0) righe <- c(1)
      else righe <- c(1:nrow(matricearchi))
      colonne <- c("Sorgente", "Destinazione", "Valore")
      dimnames(dataframearchi) <- list(righe, colonne)
      #La funzione forceNetwork prende in input i due dataframe e li usa per stampare il plot del grafo:
      #se la matrice è simmetrica il grafo viene stampato come non orientato, altrimenti come orientato.
      #I warning della funzione sono disattivati perché, nel caso il primo vertice della matrice sia isolato,
      #sarebbe stampato un avviso di un possibile errore di indicizzazione, mentre invece non ci sono problemi
      if(isSymmetric.matrix(matrice)) {
        suppressWarnings(forceNetwork(Nodes = dataframevertici, NodeID = "Nome", Nodesize = "Valore", Group = "Gruppo",
                     Links = dataframearchi, Source = "Sorgente", Target = "Destinazione", Value = "Valore",
                     fontSize = 14, opacityNoHover = 1))
      }
      else {
        suppressWarnings(forceNetwork(Nodes = dataframevertici, NodeID = "Nome", Nodesize = "Valore", Group = "Gruppo",
                     Links = dataframearchi, Source = "Sorgente", Target = "Destinazione", Value = "Valore",
                     arrows = "directed", fontSize = 14, opacityNoHover = 1))
      }
    },
    #PUNTO 3, esegue la visita in ampiezza, stampando il plot del grafo ottenuto
    VisitaInAmpiezza = function(sorgente) {
      #Controlla che il grafo sia orientato, che abbia almeno un arco, e che il vertice sorgente sia presente,
      #altrimenti restituisce dei messaggi appositi
      if(!(sorgente %in% rownames(matriceadiacenza))) return(cat("Il vertice sorgente non esiste"))
      if(length(which(matriceadiacenza != 0)) == 0) return(cat("Il grafo non ha archi"))
      if(isSymmetric.matrix(matriceadiacenza)) return(cat("Questo metodo non supporta i grafi non orientati"))
      #Crea una copia temporanea della matrice di adiacenza e delle liste dei vertici e degli archi,
      #in modo da poter creare il nuovo grafo utilizzando gli stessi metodi del precedente
      matricetemp <- matriceadiacenza
      verticitemp <- Vertici
      architemp <- Archi
      #La matrice di adiacenza e le liste dei vertici e degli archi vengono inizializzate
      matriceadiacenza <<- matrix(0, nrow=0, ncol=0)
      Vertici <<- list()
      Archi <<- list()
      #Crea la lista utilizzata per tenere traccia dei vertici da visitare,
      #inizializza la distanza a 0 ed aggiunge il primo vertice alla matrice
      distanza <- 0
      AggiungiVertice(sorgente, distanza)
      davisitare <- list(sorgente)
      #Il ciclo si interrompe quando la lista dei vertici da visitare è vuota, cioè quando la visita è terminata
      while(length(davisitare) != 0) {
        distanza <- distanza + 1
        #I vertici vengono aggiunti alla lista quando vengono scoperti, e rimossi quando vengono visitati:
        #i vertici visitati "contemporaneamente" hanno la stessa distanza dalla sorgente
        for(vertice in 1:length(davisitare)) {
          #Trova gli archi che hanno come sorgente il vertice che si sta visitando, la aggiunge,
          #poi aggiunge il vertice destinazione alla lista dei vertici da aggiungere
          for(arco in architemp) {
            if(arco$sorgente == davisitare[1] && !(arco$destinazione %in% rownames(matriceadiacenza))) {
              AggiungiVertice(arco$destinazione, distanza)
              AggiungiArco(arco$sorgente, arco$destinazione, 1)
              davisitare <- append(davisitare, arco$destinazione)
            }
          }
          #Dalla lista di aggiunta viene rimosso il vertice appena aggiunto
          davisitare <- davisitare[-1]
        }
      }
      #Cerca eventuali vertici isolati e gli dà come distanza indicativa 99, a simboleggiare che il vertice è irraggiungibile
      for(vertice in verticitemp) {
        if(!(vertice$nome %in% rownames(matriceadiacenza))) AggiungiVertice(vertice$nome, 99)
      }
      #Se invece il vertice isolato è il sorgente, il grafo viene stampato comunque, ma con un avviso
      if(length(Archi) == 0) cat("Attenzione, il vertice",sorgente,"è un vertice isolato")
      #La matrice e le due liste create vengono spostate su variabili apposite per la stampa del grafo,
      #mentre le variabili originarie vengono ripristinate per eventuali modifiche successive
      matricestampa <- matriceadiacenza
      matriceadiacenza <<- matricetemp
      verticistampa <- Vertici
      Vertici <<- verticitemp
      archistampa <- Archi
      Archi <<- architemp
      #Stampa il grafo ottenuto dalla visita in profondità
      VisualizzaGrafo(matricestampa, verticistampa, archistampa)
    },
    #PUNTO 4, esegue l'algoritmo di Dijkstra, stampando il plot del grafo ottenuto
    Dijkstra = function(sorgente) {
      #Controlla che il grafo sia orientato, che abbia almeno un arco, e che il vertice sorgente sia presente,
      #altrimenti restituisce dei messaggi appositi
      if(!(sorgente %in% rownames(matriceadiacenza))) return(cat("Il vertice sorgente non esiste"))
      if(length(which(matriceadiacenza != 0)) == 0) return(cat("Il grafo non ha archi"))
      if(isSymmetric.matrix(matriceadiacenza)) return(cat("Questo metodo non supporta i grafi non orientati"))
      #Crea una copia temporanea della matrice di adiacenza e delle liste dei vertici e degli archi,
      #in modo da poter creare il nuovo grafo utilizzando gli stessi metodi del precedente
      matricetemp <- matriceadiacenza
      verticitemp <- Vertici
      architemp <- Archi
      #La matrice di adiacenza e le liste dei vertici e degli archi vengono inizializzate
      matriceadiacenza <<- matrix(0, nrow=0, ncol=0)
      Vertici <<- list()
      Archi <<- list()
      #Crea la lista utilizzata per tenere traccia dei vertici aggiunti, ed aggiunge il primo vertice alla matrice
      AggiungiVertice(sorgente, 0)
      aggiunti <- list(sorgente)
      #Il ciclo si interrompe quando la lista dei vertici aggiunti ha la stessa lunghezza di quella con tutti i vertici
      while(length(aggiunti) < length(verticitemp)) {
        #Ad ogni ciclo viene memorizzato l'arco minimo che sarà poi aggiunto alla matrice di adiacenza
        arcominimo <- list(NULL, NULL, 0, 0)
        #Trova gli archi che hanno come sorgente uno dei vertici già visitati, ma come destinazione uno non ancora raggiunto,
        #poi, tramite il rilassamento degli archi, aggiunge i vertici alla matrice o ne aggiorna i valori
        for(arco in architemp) {
          if(arco$sorgente %in% aggiunti && !(arco$destinazione %in% aggiunti)) {
            somma <- ValoreVertice(arco$sorgente) + arco$valore
            if(!(arco$destinazione %in% rownames(matriceadiacenza))) AggiungiVertice(arco$destinazione, somma)
            else if(ValoreVertice(arco$destinazione) > somma) AssegnaValoreVertice(arco$destinazione, somma)
            if(arcominimo[4] == 0 || somma < arcominimo[4]) arcominimo <- list(arco$sorgente, arco$destinazione, arco$valore, somma)
          }
        }
        #Se a questo punto il valore dell'arco minimo è ancora 0, i vertici che restano da aggiungere sono isolati,
        #quindi vengono creati comunque ma con valore indicativo 99, a simboleggiare che sono irraggiungibili
        if(arcominimo[4] == 0) {
          for(vertice in verticitemp) {
            if(!(vertice$nome %in% aggiunti)) AggiungiVertice(vertice$nome, 99)
          }
          break
        }
        #Se invece l'arco minimo esiste, viene aggiunto alla matrice, mentre il vertice destinazione viene inserito tra gli aggiunti
        AggiungiArco(arcominimo[[1]], arcominimo[[2]], arcominimo[[3]])
        aggiunti <- append(aggiunti, arcominimo[[2]])
      }
      #Se infine il vertice isolato è il sorgente, il grafo viene stampato comunque, ma con un avviso
      if(length(Archi) == 0) cat("Attenzione, il vertice",sorgente,"è un vertice isolato")
      #La matrice e le due liste create vengono spostate su variabili apposite per la stampa del grafo,
      #mentre le variabili originarie vengono ripristinate per eventuali modifiche successive
      matricestampa <- matriceadiacenza
      matriceadiacenza <<- matricetemp
      verticistampa <- Vertici
      Vertici <<- verticitemp
      archistampa <- Archi
      Archi <<- architemp
      #Stampa il grafo ottenuto dall'algoritmo di Dijkstra
      VisualizzaGrafo(matricestampa, verticistampa, archistampa)
    },
    #PUNTO 5, esegue l'algoritmo di Kruskal, stampando il plot del grafo ottenuto
    Kruskal = function() {
      #Controlla se la matrice di adiacenza è simmetrica, cioè se il grafo è orientato o meno:
      #se il grafo è orientato, Kruskal non si usa, e quindi viene restituito un messaggio apposito
      if(!(isSymmetric.matrix(matriceadiacenza))) return(cat("Kruskal non può essere effettuato su un grafo orientato"))
      #Crea una copia temporanea della matrice di adiacenza e due della lista degli archi,
      #in modo da poter creare il nuovo grafo utilizzando gli stessi metodi del precedente
      matricetemp <- matriceadiacenza
      matriceadiacenza[] <<- 0
      architemp <- Archi
      archisparsi <- Archi
      Archi <<- list()
      #Utilizza la lista copiata "archisparsi" per aggiungere gli archi a quella originale mettendoli in ordine di peso
      while(length(archisparsi) > 0) {
        #Ad ogni ciclo viene memorizzato l'arco col valore minimo che sarà poi aggiunto alla matrice di adiacenza
        arcominimo <- list(NULL, NULL, 0)
        for(arco in archisparsi) {
          if(arcominimo[3] == 0 || arco$valore < arcominimo[3]) arcominimo <- list(arco$sorgente, arco$destinazione, arco$valore)
        }
        #Elimina dalla lista temporanea gli archi "doppi" a quelli minimi da aggiungere, riducendo così la sua lunghezza:
        #gli archi "doppi", cioè quelli che dato un arco (x,y) rappresentano l'arco (y,x) che ha lo stesso valore,
        #servivano in origine per riconoscere i grafi orientati da quelli non, e sono inutili in questo metodo che usa solo i secondi
        cont <- 0
        for(arco in archisparsi) {
          cont <- cont + 1
          if(arco$sorgente == arcominimo[[2]] && arco$destinazione == arcominimo[[1]]) break
        }
        archisparsi <- archisparsi[-cont]
        #Aggiunge l'arco col valore minimo alla lista degli archi, se non è un cappio, e poi lo rimuove dalla lista temporanea
        if(!(arcominimo[[1]] == arcominimo[[2]])) AggiungiArco(arcominimo[[1]], arcominimo[[2]], arcominimo[[3]])
        cont <- 0
        for(arco in archisparsi) {
          cont <- cont + 1
          if(arco$sorgente == arcominimo[[1]] && arco$destinazione == arcominimo[[2]]) break
        }
        archisparsi <- archisparsi[-cont]
      }
      #La matrice di adiacenza e la lista degli archi vengono inizializzate
      matriceadiacenza[] <<- 0
      archisparsi <- Archi
      Archi <<- list()
      #Crea la lista utilizzata per tenere traccia degli archi aggiunti (cioè dei "percorsi" creati), quindi per prevenire i cicli
      controllocicli <- list("")
      #Per ogni arco controlla più volte la lista dei cicli, agendo diversamente a seconda dei casi
      for(arco in archisparsi) {
        #La variabile "ciclo" verrà settata a TRUE se viene rilevato un ciclo, in modo da non aggiungere l'arco corrispondente
        ciclo <- FALSE
        for(percorso in 1:length(controllocicli)) {
          #Se sia la sorgente che la destinazione si trovano nello stesso vettore della lista, siamo di fronte ad un ciclo
          if(arco$sorgente %in% controllocicli[[percorso]] && arco$destinazione %in% controllocicli[[percorso]]) {
            ciclo <- TRUE
            break
          }
          #La variabile "trovato" è usata per controllare se sorgente e destinazione si trovano su due vettori diversi della lista:
          #in questo caso i due "percorsi" vengono uniti, altrimenti la destinazione viene aggiunta al percorso della sorgente
          trovato <- FALSE
          if(arco$sorgente %in% controllocicli[[percorso]]) {
            for(percorsoalt in percorso:length(controllocicli)) {
              if(arco$destinazione %in% controllocicli[[percorsoalt]]) {
                trovato <- TRUE
                controllocicli[[percorso]] <- append(controllocicli[[percorso]], controllocicli[[percorsoalt]])
                controllocicli <- controllocicli[-percorsoalt]
                break
              }
            }
            if(!trovato) controllocicli[[percorso]] <- append(controllocicli[[percorso]], arco$destinazione)
            break
          }
          #In questo caso il comportamento è ancora quello sopracitato, ma la destinazione si trova prima della sorgente nella lista
          if(arco$destinazione %in% controllocicli[[percorso]]) {
            for(percorsoalt in percorso:length(controllocicli)) {
              if(arco$sorgente %in% controllocicli[[percorsoalt]]) {
                trovato <- TRUE
                controllocicli[[percorso]] <- append(controllocicli[[percorso]], controllocicli[[percorsoalt]])
                controllocicli <- controllocicli[-percorsoalt]
                break
              }
            }
            if(!trovato) controllocicli[[percorso]] <- append(controllocicli[[percorso]], arco$sorgente)
            break
          }
          #Se infine abbiamo raggiunto il termine della lista, l'arco è aggiunto come nuovo "percorso"
          if(percorso == length(controllocicli)) controllocicli <- append(controllocicli, list(c(arco$sorgente, arco$destinazione)))
          #Viene usato solo una volta ed elimina il "percorso" creato inizialmente come inizializzazione provvisoria della lista
          if(controllocicli[[1]][1] == "") controllocicli <- controllocicli[-1]
        }
        #Aggiunge effettivamente l'arco e il suo "doppio" solo se non siamo in presenza di un ciclo
        if(!ciclo) {
          AggiungiArco(arco$sorgente, arco$destinazione, arco$valore)
          AggiungiArco(arco$destinazione, arco$sorgente, arco$valore)
        }
      }
      #La matrice e la lista degli archi create vengono spostate su variabili apposite per la stampa del grafo,
      #mentre le variabili originarie vengono ripristinate per eventuali modifiche successive
      matricestampa <- matriceadiacenza
      matriceadiacenza <<- matricetemp
      archistampa <- Archi
      Archi <<- architemp
      #Stampa il grafo ottenuto dall'algoritmo di Kruskal
      VisualizzaGrafo(matricestampa, Vertici, archistampa)
    }
  )
)

#Crea un nuovo oggetto "g", necessario per utilizzare i metodi della classe Graph
g <- Graph$new()

#Aggiunge i vertici ad un grafo di prova
g$AggiungiVertice("a",1)
g$AggiungiVertice("b",1)
g$AggiungiVertice("c",1)
g$AggiungiVertice("d",1)
g$AggiungiVertice("e",1)
g$AggiungiVertice("f",1)
g$AggiungiVertice("g",1)
g$AggiungiVertice("h",1)
g$AggiungiVertice("i",1)

#Aggiunge alcuni archi al grafo di prova
g$AggiungiArco("a","b",4)
g$AggiungiArco("b","c",8)
g$AggiungiArco("c","d",7)
g$AggiungiArco("d","e",9)
g$AggiungiArco("e","f",10)
g$AggiungiArco("f","g",2)
g$AggiungiArco("g","h",1)
g$AggiungiArco("h","i",7)
g$AggiungiArco("a","h",8)
g$AggiungiArco("g","i",6)
g$AggiungiArco("i","c",2)
g$AggiungiArco("d","f",14)
g$AggiungiArco("b","h",11)
g$AggiungiArco("c","f",4)

#Aggiunge gli archi opposti a quelli precedenti per formare un grafo non orientato
g$AggiungiArco("b","a",4)
g$AggiungiArco("c","b",8)
g$AggiungiArco("d","c",7)
g$AggiungiArco("e","d",9)
g$AggiungiArco("f","e",10)
g$AggiungiArco("g","f",2)
g$AggiungiArco("h","g",1)
g$AggiungiArco("i","h",7)
g$AggiungiArco("h","a",8)
g$AggiungiArco("i","g",6)
g$AggiungiArco("c","i",2)
g$AggiungiArco("h","b",11)
g$AggiungiArco("f","d",14)
g$AggiungiArco("f","c",4)

#Restituisce, il tempo di esecuzione dell'algoritmo di Kruskal
tempoesecuzionekruskal <- proc.time()
g$Kruskal()
tempoesecuzionekruskal <- proc.time() - tempoesecuzionekruskal
cat("Tempo esecuzione Kruskal:", tempoesecuzionekruskal[1],
    "\nTempo processi di sistema:", tempoesecuzionekruskal[2],
    "\nTempo totale trascorso:", tempoesecuzionekruskal[3], "\n")