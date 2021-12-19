/*

NOTE SULL'AVVIO DEL PROGRAMMA.

Per l'avvio del programma digitare al prompt:

?- start.

Verrà quindi visualizzato il menu, che permette la scelta di tre opzioni:

1 - Imposta parametri

Permette la definizione dei parametri quale Jwidth, Jlimit (compreso tra 0 e 1) ed i
i percorsi dei file di input ed output.
Questi devono essere inseriti nel formato c:\nome_percorso\nome_file

2 - Salva Testo Giustificato

Questa opzione può essere scelta solo dopo aver impostato i parametri di ingresso al punto 1.
Il testo verrà giustificato dopo una fase di preformattazione e, nel caso sia necessaria,
anche con l'ausilio della Sillabazione

3 - Salva Testo Preformattato

Crea un nuovo file nel percorso specificato contenente il testo del file in ingresso dopo
la sola procedura di preformattazione. Il testo in output non sarà quindi giustificato.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%				%
%	I/O FILE		%
%				%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
PROCEDURA DI APERTURA DEL FILE:
Utilizzo due diverse procedure per l'apertura del file da processare.

PROCEDURA N.1
Il file viene aperto e vengono create N Linee che verranno più tardi
analizzate durante il processo di giustificazione.
Questo primo procedimento crea una linea ogni volta che nel testo viene
trovato un Newline (codice ASCII 10), o nel caso dell'ultima linea,
alla fine del file (-1)
*/

% leggi_stringa/1
% Attraverso la funzione leggi_lista_car/1 che legge un carattere alla volta
% fino a quando non incontra un Newline o la fine del file, questa procedura
% crea la stringa S dopo aver convertito i codici ASCII dei caratteri letti
% tramite il predicato name/2

leggi_stringa(S) :- leggi_lista_car(L),
                    name(S,L).

leggi_lista_car([C|R]) :- get0(C),
                          C \== 10, C\== -1,!,
                          leggi_lista_car(R).
leggi_lista_car([]).


% apri_file/1
% L'argomento passato è il percorso del file da processare; il file viene
% suddiviso in diverse linee con l'ausilio della funzione leggi_lista_car.
% Ad ogni linea viene associato il parametro N, indispensabile per tenere
% traccia dell'ordine delle linee che verranno successivamente giustificate.


                    
apri_file(F):- see(F),
               crea_linee(1),
               seen.

crea_linee(N):- \+ at_end_of_stream,!,
               leggi_lista_car(L),
               length(L,X),
               assert(linea(L,X,N)),
               N2 is N+1,
               crea_linee(N2).
crea_linee(N).

/*
PROCEDURA N.2
Questa procedura varia leggermente da quella precedente. Crea un'unica lista
che verrà processata nella fase di preformattazione, pertanto interrompe
il flusso di lettura dei caratteri solo quando trova la fine del file
(e non, come nel caso precedente, anche nel caso in cui venga scandito un Newline)
*/

leggi_lista_car2([C|R]) :- get0(C),
                           C\== -1,!,
                           leggi_lista_car2(R).
leggi_lista_car2([]).

apri_file2(F):- see(F),
                  \+ at_end_of_stream,!,
                  leggi_lista_car2(S),
                  assert(testo_input(S)),
                  seen.

% salva_file/1
% Viene creato il file specificato dal parametro F.
% Il file conterrà il testo salvato in testo_output/1 dopo esser stato 
% opportunamente convertito in caratteri (la lista è infatti inzialmente 
% composta esclusivamente da caratteri ASCII)

salva_file2(F) :-
      tell(F),
      testo_output(Testo),
      name(S,Testo),
      write(S),
      told.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					%
%	PROCEDURA GIUSTIFICA		%
%					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Il corpo della procedura Giustifica si suddivide in tre grandi blocchi.

BLOCCO N.1:
Questo blocco viene selezionato quando la linea (il cui numero è passato come parametro N)
è inferiore o uguale a Jwidth, ovvero la lunghezza massima consentita.
Questo è il caso più semplice: la linea può essere aggiunta senza ulteriori controlli
alla stringa definitiva che verrà passata in output al termine dell'algoritmo
*/

giustifica(N) :- 
                   linea(Line,Lungh,N),
                   jwidth(Jwidth),
                   =<(Lungh,Jwidth),
		   append(Line,[10],Linea_da_aggiungere),
		   stringa_def(Stringa),
		   abolish(stringa_def/1),
		   append(Stringa,Linea_da_aggiungere,New_stringa),
		   assert(stringa_def(New_stringa)),

% Somma 1 e passa alla linea successiva...
                   Num is N+1,
                   giustifica(Num).

/*
BLOCCO N.2:
Questo blocco viene selezionato quando la lunghezza della linea supera il valore del
parametro Jwidth e quando è possibile ottenere una corretta giustificazione del
testo senza l'ausilio della sillabazione
*/


giustifica(N) :- 
                   linea(Line,Lungh,N),
                   jwidth(Jwidth),
                   >(Lungh,Jwidth),

% Tramite il parametro ultimo_spazio/3 viene individuato l'ultimo carattere Space (cod. ASCII 32)
% della linea in esame, precedente al carattere che si trova nella posizione Jwidth.
% In due ulteriori variabili memorizziamo il valore di ultimo_spazio +/- 1
% indispensabile più in avanti.

                   ultimo_spazio(Ultimo,Line,Jwidth),
		   Ultimo_plus1 is Ultimo + 1,
		   Ultimo_minus1 is Ultimo - 1,

% Creo una sottolista dall'inizio all'ultimo spazio (escluso). Questa sarà la linea che verrà formattata
% più in avanti

                   sublist(Line,1,Ultimo_minus1,Stringa_da_giustificare),

% Calcolo anche il numero di spazi della sotto-lista alla quale verranno aggiunti ulteriori spazi.

                   num_spazi(Stringa_da_giustificare,Num_spazi),

% Verifico se è possibile giustificare la linea

		   verifica(Ultimo,Num_spazi),!,

% Il numero di spazi da aggiungere alla linea equivale alla differenza tra il valore di Jwidth
% e l'ultimo spazio della linea (escluso)

		   Spazi_da_inserire is Jwidth - Ultimo_minus1,

% Passa il controllo alla procedura formatta che aggiunge gli spazi calcolati.

                   formatta(Stringa_da_giustificare,Spazi_da_inserire,Stringa_giustificata),

% Viene infine inclusa la linea così ottenuta al testo di output, e creata una nuova linea
% che consiste della parte rimanente della linea appena processata.
% Per far questo utilizzo la funzione sublist: la nuova stringa inizia dall'ultimo spazio
% precedentemente trovato e finisce al termine della lista. 

		   append(Stringa_giustificata,[10],Linea_da_aggiungere),
		   sublist(Line,Ultimo_plus1,Lungh,Stringa_rimanente),
                   length(Stringa_rimanente,Lungh2),
                   stringa_def(Stringa),
		   abolish(stringa_def/1),
		   append(Stringa,Linea_da_aggiungere,New_stringa),
		   assert(stringa_def(New_stringa)),retract(linea(Line,Lungh,N)),

                   assert(linea(Stringa_rimanente,Lungh2,N)),

% La linea appena creata viene passata come parametro per essere immediatamente processata.

                   giustifica(N).

/*
BLOCCO N.3
Il terzo ed ultimo blocco è selezionato quando, come nel caso precedente, la lunghezza della linea
supera il parametro Jwidth ma la linea non può essere giustificata senza l'ausilio della sillabazione
*/

giustifica(N) :- 

% La prima parte è analoga al caso precedente...

                   linea(Line,Lungh,N),
                   jwidth(Jwidth),
                   >(Lungh,Jwidth),
                   ultimo_spazio(Ultimo,Line,Jwidth),
		   Ultimo_plus1 is Ultimo + 1,
		   Ultimo_minus1 is Ultimo - 1,
                   sublist(Line,1,Ultimo_minus1,Stringa_da_giustificare),
                   num_spazi(Stringa_da_giustificare,Num_spazi),

% La procedura verifica/3 restituisce 'No'

		   \+ verifica(Ultimo,Num_spazi),!,

% Il controllo viene quindi passato alla procedura sillabazione/3, che verrà analizzata nel seguito.
% Per adesso è sufficiente sapere che tale procedura fornisce in output il valore Index, che indica la fine
% della sottostringa che verrà creata

		   sillabazione(Jwidth, Line, Index),
		   Spazi_da_inserire is Jwidth - Index -1, 
		   sublist(Line, 1, Index, Stringa1),

% A questa linea viene aggiunto il carattere '-' (codice ASCII 45)
 
		   append(Stringa1, [45], Stringa_da_giustificare2), 

% La procedura formatta aggiunge alla stringa gli spazi necessari per far sì che questa abbia lunghezza Jwidth

		   formatta(Stringa_da_giustificare2,Spazi_da_inserire, Stringa_giustificata),
		   append(Stringa_giustificata,[10],Linea_da_aggiungere),		   
		   Index_plus1 is Index + 1,nl,

% Anche la parte finale del blocco è analoga al caso precedente: viene creata una nuova linea con la parte
% eccedente, e questa viene immediatamente passata come parametro alla procedura di giustificazione.

		   sublist(Line,Index_plus1,Lungh,Stringa_rimanente),
		   length(Stringa_rimanente,Lungh2),
                   stringa_def(Stringa),
		   abolish(stringa_def/1),
		   append(Stringa,Linea_da_aggiungere,New_stringa),
		   assert(stringa_def(New_stringa)),retract(linea(Line,Lungh,N)),

                   assert(linea(Stringa_rimanente,Lungh2,N)),
                   giustifica(N).	

giustifica(N).

/*
La procedura formatta/3 è molto semplice:
Se viene trovato uno spazio e N (numero di spazi da aggiungere) è > di 0, allora viene
aggiunto uno spazio e decrementato N.
Il procedimento continua fino a quando N=0
*/

formatta([H|T], N, [H1,H2|T1]) :- H == 32,
                                    N > 0,
                                    H1 = H,
                                    H2 = H,
                                    Num is N - 1,
                                    formatta(T,Num,T1).

formatta([H|T], N, [H1|T1]) :- \+H == 32, 
                                   H1 = H, 
                                   formatta(T,N,T1).
formatta([H|T], N, [H1|T1]) :-   H == 32, 
                                   N == 0,
                                   H1 = H, 
                                   formatta(T,N,T1).
formatta([],N,[]).

                  

/* 

Questa procedura, anche se funzionante, non è conveniente
quando si ha a che fare con stringhe particolarmente lunghe...

num_spazi([H|T],X) :- num_spazi(T,Y), H == 32, X is Y+1.
num_spazi([H|T],X) :- num_spazi(T,X), \+ H == 32.
num_spazi([],0).

Ne implemento quindi una seconda, più efficace anche se un po' più complessa.
Innanzi tutto implemento una funzione elimina(Elem, Old, New) che restituisce
yes sse New è la lista Old alla quale sono state rimosse tutte le occorrenze
dell'elemento Elem
*/


elimina(Elem, Old, New) :-
   (var(Elem) -> (remove_dupl(Old, Old2),
                  member(Elem, Old2)
                 );
                  true 
   ),
   elimina2(Elem, Old, New).

elimina2(_, [], []).
elimina2(Elem1, [Elem2 | Resto], New_list) :-
   Elem1 = Elem2 -> (elimina2(Elem1, Resto, New_list)
                    );
                    (New_list = [Elem2 | Resto2],
                     elimina2(Elem1, Resto, Resto2)
                    ).

% Creo poi la vera e propria procedura num_spazi, che servendosi della procedura elimina/3
% elimina tutte le occorrenze del carattere ASCII 32 dalla lista originaria, ed effettua
% poi una sottrazione tra la stringa così ottenuta e quella integra.

num_spazi(Old,X) :- elimina(32,Old,New),
                    length(Old,Y),
                    length(New,Z),
                    X is Y-Z.


% ultimo_spazio/3
% Si serve del parametro nth/3 che restituisce l'n-esimo elemento da una lista.
% Ult è l'ultimo spazio nella stringa S prima del carattere alla posizione Num

ultimo_spazio(Ult,S,Num):- nth(Num,S,Result),
                               Result == 32,
                               Ult = Num.
ultimo_spazio(Ult,S,Num):- nth(Num,S,Result),
                               \+ Result == 32,
                               N is Num - 1,
                               ultimo_spazio(Ult,S,N).
ultimo_spazio(0,[],X).
                           
% nth/3 
% Semplice procedura ricorsiva. Restituisce yes sse Result è l'elemento
% della lista [H|T] alla posizione X.

nth(X,[H|T],Result):- X=1, 
                      Result=H.
nth(X,[H|T],Result):- Y is X-1, 
                      nth(Y,T,Result).

% verifica(Ultimo, Num)
% Ha il compito di verificare se è possibile giustificare una linea senza l'ausilio della
% sillabazione. Questo è possibile sse il rapporto tra numero di caratteri eccedenti (e cioè dall'ultimo
% spazio alla lunghezza predefinita Jwidth) e il numero di spazi della linea (escluso l'ultimo)
% è inferiore al parametro Jlimit

verifica(Ultimo,Num):- \+ Num == 1,
                       jwidth(Jwidth),
                       X is Jwidth - Ultimo,
                       Y is Num - 1,
                       Z is X/Y,
                       jlimit(Jlimit),
                       =<(Z,Jlimit).


% sublist/4
% La procedura si appoggia alla sottoprocedura prima/2, che si occupa della prima parte
% della sottostringa.

sublist(List, Inizio, Fine, Sublist) :-
	prima(Sublist, List),
	Inizio = 1,
	length(Sublist, Fine).

% Ricorsivamente viene creata la sottolista, dopo aver impostato le variabili Inizio_minus1
% e Fine_minus1

sublist([Elem|Resto], Inizio, Fine, Sublist) :-
	sublist(Resto, Inizio_minus1, Fine_minus1, Sublist),
	Inizio is Inizio_minus1 + 1,
	Fine is Fine_minus1 + 1.

prima([], _).
prima([Elem|Resto_parte], [Elem|Resto_intero]) :-
  prima(Resto_parte, Resto_intero).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%					%
%	PROCEDURA PREFORMAT		%
%					%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
La procedura preformat viene chiamata prima della giustificazione vera e propria.
La parte più complessa della procedura è per il controllo dei blank (Space e Newline),
che verrà analizzata in seguito.
Innanzi tutto definisco una lista con i caratteri concessi, quelli cioè ammissibili.
Se nel file di input verrà individuato un carattere che non è presente nella lista, questo
verrà omesso.
*/

caratteri_concessi([48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,
68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,
95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,
116,117,118,119,120,121,122,123,124,125,126,33,34,35,36,37,38,39,40,41,42,43,44,
45,46,47]).

% Creo anche una lista con i segni di interpunzione

segni_interpunzione([46,33,63,58,59,44]).

% Non essendo specificato nel testo, adotto la convenzione che, nel caso sia presente uno
% Space seguito da un Newline (caso di due blank consecutivi), viene eliminato il Newline
% e preservato lo Space.

% La procedura preformat/2 ha due argomenti: la lista di caratteri originaria e quella
% relativa al testo preformattato.
% Vengono presi in esame i diversi casi di interesse:

% Se ci sono due spazi consecutivi ne elimineremo uno:
preformat([H1|T1],[H2|T2]):- H1=32,

% trim/2 elimina tutti gli spazi e il newline

                             trim(T1,[H3|T3]),

% Lo spazio viene inserito sse il primo elemento della stringa sottoposta a trim 
% non è un segno di interpunzione...

                             segni_interpunzione(Punti),
                             \+ member(H3,Punti),!,
                             H2=32, preformat([H3|T3],T2).

% ...altrimenti non inserisco nessun carattere space

preformat([H1|T1],[H2|T2]):- H1=32,
                             trim(T1,[H3|T3]),
                             segni_interpunzione(Punti),
                             member(H3,Punti),!,
                             H2=H3, preformat(T3,T2).

% Ho bisogno di una procedura analoga per i Newline. Occorre tener presente però
% che sono ammessi più Newline consecutivi.
% Implemento pertanto una seconda procedura trim: trim_sp che elimina solamente
% gli spazi.

preformat([H1|T1],[H2|T2]):- H1=10,!,
			     trim_sp(T1,T3),
			     H2=H1,
			     preformat(T3,T2).

% Nel caso venga trovato un segno di interpunzione e questo sia seguito da un Newline,
% preservo il Newline. In caso contrario il Newline non rappresenta un separatore di
% paragrafo, pertanto viene sostituito da uno Space.

preformat([H1,Y1|T1],[H2,Y2|T2]):- segni_interpunzione(Punti),
				   member(H1,Punti),
				   Y1=10,
				   H1=H2,

% Viene effettuato un controllo analogo al precedente: se il primo carattere successivo
% agli eventuali Newline e Space consecutivi non è un segno di interpunzione...

				   trim(T1,[H3|T3]),
				   \+ member(H3,Punti),!,
				   Y1=Y2,

% ....preservo comunque in Newline ed elimino gli spazi...

      				   trim_sp(T1,T4),
				   preformat(T4,T2).

preformat([H1,Y1|T1],[H2,Y2|T2]):- segni_interpunzione(Punti),
				   member(H1,Punti),
				   Y1=10,
				   H1=H2,
				   trim(T1,[H3|T3]),

% ...altrimenti elimino tutti i blank

				   member(H3,Punti),!,
				   preformat([H3|T3],[Y2|T2]).

% Come accennato precedentemente, se il carattere non è un segno di interpunzione ed è seguito
% da un Newline, il newline non è un separatore di paragrafo e viene pertanto sostituito da uno Space

preformat([H1,Y1|T1],[H2,Y2|T2]):- segni_interpunzione(Punti),
				   \+ member(H1,Punti),
				   Y1=10,!,
				   H2=H1, Y2=32,
				   trim(T1,T3),
				   preformat(T3,T2).

% Eliminiamo inoltre eventuali sillabazioni (carattere '-' seguito da Newline)

preformat([H1,Y1|T1],[H2|T2]):- H1=45, Y1=10,!,
 				preformat(T1,T2).

% Verifichiamo infine che l'elemento faccia parte della gamma di caratteri ammessi

preformat([H|T],[H1|T1]):- caratteri_concessi(Lista), \+ member(H,Lista), !, H1=94, preformat(T,T1).

% Se nessuna delle condizioni precedenti è soddisfatta il carattere può essere inserito senza ulteriori
% controlli

preformat([H|T],[H1|T1]):- H1=H, !,preformat(T,T1).
preformat([],[]).

% start_pref/2
% Elimina eventuali blank all'inizio e alla fine del testo ed avvia il procedimento di preformattazione

start_pref(Str_input,Str_output):- trim(Str_input,W),
                                   reverse(W,X),
                                   trim(X,Y),
                                   reverse(Y,Z),
                                   preformat(Z,Str_output).
start_pref([],[]).

% Semplici procedure ricorsive la cui utilità è stata descritta in precedenza

trim([S1|S2], Str_output):- \+ S1=32, \+ S1=10, !, Str_output=[S1|S2].
trim([S1|S2], Str_output):- trim(S2,Str_output).
trim([],[]).

trim_sp([S1|S2], Str_output):- \+ S1=32, !, Str_output=[S1|S2].
trim_sp([S1|S2], Str_output):- trim(S2,Str_output).
trim_sp([],[]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%						%
%		SILLABAZIONE			%
%						%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
Il processo di Sillabazione si articola nella due sottoprocedure controlla_sillabazione/3
e sillabazione/3.
controlla_sillabazione/3 controlla che sia effettivamente possibile una suddivisione nel
rispetto delle principali regole grammaticali relative alla suddivisione in sillabe.
sillabazione/3 è una procedura ricorsiva che permette di trovare una corretta suddivisione
*/

% Prima creo una lista di vocali e di consonanti.
% Le consonanti speciali sono l, m, n, r. La loro utilità sarà evidente più in avanti

vocali([97,101,105,111,117,121,106,    % Codici ASCII delle minuscole...
        65,69,73,79,85,89,74]).        % ... e delle maiuscole

consonanti([98,99,100,102,103,104,107,108,109,110,112,113,114,115,116,118,119,120,122,
            66,67,68,70,71,72,75,76,77,78,80,81,82,83,84,86,87,88,89]).

consonanti_speciali([76,77,78,82,108,109,110,114]).

c([99,67]).
q([113,81]).


% Il processo di verifica sulla correttezza della suddivisione può essere esemplificato ai seguenti
% 5 casi.

% CASO 1: E' possibile andare a capo se l'ultimo carattere della linea è una vocale.
%	  La linea successiva comincerà con una consonante seguita da un'altra vocale.
  
controlla_sillabazione(Char2, Char3, Char4):-
	vocali(Vocali), consonanti(Consonanti),
	member(Char2, Vocali), member(Char3, Consonanti), member(Char4, Vocali).

% CASO 2: L'ultimo carattere di una linea può essere una 'consonante speciale', se questa è seguita
%	  da un'altra consonante

controlla_sillabazione(Char2, Char3, Char4):-
	vocali(Vocali), consonanti(Consonanti), consonanti_speciali(Cons_speciali),
	member(Char2, Cons_speciali), member(Char3, Consonanti).

% CASO 3: Si possono suddividere due consonanti quando queste sono uguali o....

controlla_sillabazione(Char2, Char3, Char4):-
	consonanti(Consonanti),	member(Char2, Consonanti), member(Char3, Consonanti),
	Char2=Char3.

% CASO 4: ....o nel caso la prima sia una C e la seconda una Q

controlla_sillabazione(Char2, Char3, Char4):-
	consonanti(Consonanti),	member(Char2, Consonanti), member(Char3, Consonanti),
	c(C), q(Q), member(Char2, C), member(Char3, Q).

% CASO 5: Il quinto e ultimo caso si ha quando una vocale è seguita da una consonante che non sia
%	  l, m, n, r e da un'ulteriore consonante. Le ultime due consonanti non devono però essere uguali.

controlla_sillabazione(Char2, Char3, Char4):-
	vocali(Vocali), consonanti(Consonanti), consonanti_speciali(Cons_speciali),
	member(Char2, Vocali), member(Char3, Consonanti), \+ member(Char3, Cons_speciali),
	member(Char4, Consonanti), \+ Char3 = Char4.


% sillabazione/3
% Va avanti ricorsivamente , decrementando volta per volta il parametro Index, fino a quando 
% non trova una corretta sillabazione attraverso la procedura controlla_sillabazione/3

sillabazione(Num, Line, Index) :-
		   Num_minus1 is Num - 1,
		   Num_plus1 is Num + 1,
		   nth(Num_minus1, Line, Char1),
		   nth(Num, Line, Char2),
		   nth(Num_plus1, Line, Char3),
		   controlla_sillabazione(Char1, Char2, Char3),!,
		   Index is Num-1.

sillabazione(Num, Line, Index) :-
		   Num_minus1 is Num - 1,
		   Num_plus1 is Num + 1,
		   nth(Num_minus1, Line, Char1),
		   nth(Num, Line, Char2),
		   nth(Num_plus1, Line, Char3),
		   \+ controlla_sillabazione(Char1, Char2, Char3),!,
		   sillabazione(Num_minus1, Line, Index).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%                             %%%%%%%%%
%%%%%          MENU		  %%%%%%%%%
%%%%%                             %%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start:- menu,
        get(Op),get0(_),
        Op \== 52,!,
        eseguiOperazione(Op),
        start.
start:- write('Arrivederci!').

menu:- nl,
       write('Scegli una delle seguenti operazioni: '), nl,
       write('1 - Imposta Parametri'), nl,
       write('2 - Stampa Testo Giustificato (con Preformattazione e Sillabazione)'), nl,
       write('3 - Stampa Testo Preformattato (senza Giustificazione)'), nl,
       write('4 - Esci'), nl.

% Operazione 1 - Imposta parametri:
eseguiOperazione(49):-
	abolish(testo_input/1), abolish(testo_output/1),
	abolish(jwidth/1), abolish(jlimit/1),
        abolish(input_path/1), abolish(output_path/1),
	abolish(stringa_def/1), abolish(linea/3),
	write('Inserire valore Jwidth: '),
        leggi_stringa(Jwidth),nl,
	write('Inserire valore Jlimit (da 0 a 1): '),
        leggi_stringa(Jlimit),nl,
	write('Percorso file di Input: '),
	leggi_stringa(Input_path),nl,
	write('Percorso file di Output: '),
	leggi_stringa(Output_path),nl,
        assert(jwidth(Jwidth)),
        assert(jlimit(Jlimit)),
	assert(input_path(Input_path)),
	assert(output_path(Output_path)).

% Operazione 2 - Giustificazione completa
eseguiOperazione(50):-
% Viene creato il file con il testo preformattato...
       input_path(Nome_file_in),
       apri_file2(Nome_file_in),
       testo_input(Testo_input),
       start_pref(Testo_input, Testo_output),
       assert(testo_output(Testo_output)),
       output_path(Nome_file_out),
       salva_file2(Nome_file_out),
%% E dopo viene passato come parametro per la giustificazione tale testo...
	apri_file(Nome_file_out),
	assert(stringa_def([])),
	giustifica(1),
        stringa_def(Stringa),
	abolish(testo_output/1),
	assert(testo_output(Stringa)),
        output_path(Nome_file_out),
	salva_file2(Nome_file_out).

% Operazione 4 - Solo Preformattazione
eseguiOperazione(51):- 
       input_path(Nome_file_in),
       apri_file2(Nome_file_in),
       testo_input(Testo_input),
       start_pref(Testo_input, Testo_output),
       assert(testo_output(Testo_output)),
       output_path(Nome_file_out),
       salva_file2(Nome_file_out).
