#base.pl

:- dynamic localizacao/2, visitado/2, pokebolas/1, mapa/3, mapaEntidades/3, pontos/1, pokemon/1, visao/1, recuperado/1, pokemonsCapturados/1, mapaPontos/3, tipo/2.

% pokemon(pikachu, elétrico).
% mapa(1, 2, agua).
% mapaEntidades(5, 6, treinador).
% mapaEntidades(5, 7, pokemon(ratata)).
% mapaPontos(19, 25, 9).
% tipo(ratata, normal)

localizacao(19, 24).
visitado(19, 24).
visao(0). % 0 - baixo, 1 - direita, 2 - cima, 3 - esquerda 
pokebolas(25).
pontos(0).
pokemonsCapturados(0).
recuperado(1).

adicionarPontos(X) :-
  retract(pontos(Pontos)),
  P is Pontos + X,
  assert(pontos(P)).

adicionarPokemonCapturado(X) :-
  retract(pokemonsCapturados(Num)),
  N is Num + X,
  assert(pokemonsCapturados(N)).

adicionarBolas(X) :-
  retract(pokebolas(Bolas)),
  B is Bolas + X,
  assert(pokebolas(B)).

mudarLocalizacao(X, Y) :-
  retract(localizacao(Lin, Col)),
  L is Lin + X,
  C is Col + Y,
  assert(localizacao(L, C)),
  assert(visitado(L, C)).

virarDireita :-
  visao(Posicao),
  Virado is (Posicao + 3) mod 4,
  retract(visao(Posicao)),
  assert(visao(Virado)),
  adicionarPontos(-1).

virarEsquerda :-
  visao(Posicao),
  Virado is (Posicao + 1) mod 4,
  retract(visao(Posicao)),
  assert(visao(Virado)),
  adicionarPontos(-1).

andarParaFrente :- visao(0), mudarLocalizacao(1, 0), adicionarPontos(-1),!.
andarParaFrente :- visao(1), mudarLocalizacao(0, 1), adicionarPontos(-1),!.
andarParaFrente :- visao(2), mudarLocalizacao(-1, 0), adicionarPontos(-1),!.
andarParaFrente :- visao(3), mudarLocalizacao(0, -1), adicionarPontos(-1),!.

girarParaCima :- visao(0), virarDireita, virarDireita,!.
girarParaCima :- visao(1), virarEsquerda,!.
girarParaCima :- visao(2),!.
girarParaCima :- visao(3), virarDireita,!.

moverParaCima :-  girarParaCima, andarParaFrente.

girarParaDireita :- visao(0), virarEsquerda,!.
girarParaDireita :- visao(1),!.
girarParaDireita :- visao(2), virarDireita,!.
girarParaDireita :- visao(3), virarDireita, virarDireita,!.

moverParaDireita :- girarParaDireita, andarParaFrente.

girarParaBaixo :- visao(0),!.
girarParaBaixo :- visao(1), virarDireita,!.
girarParaBaixo :- visao(2), virarDireita, virarDireita,!.
girarParaBaixo :- visao(3), virarEsquerda,!.

moverParaBaixo :- girarParaBaixo, andarParaFrente.

girarParaEsquerda :- visao(0), virarDireita,!.
girarParaEsquerda :- visao(1), virarDireita, virarDireita,!.
girarParaEsquerda :- visao(2), virarEsquerda,!.
girarParaEsquerda :- visao(3),!.

moverParaEsquerda :- girarParaEsquerda, andarParaFrente.

recuperarPokemons :-
  localizacao(X, Y),
  mapaEntidades(X, Y, centroPokemon),
  retract(recuperado(Recuperado)),
  retract(mapaPontos(X, Y, _)),
  assert(recuperado(1)),
  adicionarPontos(-100),!.

resultadoDaBatalha :- recuperado(0), adicionarPontos(-1000),!.
resultadoDaBatalha :- recuperado(1), adicionarPontos(150), retract(recuperado(1)), assert(recuperado(0)),!.

batalharContraTreinador :-
  localizacao(X, Y),
  mapaEntidades(X, Y, treinador),
  resultadoDaBatalha,
  retract(mapaEntidades(X, Y, treinador)),
  retract(mapaPontos(X, Y, _)),
  assert(mapaEntidades(X, Y, vazio)).

precisaPegarBolas :-
  pokebolas(Bolas),
  pokemonsCapturados(NumPokemons),
  Sum is Bolas + NumPokemons,
  Sum < 150.

pegarPokebolas :-
  localizacao(X, Y),
  precisaPegarBolas,
  retract(mapaEntidades(X, Y, loja)),
  retract(mapaPontos(X, Y, _)),
  assert(mapaEntidades(X, Y, vazio)),
  adicionarBolas(25),
  adicionarPontos(-10).

capturar :-
  localizacao(X, Y),
  pokebolas(Bolas),
  Bolas > 0,
  mapaEntidades(X, Y, pokemon(Pokemon)),
  retract(mapaEntidades(X, Y, pokemon(Pokemon))),
  assert(mapaEntidades(X, Y, vazio)),
  assert(pokemon(Pokemon)),
  retract(mapaPontos(X, Y, _)),
  adicionarBolas(-1),
  adicionarPokemonCapturado(1),
  adicionarPontos(-5).

podeMover(X, Y) :- mapa(X, Y, grama),!.
podeMover(X, Y) :- mapa(X, Y, T), tipo(_, T),!.

verificarBloco(X, Y) :- mapaPontos(X, Y, Ponto), Ponto < 70, retract(mapaPontos(X, Y, Ponto)).
verificarBloco(X, Y) :- mapaPontos(X, Y, Ponto), Ponto >= 70,!.

% Não pode mover para o bloco
verificarBloco(X, Y) :- not(podeMover(X, Y)), assert(mapaPontos(X, Y, -1)),!.

blocoFrente(X, Y) :-
  visao(0),
  localizacao(Linha, Coluna),
  LPO is Linha + 1,
  X =:= LPO,
  Y =:= Coluna,!.

blocoFrente(X, Y) :-
  visao(1),
  localizacao(Linha, Coluna),
  CPO is Coluna + 1,
  X =:= Linha,
  Y =:= CPO,!.

blocoFrente(X, Y) :-
  visao(2),
  localizacao(Linha, Coluna),
  LMO is Linha - 1,
  X =:= LMO,
  Y =:= Coluna,!.

blocoFrente(X, Y) :-
  visao(3),
  localizacao(Linha, Coluna),
  CMO is Coluna - 1,
  X =:= Linha,
  Y =:= CMO,!.

blocoLado(X, Y) :-
  (visao(0); visao(2)),
  localizacao(Linha, Coluna),
  CPO is Coluna + 1,
  CMO is Coluna - 1,
  X =:= Linha,
  (Y =:= CPO; Y =:= CMO),!.

blocoLado(X, Y) :-
  (visao(1); visao(3)),
  localizacao(Linha, Coluna),
  LPO is Linha + 1,
  LMO is Linha - 1,
  (X =:= LPO; X =:= LMO),
  Y =:= Coluna,!.

% Pode mover para o bloco

verificarBloco(X, Y) :-
  podeMover(X, Y),
  mapaEntidades(X, Y, pokemon(Pokemon)),
  pokebolas(Bolas),
  Bolas > 0,
  assert(mapaPontos(X, Y, 100)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  mapaEntidades(X, Y, loja),
  precisaPegarBolas,
  assert(mapaPontos(X, Y, 90)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  mapaEntidades(X, Y, centroPokemon),
  recuperado(0),
  assert(mapaPontos(X, Y, 80)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  mapaEntidades(X, Y, treinador),
  pokemon(_),
  recuperado(1),
  assert(mapaPontos(X, Y, 70)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  not(visitado(X, Y)),
  assert(mapaPontos(X, Y, 60)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  blocoFrente(X, Y),
  assert(mapaPontos(X, Y, 50)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  blocoLado(X, Y),
  random(30, 49, Rand),
  assert(mapaPontos(X, Y, Rand)),!.

verificarBloco(X, Y) :-
  podeMover(X, Y),
  random(1, 29, Rand),
  assert(mapaPontos(X, Y, Rand)),!.

movimento(A, B, C, D) :- A >= B, A >= C, A >= D, moverParaBaixo,!.
movimento(A, B, C, D) :- B >= A, B >= C, B >= D, moverParaCima,!.
movimento(A, B, C, D) :- C >= A, C >= B, C >= D, moverParaDireita,!.
movimento(A, B, C, D) :- D >= A, D >= B, D >= C, moverParaEsquerda,!.

mover :-
  localizacao(Linha, Coluna),
  LPO is Linha + 1,
  LMO is Linha - 1,
  CPO is Coluna + 1,
  CMO is Coluna - 1,
  verificarBloco(LPO, Coluna),
  verificarBloco(LMO, Coluna),
  verificarBloco(Linha, CPO),
  verificarBloco(Linha, CMO),
  mapaPontos(LPO, Coluna, P1),
  mapaPontos(LMO, Coluna, P2),
  mapaPontos(Linha, CPO, P3),
  mapaPontos(Linha, CMO, P4),
  movimento(P1, P2, P3, P4).

acao :- pokebolas(Bolas), Bolas > 0, capturar,!.
acao :- precisaPegarBolas, pegarPokebolas,!.
acao :- recuperado(0), recuperarPokemons,!.
acao :- recuperado(1), pokemon(_), batalharContraTreinador,!.
acao :- mover,!.
