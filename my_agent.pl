% :- dynamic <predicado>/<aridade>.
% Declara que os fatos para estes predicados podem ser adicionados ou removidos durante a execução.

:- dynamic stench/3.
:- dynamic breeze/3.
:- dynamic glitter/3.
:- dynamic scream/3.
:- dynamic interaction/1.
:- dynamic position/3.
:- dynamic orientation/2.
:- dynamic act/2.
:- dynamic wall/2.     % Parede está em uma coordenada (L,C)
:- dynamic gold1/2.
:- dynamic visited/2.
:- dynamic safe/2.
:- dynamic has_gold/0.
:- dynamic danger/2.
:- dynamic path_back/1.
:- dynamic wumpus/2.
:- dynamic has_arrow/0.
:- dynamic wumpus_dead/0.
:- dynamic scream_handled/1.

% init_agent/0: Prepara o agente para um novo jogo, limpando a base de conhecimento.
init_agent :-
    retractall_predicates,
    assertall_current.

% retractall_predicates/0: Remove todos os fatos dinâmicos da memória.
retractall_predicates :-
    retractall(stench(_,_,_)),
    retractall(breeze(_,_,_)),
    retractall(glitter(_,_,_)),
    retractall(scream(_,_,_)),
    retractall(interaction(_)),
    retractall(position(_,_,_)),
    retractall(orientation(_,_)),
    retractall(act(_,_)),
    retractall(wall(_,_)),
    retractall(gold1(_,_)),
    retractall(visited(_,_)),
    retractall(safe(_,_)),
    retractall(has_gold),
    retractall(danger(_,_)),
    retractall(scream_handled(_)).

% assertall_current/0: Define o estado inicial do agente.
assertall_current :-
    asserta(interaction(0)),
    asserta(position(1,1,0)),
    asserta(orientation(east,0)),
    asserta(act([],0)),
    asserta(visited(1,1)),
    asserta(safe(1,1)),
    asserta(path_back([])),
    asserta(has_arrow),
    retractall(wumpus_dead).

restart_agent :-
    init_agent.

my_agent(Percept, Action) :-
    see(Percept, S),
    next(S),
    (has_gold ->
        (return_to_start(Action),
         interaction(K), asserta(act(Action,K))) ;
        action(Action)).

% PROCESSAMENTO DE PERCEPÇÕES
see(LPercepts, LAtoms) :-
    see1(LPercepts, 1, LAtoms),
    log_see(LPercepts, LAtoms).

log_see(LPercepts, LAtoms) :-
    nl, write('Log_see'), nl,
    write('Percepts = '), write(LPercepts), nl,
    write('Atoms = '), write(LAtoms), nl.

see1([], _, []) :- !.
see1([no|R], X, L) :- !, X1 is X+1, see1(R, X1, L).
see1([yes|R], X, [A|L]) :- X1 is X+1, translate(X, A), see1(R, X1, L).

translate(1, stench) :- !.
translate(2, breeze) :- !.
translate(3, glitter) :- !.
translate(4, bump) :- !.
translate(5, scream).

infer_wumpus_position :-
    findall((L,C), stench(L,C,_), Stenches),
    find_possible_wumpus_cells(Stenches, Candidates),
    (Candidates = [(L,C)] ->
        (\+ wumpus(L,C) ->
            assertz(wumpus(L,C))
        ; true)
    ; true).

find_possible_wumpus_cells(Stenches, UniqueCandidates) :-
    (setof((X,Y),
        (
            adjacent_to_all(Stenches, (X,Y)),
            \+ visited(X,Y),
            \+ safe(X,Y),
            is_valid(X,Y)
        ),
        AllCandidates) ->
            exclude(already_known_wumpus, AllCandidates, UniqueCandidates)
    ;
        UniqueCandidates = []
    ),
    !.

% Após ouvir o grito do Wumpus, remove os dangers causados por stench
handle_scream_cleanup(K) :-
    write('[DEBUG] Limpando pós-grito na interação '), write(K), nl,
    (\+ wumpus_dead -> asserta(wumpus_dead) ; true),

    findall((L1,C1), stench(L1,C1,_), StenchPositions),
    findall((DL,DC), (
        member((SL,SC), StenchPositions),
        adjacent(SL, SC, DL, DC),
        danger(DL,DC)
    ), DangersToRemoveDup),
    sort(DangersToRemoveDup, DangersToRemove),
    forall(member((DL,DC), DangersToRemove), (
        write('[DEBUG] Removendo danger de '), write((DL,DC)), nl,
        retract(danger(DL,DC))
    )),
    retractall(stench(_,_,_)),
    asserta(scream_handled(K)).

adjacent_to_all([], _).
adjacent_to_all([(L,C)|Rest], (X,Y)) :-
    adjacent(L,C,X,Y),
    adjacent_to_all(Rest, (X,Y)).

already_known_wumpus((L,C)) :- wumpus(L,C).

% ATUALIZAÇÃO DO ESTADO DO MUNDO E CONHECIMENTO
next(LAtoms) :-
    current_interaction(Lprev, Cprev, D, A, Knew, LAtoms),
    effect(Lprev, Cprev, D, A, Knew, LAtoms),
    position(Lcur, Ccur, Knew),
    assertall_percepts(Knew, Lcur, Ccur, LAtoms), 
    evolution(Knew),
    mark_dangers(Lcur, Ccur, LAtoms),
    infer_wumpus_position,
    log_next.

log_next :-
    nl, write('Log_next'), nl,
    write('Knowledge Base'), nl,
    listing(interaction),
    listing(position),
    listing(orientation),
    listing(stench/3),
    listing(breeze/3),
    listing(glitter/3),
    listing(scream/3),
    listing(wall/2),
    listing(gold1/2),
    listing(danger/2),
    listing(safe/2),
    listing(visited/2),
    listing(act/2),
    print_stack.

current_interaction(L,C,D,A,K1,_LAtoms) :-
    retract(interaction(K)),
    position(L,C,K),
    orientation(D,K),
    act(A,K),
    K1 is K+1,
    asserta(interaction(K1)),
    % assertall_percepts(K1, L, C, LAtoms),
    (\+ visited(L,C) -> asserta(visited(L,C)) ; true),
    (\+ safe(L,C) -> asserta(safe(L,C)) ; true),
    (safe(L,C) -> retractall(danger(L,C)) ; true).

% Atualiza conhecimento com base nas percepções
assertall_percepts(_,_,_,[]) :- !.
assertall_percepts(K,L,C,[bump|R]) :- !, assertall_percepts(K,L,C,R).
assertall_percepts(K,L,C,[scream|R]) :-
    asserta(scream(L,C,K)),
    (\+ scream_handled(K) ->
        handle_scream_cleanup(K)
    ; true),
    assertall_percepts(K,L,C,R).
assertall_percepts(K,L,C,[stench|R]) :-
    wumpus_dead,  % <- IGNORA stench se o Wumpus está morto
    !,
    assertall_percepts(K,L,C,R).
assertall_percepts(K,L,C,[Atom|R]) :-
    Predicate =.. [Atom, L, C, K],
    asserta(Predicate),
    assertall_percepts(K,L,C,R).

% EFEITOS DAS AÇÕES
effect(L, C, D, [], K, _) :-
    asserta(position(L, C, K)),
    asserta(orientation(D, K)).

effect(L, C, D, grab, K, _) :-
    (retract(gold1(L, C)) -> asserta(has_gold) ; true),
    asserta(position(L, C, K)),
    asserta(orientation(D, K)),
    !.

effect(L, C, D, turnleft, K, _) :-
    turn_left(D, ND),
    asserta(position(L, C, K)),
    asserta(orientation(ND, K)).

effect(L, C, D, turnright, K, _) :-
    turn_right(D, ND),
    retractall(orientation(_, K)),
    asserta(position(L, C, K)),
    asserta(orientation(ND, K)).

effect(L,C,D,goforward,K,LAtoms) :-
    (member(bump, LAtoms) ->
        asserta(position(L,C,K)),
        asserta(orientation(D,K)),
        next_position(L,C,D,WL,WC),
        (\+ wall(WL,WC) -> asserta(wall(WL,WC)) ; true)
    ;
        next_position(L,C,D,NL,NC),
        asserta(position(NL,NC,K)),
        asserta(orientation(D,K)),
        ( \+ has_gold ->
            retract(path_back(Stack)),
            update_path_back((L,C), Stack, NewStack),
            asserta(path_back(NewStack))
        ; true )
    ).

effect(L, C, D, shoot, K, _) :-
    next_position(L, C, D, NL, NC),
    wumpus(NL, NC),
    retract(wumpus(NL, NC)),
    ( \+ wumpus_dead -> asserta(wumpus_dead) ; true ),
    retract(has_arrow),
    asserta(position(L, C, K)),
    asserta(orientation(D, K)),
    asserta(scream(L, C, K)),
    handle_scream_cleanup(K),  % essa parte faz a limpeza também
    !.

update_path_back(Pos, Stack, NewStack) :-
    remove_recent_cycle(Pos, Stack, NewStack).

remove_recent_cycle(Pos, Stack, Cleaned) :-
    append(_, [Pos|Suffix], Stack), !,
    Cleaned = [Pos|Suffix].
remove_recent_cycle(Pos, Stack, [Pos|Stack]).

turn_left(north, west).
turn_left(west, south).
turn_left(south, east).
turn_left(east, north).

turn_right(north, east).
turn_right(east, south).
turn_right(south, west).
turn_right(west, north).

evolution(K) :- evol(K), fail.
evolution(_).

evol(K) :-
    retract(glitter(L,C,K)),
    \+ gold1(L,C),
    asserta(gold1(L,C)).

% LÓGICA DE PERIGO
mark_dangers(L,C,LAtoms) :-
    (
        member(breeze, LAtoms)
        ;
        (member(stench, LAtoms), \+ wumpus_dead)
    ),
    forall(
        (adjacent(L,C,LA,CA), \+ visited(LA,CA), \+ safe(LA,CA), \+ danger(LA,CA)),
        asserta(danger(LA,CA))
    ).

mark_dangers(_,_,_).

adjacent(L,C,NL,C) :- NL is L+1, is_valid(NL,C).
adjacent(L,C,NL,C) :- NL is L-1, is_valid(NL,C).
adjacent(L,C,L,NC) :- NC is C+1, is_valid(L,NC).
adjacent(L,C,L,NC) :- NC is C-1, is_valid(L,NC).

is_valid(L,C) :- L > 0, L < 5, C > 0, C < 5.

% LÓGICA DE AÇÃO
action(A) :-
    decide_action(A),
    interaction(K),
    asserta(act(A,K)),
    log_action(A).

log_action(A) :-
    nl, write('Log_action'), nl,
    write('Action = '), write(A), nl.

% HIERARQUIA DE DECISÃO
decide_action(shoot) :-
    has_arrow,
    \+ wumpus_dead,
    interaction(K), position(L,C,K), orientation(D,K),
    next_position(L,C,D,NL,NC),
    wumpus(NL,NC),
    !.
decide_action(grab) :-
    interaction(K), position(L,C,K), gold1(L,C), !.
decide_action(Action) :- explore(Action), !.
decide_action(Action) :- revisit(Action), !.
decide_action(Action) :- backtrack(Action), !.
decide_action(turnright).

explore(goforward) :-
    interaction(K), position(L,C,K), orientation(D,K),
    next_position(L,C,D,NL,NC),
    write('[DEBUG] Testando GOFORWARD para: '), write((NL,NC)), nl,
    \+ wall(NL,NC), \+ visited(NL,NC), \+ danger(NL,NC), !.
explore(turnleft) :-
    interaction(K), position(L,C,K),
    adjacent(L,C,NL,NC),
    \+ wall(NL,NC), \+ visited(NL,NC), \+ danger(NL,NC), !.

revisit(goforward) :-
    interaction(K), position(L,C,K), orientation(D,K),
    next_position(L,C,D,NL,NC),
    \+ wall(NL,NC), visited(NL,NC), safe(NL,NC), !.
revisit(turnleft) :-
    interaction(K), position(L,C,K),
    adjacent(L,C,NL,NC),
    visited(NL,NC), safe(NL,NC), !.

backtrack(goforward) :-
    interaction(K), position(L,C,K), orientation(D,K),
    next_position(L,C,D,NL,NC),
    \+ wall(NL,NC), safe(NL,NC), !.
backtrack(turnleft) :-
    interaction(K), position(L,C,K),
    adjacent(L,C,NL,NC),
    \+ wall(NL,NC), safe(NL,NC), !.

next_position(L,C,east, L, C1) :- C1 is C+1.
next_position(L,C,west, L, C1) :- C1 is C-1.
next_position(L,C,north, L1, C) :- L1 is L+1.
next_position(L,C,south, L1, C) :- L1 is L-1.

next_position_to((L,C), north, (L1,C)) :- L1 is L + 1.
next_position_to((L,C), south, (L1,C)) :- L1 is L - 1.
next_position_to((L,C), east,  (L,C1)) :- C1 is C + 1.
next_position_to((L,C), west,  (L,C1)) :- C1 is C - 1.

return_to_start(climb) :-
    interaction(K),
    position(1,1,K), !.

return_to_start(goforward) :-
    interaction(K), position(L,C,K), orientation(D,K),
    path_back([(PL,PC)|Rest]),
    direction_to((L,C), (PL,PC), TargetDir),
    D == TargetDir,
    next_position_to((L,C), D, (PL,PC)),
    retract(path_back(_)),
    asserta(path_back(Rest)), !.

return_to_start(turnleft) :-
    interaction(K), position(L,C,K), orientation(D,K),
    path_back([(PL,PC)|_]),
    direction_to((L,C), (PL,PC), TargetDir),
    turn_left(D, D1),
    D1 == TargetDir, !.

return_to_start(turnright) :-
    interaction(K), position(L,C,K), orientation(D,K),
    path_back([(PL,PC)|_]),
    direction_to((L,C), (PL,PC), TargetDir),
    turn_right(D, D1),
    D1 == TargetDir, !.

return_to_start(turnleft) :-
    path_back(_), !.

desired_direction(north).
desired_direction(west).

distance(L,C,Dist) :- Dist is abs(L - 1) + abs(C - 1).

direction_to((L1,C), (L2,C), north) :- L2 is L1 + 1, !.
direction_to((L1,C), (L2,C), south) :- L2 is L1 - 1, !.
direction_to((L,C1), (L,C2), east)  :- C2 is C1 + 1, !.
direction_to((L,C1), (L,C2), west)  :- C2 is C1 - 1, !.

print_stack :-
    path_back(Stack),
    write('>>> Path_back stack = '), write(Stack), nl.
