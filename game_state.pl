% GameState = (Global, Investigators, Map, Encounters, Stack)
	% Global = (Phase,ChaosBag,Scenario,Difficulty)
	% Investigators = (CurrInvId, List InvIds, List (InvId,Cards,Counters,Active))

:- discontiguous game_state_get/3.
:- discontiguous game_state_get/4.

:- ensure_loaded(investigator).
:- ensure_loaded(location).
:- ensure_loaded(helper).

inv_has_id(Id,I) :- inv_get("Id",I,Id).

%%%%% Get rules
game_state_get("Global",(G,_,_,_,_),G).
game_state_get("Invs",  (_,I,_,_,_),I).
game_state_get("Map",   (_,_,M,_,_),M).
game_state_get("Enc",   (_,_,_,E,_),E).
game_state_get("Stack", (_,_,_,_,S),S).
% Global
game_state_get("Scenario", GS,V) :- game_state_get("Global",GS,(V,_,_,_,_,_)).
game_state_get("Phase",    GS,V) :- game_state_get("Global",GS,(_,V,_,_,_,_)).
game_state_get("InvOrder", GS,V) :- game_state_get("Global",GS,(_,_,V,_,_,_)).
game_state_get("LeadInv",  GS,V) :- game_state_get("Global",GS,(_,_,_,V,_,_)).
game_state_get("CurrInvId",GS,V) :- game_state_get("Global",GS,(_,_,_,_,V,_)).
game_state_get("ChaosBag", GS,V) :- game_state_get("Global",GS,(_,_,_,_,_,V)).
% Investigators
game_state_get("Inv",Id,GS,I) :- game_state_get("Invs",GS,Is), get_inv(Id,Is,I).
game_state_get("CurrInv",GS,I) :- game_state_get("CurrInvId",GS,Id), game_state_get("Inv",Id,GS,I).
% Locations
game_state_get("Loc",LId,GS,L) :- game_state_get("Map",GS,Map), include(loc_filter("Id",LId),Map,[L]).
game_state_get(("Loc",LId),GS,L) :- game_state_get("Loc",LId,GS,L).
% Encounter
game_state_get("Enemies",   GS,V) :- game_state_get("Enc",GS,(V,_,_,_,_,_,_)).
game_state_get("EncDeck",   GS,V) :- game_state_get("Enc",GS,(_,V,_,_,_,_,_)).
game_state_get("EncDiscard",GS,V) :- game_state_get("Enc",GS,(_,_,V,_,_,_,_)).
game_state_get("AgendaDeck",GS,V) :- game_state_get("Enc",GS,(_,_,_,V,_,_,_)).
game_state_get("ActDeck",   GS,V) :- game_state_get("Enc",GS,(_,_,_,_,V,_,_)).
game_state_get("CurrAgenda",GS,V) :- game_state_get("Enc",GS,(_,_,_,_,_,V,_)).
game_state_get("CurrAct",   GS,V) :- game_state_get("Enc",GS,(_,_,_,_,_,_,V)).

game_state_get("NumInv",GS,N) :-
	game_state_get("InvOrder",GS,O),
	length(O,N).

game_state_get([],_,[]).
game_state_get([H|T],I,[X|L]) :- 
	game_state_get(H,I,X), 
	game_state_get(T,I,L).
%%%%% Set rules
% some set rules will not be used (e.g., scenario/difficulty)
game_state_set("Global",G,GS,GS1) :- GS = (_,I,M,E,S), GS1 = (G,I,M,E,S). 
game_state_set("Invs",  I,GS,GS1) :- GS = (G,_,M,E,S), GS1 = (G,I,M,E,S).
game_state_set("Map",   M,GS,GS1) :- GS = (G,I,_,E,S), GS1 = (G,I,M,E,S).
game_state_set("Enc",   E,GS,GS1) :- GS = (G,I,M,_,S), GS1 = (G,I,M,E,S).
game_state_set("Stack", S,GS,GS1) :- GS = (G,I,M,E,_), GS1 = (G,I,M,E,S).
% Global
game_state_set("Phase",    P,GS,GS1) :- GS = ((S,_,O,L,C,B),I,M,E,K), GS1 = ((S,P,O,L,C,B),I,M,E,K).
game_state_set("CurrInvId",C,GS,GS1) :- GS = ((S,P,O,L,_,B),I,M,E,K), GS1 = ((S,P,O,L,C,B),I,M,E,K).
game_state_set("ChaosBag", B,GS,GS1) :- GS = ((S,P,O,L,C,_),I,M,E,K), GS1 = ((S,P,O,L,C,B),I,M,E,K).
% Investigators
game_state_set("Inv",I,GS,GS1) :- 
	game_state_get("Invs",GS,Is), 
	replace_inv(I,Is,Is1), 
	game_state_set("Invs",Is1,GS,GS1).
game_state_set("SubInvs",[],GS,GS).
game_state_set("SubInvs",[I|T],GS,GS1) :-
	game_state_set("Inv",I,GS,GS2),
	game_state_set("SubInvs",T,GS2,GS1).
% Locations
game_state_set("Loc",L,GS,GS1) :-
	game_state_get("Map",GS,Map),
	replace_loc(L,Map,Map1),
	game_state_set("Map",Map1,GS,GS1).
% Encounter
game_state_set("Enemies",   E,GS,GS1) :- GS = (G,I,M,(_,D,R,A,C,X,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("EncDeck",   D,GS,GS1) :- GS = (G,I,M,(E,_,R,A,C,X,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("EncDiscard",R,GS,GS1) :- GS = (G,I,M,(E,D,_,A,C,X,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("AgendaDeck",A,GS,GS1) :- GS = (G,I,M,(E,D,R,_,C,X,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("ActDeck",   C,GS,GS1) :- GS = (G,I,M,(E,D,R,A,_,X,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("CurrAgenda",X,GS,GS1) :- GS = (G,I,M,(E,D,R,A,C,_,Y),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
game_state_set("CurrAct",   Y,GS,GS1) :- GS = (G,I,M,(E,D,R,A,C,X,_),S), GS1 = (G,I,M,(E,D,R,A,C,X,Y),S).
% multiple set
game_state_set([],[],GS,GS).
game_state_set([H|T],[X|L],GS,GS1) :- game_state_set(H,X,GS,GS2), game_state_set(T,L,GS2,GS1).
% list replace rules
replace_inv(I,[H|T],[I|T]) :- 
	inv_get("Id",I,Id), inv_get("Id",H,Id).
replace_inv(I,[H|T],[H|L1]) :-
	inv_get("Id",I,Id1), inv_get("Id",H,Id2), Id1 \= Id2,
	replace_inv(I,T,L1).
replace_loc(L,[H|T],[L|T]) :-
	loc_get("Id",L,Id), loc_get("Id",H,Id).
replace_loc(L,[H|T],[H|L1]) :-
	loc_get("Id",L,Id1), loc_get("Id",H,Id2), Id1 \= Id2,
	replace_loc(L,T,L1).

get_inv(Id,L,I) :-
	choose(L,I,_),
	inv_get("Id",I,Id),!.