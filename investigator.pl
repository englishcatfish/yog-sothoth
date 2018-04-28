% (InvId,Cards,Counters,Active)
	% Cards = (Deck,Hand,Assets,ThreatArea,Discard)
	% Counters = (Location,Actions,Damage,Horror,Resources,Clues)

inv_get("Id",    (N,_,_,_,_,_),N).
inv_get("Cards", (_,C,_,_,_,_),C).
inv_get("Status",(_,_,S,_,_,_),S).
inv_get("Skill", (_,_,_,S,_,_),S).
inv_get("Limits",(_,_,_,_,L,_),L).
inv_get("Active",(_,_,_,_,_,A),A).
% Cards
inv_get("Deck",   I,D) :- inv_get("Cards",I,(D,_,_,_,_)).
inv_get("Hand",   I,H) :- inv_get("Cards",I,(_,H,_,_,_)).
inv_get("Assets", I,A) :- inv_get("Cards",I,(_,_,A,_,_)).
inv_get("Threats",I,T) :- inv_get("Cards",I,(_,_,_,T,_)).
inv_get("Discard",I,D) :- inv_get("Cards",I,(_,_,_,_,D)).
% Status
inv_get("Loc",      I,L) :- inv_get("Status",I,(L,_,_,_,_,_,_)).
inv_get("Actions",  I,A) :- inv_get("Status",I,(_,A,_,_,_,_,_)).
inv_get("Damage",   I,D) :- inv_get("Status",I,(_,_,D,_,_,_,_)).
inv_get("Horror",   I,H) :- inv_get("Status",I,(_,_,_,H,_,_,_)).
inv_get("Resources",I,R) :- inv_get("Status",I,(_,_,_,_,R,_,_)).
inv_get("Clues",    I,C) :- inv_get("Status",I,(_,_,_,_,_,C,_)).
inv_get("Exhausted",I,E) :- inv_get("Status",I,(_,_,_,_,_,_,E)).
% Skill
inv_get("Will",   I,W) :- inv_get("Skill",I,(W,_,_,_,_,_)).
inv_get("Intel",  I,L) :- inv_get("Skill",I,(_,L,_,_,_,_)).
inv_get("Fight",  I,F) :- inv_get("Skill",I,(_,_,F,_,_,_)).
inv_get("Agility",I,A) :- inv_get("Skill",I,(_,_,_,A,_,_)).
inv_get("Health", I,H) :- inv_get("Skill",I,(_,_,_,_,H,_)).
inv_get("Sanity", I,S) :- inv_get("Skill",I,(_,_,_,_,_,S)).
% To do a "bulk" get
% e.g., inv_get(["Location","Actions","Clues"],I,[L,A,C])
inv_get([],_,[]).
inv_get([H|T],I,[X|L]) :- inv_get(H,I,X), inv_get(T,I,L).
inv_filter(String,Val,I) :- inv_get(String,I,Val).
% some of the set rules will not exist (e.g., for Id)
inv_set("Limits",L,(N,C,S,K,_,A),(N,C,S,K,L,A)).
inv_set("Active",A,(N,C,S,K,L,_),(N,C,S,K,L,A)).
% Set Cards
inv_set("Deck",   D,I,I1) :- I = (N,(_,H,A,T,S),U,K,L,C), I1 = (N,(D,H,A,T,S),U,K,L,C).
inv_set("Hand",   H,I,I1) :- I = (N,(D,_,A,T,S),U,K,L,C), I1 = (N,(D,H,A,T,S),U,K,L,C).
inv_set("Assets", A,I,I1) :- I = (N,(D,H,_,T,S),U,K,L,C), I1 = (N,(D,H,A,T,S),U,K,L,C).
inv_set("Threats",T,I,I1) :- I = (N,(D,H,A,_,S),U,K,L,C), I1 = (N,(D,H,A,T,S),U,K,L,C).
inv_set("Discard",S,I,I1) :- I = (N,(D,H,A,T,_),U,K,L,C), I1 = (N,(D,H,A,T,S),U,K,L,C).
% Set Status
inv_set("Loc",      L,I,I1) :- I = (N,C,(_,A,D,H,R,U,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Actions",  A,I,I1) :- I = (N,C,(L,_,D,H,R,U,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Damage",   D,I,I1) :- I = (N,C,(L,A,_,H,R,U,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Horror",   H,I,I1) :- I = (N,C,(L,A,D,_,R,U,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Resources",R,I,I1) :- I = (N,C,(L,A,D,H,_,U,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Clues",    U,I,I1) :- I = (N,C,(L,A,D,H,R,_,E),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
inv_set("Exhausted",E,I,I1) :- I = (N,C,(L,A,D,H,R,U,_),K,T,V), I1 = (N,C,(L,A,D,H,R,U,E),K,T,V).
% Set Skill
inv_set("Will",   W,I,I1) :- I = (N,C,S,(_,L,F,A,H,Y),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
inv_set("Intel",  L,I,I1) :- I = (N,C,S,(W,_,F,A,H,Y),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
inv_set("Fight",  F,I,I1) :- I = (N,C,S,(W,L,_,A,H,Y),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
inv_set("Agility",A,I,I1) :- I = (N,C,S,(W,L,F,_,H,Y),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
inv_set("Health", H,I,I1) :- I = (N,C,S,(W,L,F,A,_,Y),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
inv_set("Sanity", Y,I,I1) :- I = (N,C,S,(W,L,F,A,H,_),T,V), I1 = (N,C,S,(W,L,F,A,H,Y),T,V).
% To do a "bulk" set, similar to bulk get
inv_set([],[],I,I).
inv_set([H|T],[X|L],I,I1) :- inv_set(H,X,I,I2), inv_set(T,L,I2,I1).
inv_set_map(String,(I,V),I1) :- inv_set(String,V,I,I1).