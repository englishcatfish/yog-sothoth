:- discontiguous card/6.

% (CardId, Name, Unique, Kind, Types, Cost, Slot, CommitVals)
% Player Cards (Assets)
card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1006,
	Name = "Roland's .38 Special",
	Kind = "Asset",
	Types = ["Item","Weapon","Firearm"],
	Cost = 3,
	Slot = ("Hand",1),
	SkillPips = (0,0,1,1,1).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1008,
	Name = "Daisy's Tote Bag",
	Kind = "Asset",
	Types = ["Item"],
	Cost = 2,
	Slot = ("None",1),
	SkillPips = (1,1,0,0,1).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1009,
	Name = "The Necronomicon (John Dee Translation)",
	Kind = "Asset",
	Types = ["Item","Tome"],
	Cost = 0,
	Slot = ("Hand",1),
	SkillPips = (0,0,0,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1012,
	Name = "Heirloom of Hyperborea",
	Kind = "Asset",
	Types = ["Item","Relic"],
	Cost = 3,
	Slot = ("Accessory",1),
	SkillPips = (1,0,1,0,1).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1014,
	Name = "Wendy's Amulet",
	Kind = "Asset",
	Types = ["Item","Relic"],
	Cost = 2,
	Slot = ("Accessory",1),
	SkillPips = (0,0,0,0,2).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1016,
	Name = ".45 Automatic",
	Kind = "Asset",
	Types = ["Item","Weapon","Firearm"],
	Cost = 4,
	Slot = ("Hand",1),
	SkillPips = (0,0,0,1,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1017,
	Name = "Physical Training",
	Kind = "Asset",
	Types = ["Talent"],
	Cost = 2,
	Slot = ("None",1),
	SkillPips = (1,0,1,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1018,
	Name = "Beat Cop",
	Kind = "Asset",
	Types = ["Ally","Police"],
	Cost = 4,
	Slot = ("Ally",1),
	SkillPips = (0,0,1,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1019,
	Name = "First Aid",
	Kind = "Asset",
	Types = ["Talent","Science"],
	Cost = 2,
	Slot = ("None",1),
	SkillPips = (1,0,0,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1020,
	Name = "Machete",
	Kind = "Asset",
	Types = ["Item","Weapon","Melee"],
	Cost = 3,
	Slot = ("Hand",1),
	SkillPips = (0,0,1,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1021,
	Name = "Guard Dog",
	Kind = "Asset",
	Types = ["Ally","Creature"],
	Cost = 3,
	Slot = ("Ally",1),
	SkillPips = (0,0,1,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1027,
	Name = "Police Badge",
	Kind = "Asset",
	Types = ["Item"],
	Cost = 3,
	Slot = ("Accessory",1),
	SkillPips = (1,0,0,0,1).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1028,
	Name = "Beat Cop",
	Kind = "Asset",
	Types = ["Ally","Police"],
	Cost = 4,
	Slot = ("Ally",1),
	SkillPips = (0,0,1,1,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1029,
	Name = "Shotgun",
	Kind = "Asset",
	Types = ["Item","Weapon","Firearm"],
	Cost = 5,
	Slot = ("Hand",2),
	SkillPips = (0,0,2,0,0).

card(Id,Name,Kind,Types,Cost,Slot,SkillPips) :-
	Id = 1087,
	Name = "Flashlight",
	Kind = "Asset",
	Types = ["Item","Tool"],
	Cost = 2,
	Slot = ("Hand",1),
	SkillPips = (0,1,0,0,0).

% Events
card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1010,
	Name = "On the Lam",
	Kind = "Event",
	Types = ["Tactic"],
	Cost = 1,
	SkillPips = (0,1,0,1,2).

card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1013,
	Name = "Dark Memory",
	Kind = "Event",
	Types = ["Spell"],
	Cost = 2,
	SkillPips = (0,0,0,0,0).

card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1022,
	Name = "Evidence!",
	Kind = "Event",
	Types = ["Insight"],
	Cost = 1,
	SkillPips = (0,2,0,0,0).

card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1023,
	Name = "Dodge",
	Kind = "Event",
	Types = ["Tactic"],
	Cost = 1,
	SkillPips = (1,0,0,1,0).

card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1024,
	Name = "Dynamite Blast",
	Kind = "Event",
	Types = ["Tactic"],
	Cost = 5,
	SkillPips = (1,0,0,0,0).

card(Id,Name,Kind,Types,Cost,SkillPips) :-
	Id = 1026,
	Name = "Extra Ammunition",
	Kind = "Event",
	Types = ["Supply"],
	Cost = 2,
	SkillPips = (0,1,0,0,0).

% Skills

card(Id,Name,Kind,Types,SkillPips) :-
	Id = 1025,
	Name = "Vicious Blow",
	Kind = "Skill",
	Types = ["Practiced"],
	SkillPips = (0,0,1,0,0).

card(Id,Name,Kind,Types,SkillPips) :-
	Id = 1039,
	Name = "Deduction",
	Kind = "Skill",
	Types = ["Practiced"],
	SkillPips = (0,1,0,0,0).


% Treachery

card(Id,Name,Kind,Types) :-
	Id = 1007,
	Name = "Cover Up",
	Kind = "Treachery",
	Types = ["Task"].

card(Id,Name,Kind,Types) :-
	Id = 1011,
	Name = "Hospital Debts",
	Kind = "Treachery",
	Types = ["Task"].

card(Id,Name,Kind,Types) :-
	Id = 1015,
	Name = "Abandoned and Alone",
	Kind = "Treachery",
	Types = ["Madness"].

card(Id,Name,Kind,Types) :-
	Id = 1096,
	Name = "Amnesia",
	Kind = "Treachery",
	Types = ["Madness"].

card(Id,Name,Kind,Types) :-
	Id = 1097,
	Name = "Paranoia",
	Kind = "Treachery",
	Types = ["Madness"].

card(Id,Name,Kind,Types) :-
	Id = 1098,
	Name = "Haunted",
	Kind = "Treachery",
	Types = ["Curse"].

card(Id,Name,Kind,Types) :-
	Id = 1099,
	Name = "Psychosis",
	Kind = "Treachery",
	Types = ["Madness"].

card(Id,Name,Kind,Types) :-
	Id = 1100,
	Name = "Hypochondria",
	Kind = "Treachery",
	Types = ["Madness"].

% enemy card 
card(Id,Name,Kind,Types,Stats,_) :-
	Id = 1159,
	Name = "Swarm of Rats",
	Kind = "Enemy",
	Types = ["Creature"],
	Stats = (1,1,3,1,0).

card(Id,Name,Kind,Types,Stats,_) :-
	Id = 1160,
	Name = "Ghoul Minion",
	Kind = "Enemy",
	Types = ["Humanoid","Monster","Ghoul"],
	Stats = (2,2,2,1,1).

% get functions
card_get("Name",Id,N) :-
	card(Id,N,"Asset",_,_,_,_)
	; card(Id,N,"Event",_,_,_)
	; card(Id,N,"Skill",_,_)
	; card(Id,N,"Treachery",_)
	; card(Id,N,"Name",_,_,_).

card_get("Kind",Id,K) :-
	card(Id,_,K,_,_,_,_)
	; card(Id,_,K,_,_,_)
	; card(Id,_,K,_,_)
	; card(Id,_,K,_).

card_get("Types",Id,T) :-
	card(Id,_,_,T,_,_,_)
	; card(Id,_,_,T,_,_)
	; card(Id,_,_,T,_)
	; card(Id,_,_,T).

card_get("Cost",Id,C) :-
	card(Id,_,"Asset",_,C,_,_)
	; card(Id,_,"Event",_,C,_).

card_get("Slot",Id,S) :- 
	card(Id,_,"Asset",_,_,S,_).

card_get("SkillPips",Id,S) :-
	card(Id,_,"Asset",_,_,_,S)
	; card(Id,_,"Event",_,_,S)
	; card(Id,_,"Skill",_,S).

card_get([],_,[]).
card_get([H|T],I,[X|L]) :- 
	card_get(H,I,X), 
	card_get(T,I,L).

has_type(Type,Card) :-
	card_get("Types",Card,Types),
	choose(Types,Type,_).

card_filter(String,Value,Id) :- card_get(String,Id,Value).

discardable_from_hand(false,1013).

can_commit("Will",Card) :-
	card_get("SkillPips",Card,(N,_,_,_,M)),
	L is N + M, L @> 0.

can_commit("Intel",Card) :-
	card_get("SkillPips",Card,(_,N,_,_,M)),
	L is N + M, L @> 0.

can_commit("Fight",Card) :-
	card_get("SkillPips",Card,(_,_,N,_,M)),
	L is N + M, L @> 0.

can_commit("Agility",Card) :-
	card_get("SkillPips",Card,(_,_,_,N,M)),
	L is N + M, L @> 0.

active_skill_value(_,Card,1) :-
	choose([1017,1034,1049,1062,1077],Card,_).

active_skill_value(GS,1006,N) :-
	game_state_get("Current Investigator",GS,I),
	inv_get("Location",I,LId),
	game_state_get("Location",LId,GS,Location),
	loc_get("Clues",Location,Clues),
	((Clues @> 0, N = 3)
	; (Clues = 0, N = 1)).

committed_skill_value("Will",Card,V) :-
	card_get("SkillPips",Card,(N,_,_,_,M)),
	V is N+M.
committed_skill_value("Intel",Card,V) :-
	card_get("SkillPips",Card,(_,N,_,_,M)),
	V is N+M.
committed_skill_value("Fight",Card,V) :-
	card_get("SkillPips",Card,(_,_,N,_,M)),
	V is N+M.
committed_skill_value("Agility",Card,V) :-
	card_get("SkillPips",Card,(_,_,_,N,M)),
	V is N+M.

committed_skill_value(_,[],0).
committed_skill_value(Skill,[H|T],N) :-
	committed_skill_value(Skill,H,X),
	committed_skill_value(Skill,T,Y),
	N is X+Y.