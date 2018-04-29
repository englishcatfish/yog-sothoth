% TODO: When activating skill bonuses, they go in the Active list under ("SkillBonus",Val).

:- discontiguous single_step/3.

:- ensure_loaded(helper).
:- ensure_loaded(game_state).
:- ensure_loaded(investigator).
:- ensure_loaded(location).
:- ensure_loaded(card).
:- ensure_loaded(enemy).

step_n(0,GS,GS,[]).
step_n(N,GS,GSN,Actions) :-
	N @> 0, N1 is N - 1,
	single_step(GS,GS1,A1),
	step_n(N1,GS1,GSN,AN),
	Actions = [A1|AN].

step_n_atom(0,GS,GS,[]).
step_n_atom(N,GS,GSN,Actions) :-
	term_to_atom(GS_Term,GS),
	step_n(N,GS_Term,GSN_Term,Actions),
	term_to_atom(GSN_Term,GSN).

sample_gs_atom(GS) :-
	sample_gs("Action",GS_Term),
	term_to_atom(GS_Term,GS).

sample_gs("ST.2", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1039],[((1030,1),false,[]),((1033,1),false,[]),((1073,1),false,[])],[((1007,1),[("Clues",3)])],[1006]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[("SkillBonus",0)]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[1168])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",2,"Intel",2,ChaosBag),("Investigate",("Location",1111),false)],
	GS = (G,Is,Map,Enc,S).

sample_gs("ST.2a", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1039],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[("SkillBonus",0)]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[1168])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",2,"Intel",2,ChaosBag),("Investigate",("Location",1111),false)],
	GS = (G,Is,Map,Enc,S).

sample_gs("Action", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Easy"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([1087,1039],[1087,1039],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = ["Next Player"],
	GS = (G,Is,Map,Enc,S).

sample_gs("Action2", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Easy"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([1006,1007,1020],[1087,1039],[],[],[]),(1112,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1112,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1112,true,false,2,[]),(1113,false,false,0,[]),(1114,false,false,0,[]),(1115,false,true,0,[])],
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = ["Next Action"],
	GS = (G,Is,Map,Enc,S).

sample_gs("ST.3", GS) :-
	ChaosBag = ["+1","0","-1","-2","Skull","Cultist","Tablet","AutoFail","ElderSign"],
	G = ((1104,"Expert"),"I",[1001,1002],1001,1001,ChaosBag),
	I1 = (1001,([],[1087,1056],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	I2 = (1002,([],[1014,1087],[],[],[]),(1111,3,0,0,5,0,false),(3,3,3,3,6,6),[],[]),
	Is = [I1,I2],
	Map = [(1111,true,false,4,[])],
	%E1 = ((1160,1),(2,2,2,1,1),(1111,0,0,false,[])),
	Enc = ([],[],[],[],[],(0,[]),(0,[])),
	S = [("Skill Test",3,"Intel",2,ChaosBag,[1014,1087,1014],[(1001,[1087,1014]),(1002,[1014])]),("Investigate",1111)],
	GS = (G,Is,Map,Enc,S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PHASE: INVESTIGATION %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2.2.1 Investigator takes an action, if able.
%
% During their turn, an investigator is permitted to take three actions. An action can be used
% to do one of the following:
% - Investigate your location.
% - Move to a connecting location.
% - Draw (draw 1 card).
% - Resource (gain 1 resource).
% - Play an asset or event card from your hand.
% - Activate an =>-costed ability on:
%   - an in-play card you control.
%   - an in-play encounter card at your location.
%   - a card in your threat area.
%   - the current act card.
%   - the current agenda card.
% - Fight an enemy at your location.
% - Engage an enemy at your location.
% - Attempt to evade an enemy engaged with you.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Q: Is it always the case that the Stack will be 1 single element at this point?
% No more actions
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [["Next Action"], I]),
	inv_get("Actions", I, 0),
	game_state_set("Stack", ["End Turn"], GS_in, GS_out),
	Action = ["Turn finished"].

% Investigator may choose to end turn early
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [["Next Action"], I]),
	inv_get("Actions", I, N),
	N @> 0,
	inv_set("Actions", 0, I, I1),
	game_state_set(["Stack", "Inv"],[["End Turn"], I1], GS_in, GS_out),
	Action = ["End turn early"].

% Investigator turn finished
% TODO: process end of turn event limits
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in, ["End Turn"]),
	game_state_set("Stack", ["Next Player"], GS_in, GS_out),
	Action = ["Ending Turn"].

% choose next investigator
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "Invs"], GS_in, [["Next Player"], Is]),
	% choose investigator that still has actions left
	choose(Is, I, _),
	inv_get("Actions", I, N),
	N @> 0,
	inv_get("Id", I, Id),
	game_state_set(["CurrInvId", "Stack"], [Id, ["Begin Turn"]], GS_in, GS_out),
	string_builder(["Selected ", Id, " to go next."], Str),
	Action = [Str].

% Formalize begining of player turn
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in , ["Begin Turn"]),
	game_state_set("Stack", ["Next Action"], GS_in, GS_out),
	Action = ["Begin turn"].

% Investigate your location (Basic)
% condition: at least one clue at your location
% TODO: handle more complex scenarios like locations that use 
%       different skill type for investigation
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [["Next Action"], I]),
	inv_get("Actions", I, N),
	N @> 0,
	inv_get("Loc", I, LocId),
	game_state_get(("Loc", LocId), GS_in, Loc),
	loc_get("Clues", Loc, Clues),
	Clues @> 0,
	N1 is N - 1,
	game_state_get("ChaosBag", GS_in, ChaosBag),
	loc_get("Shroud", Loc, Shroud),
	inv_get("Active", I, Active),
	inv_set(["Actions", "Active"], [N1, [("SkillBonus", 0)|Active]], I, I1),
	H1 = ("Investigate", ("Location", LocId), false),
	H2 = ("Skill Test", 2, "Intel", Shroud, ChaosBag),
	game_state_set(["Stack", "Inv"],[[H2, H1, "End Action"], I1], GS_in, GS_out),
	string_builder(["Investigate ", LocId], Str),
	Action = [Str].

% Move to a connecting location
% - Choose an unlocked connecting location to move to
% - Will be split into two events: MoveOut, MoveIn
%   Contrary to the rules, which say this is "simulataneous", but it'll behave that way
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [["Next Action"], I]),
	inv_get("Actions", I, N),
	N @> 0,
	game_state_get("Map", GS_in, Map),
	inv_get("Loc", I, LocId),
	loc_get("Connection", LocId, Map, DstId),
	game_state_set("Stack", [("Move", "MoveOut", LocId, DstId)|"End Action"], GS_in, GS_out),
	string_builder(["Move from ", LocId, " to ", DstId],Str),
	Action = [Str].

% MoveOut:
% Most places you can simply move out of.
% TODO: handle locations like 1115 that force a test to leave
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in, [("Move", "MoveOut", SrcId, DstId)|Stack]),
	game_state_set("Stack", [("Move", "MoveIn", SrcId, DstId)|Stack], GS_in, GS_out),
	string_builder(["Moving out of ", SrcId], Str),
	Action = [Str].

% MoveIn:
% If location is not revealed, then reveal and add clues
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", ("Loc", DstId)], GS_in, [[("Move", "MoveIn" , _, DstId)|_], Loc]),
	loc_get("Revealed", Loc, false),
	game_state_get("NumInv", GS_in, N),
	location_get("Clues", N, DstId, Clues),
	loc_set(["Revealed", "Clues"],[true, Clues], Loc, Loc1),
	game_state_set("Loc", Loc1, GS_in, GS_out),
	string_builder(["Revealing location ", DstId],Str),
	Action = [Str].

% TODO: If there are enemies at the location you move to, they engage you immediately

% MoveIn:
% 1113, Attic
% -F->
% After you enter the Attic: Take 1 horror
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", ("Loc", 1113)], GS_in,[[("Move", "MoveIn", _, 1113)|Stack], Loc]),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", 1113, I, I1),
	game_state_set(["Stack", "Inv"],[[("Assign Affliction", 0, 1)|Stack], I1], GS_in, GS_out),
	Action = ["Move into 1113"].

% MoveIn:
% 1114, Cellar
% -F->
% After you enter the Cellar: Take 1 damage
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", ("Loc", 1114)], GS_in, [[("Move", "MoveIn", _, 1114)|Stack], Loc]),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", 1114, I, I1),
	game_state_set(["Stack", "Inv"],[[("Assign Affliction", 1, 0)|Stack], I1], GS_in, GS_out),
	Action = ["Move into 1114"].

% MoveIn:
% Most places you can simply move into.
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in, [("Move", "MoveIn", _, DstId)|Stack]),
	\+in(DstId, [1113, 1114]),
	game_state_get(("Loc", DstId), GS_in, Loc),
	loc_get("Revealed", Loc, true),
	game_state_get("CurrInv", GS_in, I),
	inv_set("Loc", DstId, I, I1),
	game_state_set(["Stack", "Inv"],[Stack, I1], GS_in, GS_out),
	string_builder(["Move into ", DstId], Str),
	Action = [Str].

% Assign Affliction
% Right now, just going to assign to investigator
% TODO: Choose how to distribute and what amounts to each card
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in, [("Assign Affliction", D, H)|Stack]),
	game_state_set("Stack", [("Apply Affliction", D, H, "CurrInv")|Stack], GS_in, GS_out),
	string_builder(["Assigned ", D, " damage and ", H, " horror to investigator"], Str),
	Action = [Str].

% TODO: What if the investigator faints?
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack","CurrInv"], GS_in, [[("Apply Affliction", Dmg, Hrr, "CurrInv")|Stack], I]),
	inv_get(["Health", "Sanity"], I, [Health, Sanity]),
	Health1 is Health - Dmg,
	Sanity1 is Sanity - Hrr,
	inv_set(["Health", "Sanity"], [Health1, Sanity1], I, I1),
	game_state_set(["Stack", "Inv"], [Stack, I1], GS_in, GS_out),
	Action = ["Applying affliction to investigator"].

% Draw 1 card
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [["Next Action"], I]),
	inv_get(["Actions", "Deck"], I, [N, Deck]),
	N @> 0,
	N1 is N - 1,
	choose(Deck, Card, Deck1),
	inv_set(["Actions", "Deck"], [N1, Deck1], I, I1),
	game_state_set(["Stack", "Inv"], [[("Reveal Card",Card), "End Action"], I1], GS_in, GS_out),
	string_builder(["Drew card ", Card], Str),
	Action = [Str].

% Reveal Card
% TODO: Process revelation effects
single_step(GS_in, GS_out, Action) :-
	game_state_get(["Stack", "CurrInv"], GS_in, [[("Reveal Card", Card)|Stack], I]),
	inv_get("Hand", I, Hand),
	insert_card(Card, Hand, Hand1),
	inv_set("Hand", Hand1, I, I1),
	game_state_set(["Stack", "Inv"], [Stack, I1], GS_in, GS_out),
	Action = ["Adding Card to hand"].

% TODO: Play a card
% TODO: Activate an ==> costed ability
% TODO: Fight enemy at location
% TODO: Evade enemy at location
% TODO: Engaged enemy at location
% TODO: AoE

% TODO: process end action properly
single_step(GS_in, GS_out, Action) :-
	game_state_get("Stack", GS_in, ["End Action"|Stack]),
	game_state_set("Stack", ["Next Action"|Stack], GS_in, GS_out),
	Action = ["Ending action"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.2: Commit cards from hand to skill test.
% 
% The investigator performing the skill test may commit any number of cards with an appropriate
% skill icon from his or her hand to this test. Each other investigator at the same location as
% the investigator performing the skill test may commit one card with an appropriate skill icon
% to this test. An appropriate skill icon is either one that matches the skill being tested, or
% a wild icon.  The investigator performing this test gets +1 to his or her skill value during
% this test for each appropriate skill icon that is committed to this test. Cards that lack an
% appropriate skill icon may not be committed to a skill test. Do not pay a card's resource cost
% when committing it.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get("Stack", GS, [Head | Stack]),
	Head = ("Skill Test", 2, Skill, Difficulty, ChaosBag),!,
	% 1. get investigator cards
	game_state_get("CurrInv", GS, I),
	inv_get(["Id","Loc","Hand"], I, [Id,LId,H]),
	% 2. choose any combination of cards
	choose_any(H, Commit, H1),
	exclude(can_commit(Skill), Commit, []),
	inv_set("Hand", H1, I, I1),
	% 3. get list of lists of appropriate skill/commit cards from other players
	game_state_get("Invs", GS, Is),
	% all players except me, and at my location
	exclude(inv_filter("Id",Id), Is, Is1),
	include(inv_filter("Loc",LId), Is1, Others),
	maplist(inv_get("Id"), Others, OIds),
	maplist(inv_get("Hand"), Others, OHands),
	% 4. choose 0 or 1 card(s) from each other player
	maplist(choose_max_map(1), OHands, OCommitNotCommit),
	pair_lists(OCommit, ONotCommit, OCommitNotCommit),
	% make sure they're all applicable
	maplist(exclude(can_commit(Skill)), OCommit, Empty), flatten(Empty,[]),
	% 5. update investigator hands
	pair_lists(Others, ONotCommit, OHands1),
	maplist(inv_set_map("Hand"), OHands1, Others1),
	game_state_set("SubInvs", Others1, GS, GS1),
	game_state_set("Inv", I1, GS1, GS2),
	% 6. commit chosen
	flatten(OCommit, OCommitFlat),
	append(OCommitFlat, Commit, AllCommit),
	pair_lists(OIds, OCommit, OCommitted),
	PsCommit = [(Id,Commit) | OCommitted],
	% 7. Push next step
	Head1 = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	game_state_set("Stack", [Head1 | Stack], GS2, GSN),
	term_string(PsCommit, S1),
	string_concat("Commit cards to skill test: ", S1, S),
	Action = [S].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.3: Reveal chaos token
%
% The investigator performing the skill test reveals one chaos token at random from the chaos 
% bag.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get("Stack", GS, [Head|Stack]),
	Head = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	% 1. Reveal Chaos Token
	choose(ChaosBag, Token, ChaosBag1),
	% 2. Push next token
	Head1 = ("Skill Test", 4, Skill, Difficulty, ChaosBag1, AllCommit, PsCommit, Token),
	game_state_set("Stack", [Head1|Stack], GS, GSN),
	string_concat("Drew Token: ", Token, Str),
	Action = [Str].

% 1005, Wendy Adams
% -r->
% When you reveal a chaos token, choose and discard 1 card from your hand:
% Cancel that chaos token and return it to the bag. Reveal a new chaos token.
% (Limit once per test/ability)

single_step(GS, GSN, Action) :-
	game_state_get("Stack", GS, [Head|_]),
	Head = ("Skill Test", 3,_,_,ChaosBag,_,_),
	% Reveal Chaos Token
	choose(ChaosBag, Token, _),
	% You are Wendy Adams
	game_state_get("CurrInv", GS, I),
	inv_get(["Id", "Hand", "Discard", "Limits"], I, [1005, H, D, L]),
	% haven't used this reaction yet this test
	include(eq(("Test",1005)),L,[]),
	% discard 1 card
	choose(H,C,H1),
	inv_set(["Hand", "Discard", "Limits"], [H1, [C|D], [("Test",1005)|L]], I, I1),
	game_state_set("Inv", I1, GS, GSN),
	term_string(C,CardStr),
	string_builder(["Drew Token: ",Token,", -r-> Wendy Adams (Discard ",CardStr,"). Reveal new chaos token"],Str),
	Action = [Str].

% TODO
% 1071, Grotesque Statue
% -r->
% When you would reveal a chaos token, spend 1 charge:
% Reveal 2 chaos tokens instead of 1.  Choose 1 of those tokens to resolve, and ignore the other.

% 1009, The Necronomicon: John Dee Translation
% -F->
% Treat each ElderSign you reveal on a chaos token as an AutoFail
% REDCUT

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"], GS, [[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get("Threats",I,Threats),
	choose(Threats,((1009,_),_),_),!,
	Head1 = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail"),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["The Necronomicon: ElderSign -F-> AutoFail"].

% 1060, Shrivelling
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this attack, 
% take 1 horror
% REDCUT

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"], GS, [[Head|Stack],I]),
	Head = ("Skill Test", 4,_,_,_,_,_, Token),
	choose(["Skull","Cultist","Tablet","ElderThing","AutoFail"], Token, _),
	inv_get(["Active","Limits"],I,[Active,Limits]),
	choose(Active,1060,_),
	\+choose(Limits,("Action",1060),_),!,
	inv_set("Limits",[("Action",1060)|Limits],I,I1),
	game_state_set(["Stack","Inv"],[[("Assign Affliction",0,1),Head|Stack],I1],GS,GSN),
	Action = ["Shrivelling -F-> take 1 horror"].

% 1066, Blinding Light
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this evasion 
% attempt, lose 1 action this turn.
% REDCUT

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"], GS, [[Head|_],I]),
	Head = ("Skill Test", 4,_,_,_,_,_, Token),
	choose(["Skull","Cultist","Tablet","ElderThing","AutoFail"], Token, _),
	inv_get(["Active","Limits","Resources"],I,[Active,Limits,R]),
	choose(Active,1066,_),
	\+choose(Limits,("Action",1066),_),!,
	R1 is R - 1, max(R1,0,R2),
	inv_set(["Limits","Resources"],[[("Action",1066)|Limits],R2],I,I1),
	game_state_set("Inv",I1,GS,GSN),
	Action = ["Blinding Light -F-> lose 1 action"].

% 1069, Blinding Light
% -F->
% If a Skull, Cultist, Tablet, ElderThing, or AutoFail symbol is revealed during this evasion 
% attempt, lose 1 action this turn and take 1 horror.
% REDCUT

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"], GS, [[Head|Stack],I]),
	Head = ("Skill Test", 4,_,_,_,_,_, Token),
	choose(["Skull","Cultist","Tablet","ElderThing","AutoFail"], Token, _),
	inv_get(["Active","Limits","Resources"],I,[Active,Limits,R]),
	choose(Active,1069,_),
	\+choose(Limits,("Action",1069),_),!,
	R1 is R - 1, max(R1,0,R2),
	inv_set(["Limits","Resources"],[[("Action",1069)|Limits],R2],I,I1),
	game_state_set(["Stack","Inv"],[[("Assign Affliction",0,1),Head|Stack],I1],GS,GSN),
	Action = ["Blinding Light -F-> lose 1 action and take 1 horror"].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.4: Apply chaos symbol effect(s)
%
% Apply any effects initiated by the symbol on the revealed chaos token. Each of the following
% symbols indicates that an ability on the scenario reference card must initiate:
% - Skull
% - Cultist
% - Tablet
% - ElderThing
%
% The ElderSign symbol indicates that the ElderSign ability on the investigator card belonging
% to the player performing the test must initiate.
%
% If none of the above symbols are revealed, or if the icon has no corresponding ability, this
% step completes with no effect
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% token drawn has no corresponding ability (i.e, just a numbered token)
single_step(GS, GSN, Action) :-
	game_state_get("Stack", GS, [Head|Stack]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token),
	choose(["+1","0","-1","-2","AutoFail"],Token,_),
	token_value(Token,TokenVal),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	game_state_set("Stack", [Head1|Stack], GS,GSN),
	string_builder(["Token: ",Token," has no additional effect."],Str),
	Action = [Str].

% ElderSign effects

% Roland Banks (1001): +1 for each clue on your location
% Daisy Walker (1002): +0. If you succeed, draw 1 card for each Tome you control
% "Skids" O'Toole (1003): +2. If you succeed, gain 2 resources
% Agnes Baker (1004): +1 for each horror on Agnes Baker
% Wendy Adams (1005): +0. If Wendy's Amulet is in play, you automatically succeed instead

% TODO: handle 1103

%%%%%%%%%%%%%%%%%%%%%%%
% Roland Banks (1001) %
%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get(["Id","Loc"],I,[1001,LocId]),
	game_state_get("Loc",LocId,GS,Loc),
	loc_get("Clues",Loc,Clues),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", Clues),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	string_builder(["Token: ElderSign has the value ",Clues],Str),
	Action = [Str].
%%%%%%%%%%%%%%%%%%%%%%%
% Dasiy Walker (1002) %
%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get(["Id","Active"],I,[1002,Active]),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 0),
	inv_set("Active",[("SkillTest","ElderSign")|Active],I,I1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1],GS,GSN),
	Action = ["Token: ElderSign has the value +0"].
%%%%%%%%%%%%%%%%%%%%%%%%%%
% "Skids" O'Toole (1003) %
%%%%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get(["Id","Active"],I,[1003,Active]),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 2),
	inv_set("Active",[("SkillTest","ElderSign")|Active],I,I1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1],GS,GSN),
	Action = ["Token: ElderSign has the value +2"].
%%%%%%%%%%%%%%%%%%%%%%
% Agnes Baker (1004) %
%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get(["Id","Horror"],I,[1004,Horror]),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", Horror),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	string_builder(["Token: ElderSign has the value ",Horror],Str),
	Action = [Str].
%%%%%%%%%%%%%%%%%%%%%%
% Wendy Adams (1005) %
%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign"),
	inv_get(["Id","Assets"],I,[1005,Assets]),
	((choose(Assets,((1104,_),_,_),_),
		Head1 = ("Skill Test", 5, Skill, 0, ChaosBag, AllCommit, PsCommit, "ElderSign", 0))
	;(\+ choose(Assets,((1104,_),_,_),_),
		Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "ElderSign", 0))),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Token: ElderSign has the value +0"].

% Scenario: The Gathering
%   Difficulty: Easy/Standard
%   -   Skull: -X. X is the number of Ghoul enemies at your location.
%   - Cultist: -1. If you fail, take 1 horror.
%   -  Tablet: -2. If there is a Ghoul enemy at your location, take 1 damage
%   Difficulty: Hard/Expert
%   -   Skull: -2. If you fail, after this skill test, search the encounter deck and discard
%                  pile for a Ghoul enemy, and draw it. Shuffle the encounter deck.
%   - Cultist: Reveal another token. If you fail, take 2 horror.
%   -  Tablet: -4. If there is a Ghoul enemy at your location, take 1 damage and 1 horror.

%%%%%%%%%%%%%%%%%%%%%%%%
% Easy/Standard: Skull %
%%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get("Scenario",GS,(1104,Diff)),
	(Diff = "Easy"; Diff = "Standard"),
	game_state_get("Stack", GS, [Head|Stack]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull"),
	% count number of ghoul enemies at my location
	game_state_get(["CurrInv","Enemies"],GS,[I,Es]),
	inv_get("Loc",I,LocId),
	include(enemy_filter("Loc",LocId),Es,Es1),
	include(enemy_filter("Type","Ghoul"),Es1,Es2),
	length(Es2,N),
	X is -1 * N,
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull", X),
	game_state_set("Stack", [Head1|Stack], GS,GSN),
	string_builder(["Token: Skull has the value: ",X],Str),
	Action = [Str].
%%%%%%%%%%%%%%%%%%%%%%%%%%
% Easy/Standard: Cultist %
%%%%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Scenario", "Stack", "CurrInv"],GS,[(1104,Diff),[Head|Stack],I]),
	(Diff = "Easy"; Diff = "Standard"),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist"),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist", -1),
	inv_get("Active",I,Active),
	inv_set("Active",[("Cultist",(1104,Diff))|Active],I,I1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1], GS,GSN),
	Action = ["Token: Cultist has value -1"].
%%%%%%%%%%%%%%%%%%%%%%%%%
% Easy/Standard: Tablet %
%%%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get("Scenario",GS,(1104,Diff)),
	(Diff = "Easy"; Diff = "Standard"),
	game_state_get("Stack", GS, [Head|Stack]),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet"),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet", -2),
	% count number of ghoul enemies at my location
	game_state_get(["CurrInv","Enemies"],GS,[I,Es]),
	inv_get("Loc",I,LocId),
	include(enemy_filter("Loc",LocId),Es,Es1),
	include(enemy_filter("Type","Ghoul"),Es1,Es2),
	length(Es2,N),
	((N = 0, 
		game_state_set("Stack", [Head1|Stack], GS,GSN)), 
	    Str = ""
	;(N @> 0,
		game_state_set("Stack", [("Assign Affliction",1,0),Head1|Stack], GS,GSN),
		Str = " Also take 1 damage.")),
	string_builder(["Token: Tablet has value -2.",Str],Str1),
	Action = [Str1].
%%%%%%%%%%%%%%%%%%%%%%
% Hard/Expert: Skull %
%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Scenario","Stack","CurrInv"],GS,[(1104,Diff),[Head|Stack],I]),
	(Diff = "Hard"; Diff = "Expert"),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull"),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Skull", -2),
	inv_get("Active",I,Active),
	inv_set("Active",[("Skull",(1104,Diff))|Active],I,I1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1], GS,GSN),
	Action = ["Token: Skull has value -2"].
%%%%%%%%%%%%%%%%%%%%%%%%
% Hard/Expert: Cultist %
%%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Scenario","Stack","CurrInv"],GS,[(1104,Diff),[Head|Stack],I]),
	(Diff = "Hard"; Diff = "Expert"),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Cultist"),
	Head1 = ("Skill Test", 3, Skill, Difficulty, ChaosBag, AllCommit, PsCommit),
	inv_get("Active",I,Active),
	inv_set("Active",[("Cultist",(1104,Diff))|Active],I,I1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1], GS,GSN),
	Action = ["Token: Cultist --> reveal another token"].
%%%%%%%%%%%%%%%%%%%%%%%
% Hard/Expert: Tablet %
%%%%%%%%%%%%%%%%%%%%%%%
single_step(GS, GSN, Action) :-
	game_state_get(["Scenario","Stack","CurrInv","Enemies"],GS,[(1104,Diff),[Head|Stack],I,Es]),
	(Diff = "Hard"; Diff = "Expert"),
	Head = ("Skill Test", 4, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet"),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "Tablet", -4),
	% count number of ghoul enemies at my location
	inv_get("Loc",I,LocId),
	include(enemy_filter("Loc",LocId),Es,Es1),
	include(enemy_filter("Type","Ghoul"),Es1,Es2),
	length(Es2,N),
	((N = 0, 
		game_state_set("Stack", [Head1|Stack], GS,GSN)), 
	    Str = ""
	;(N @> 0,
		game_state_set("Stack", [("Assign Affliction",1,1),Head1|Stack], GS,GSN),
		Str = " Also take 1 damage and 1 horror.")),
	string_builder(["Token: Tablet has value -4.",Str],Str1),
	Action = [Str1].

% TODO: anytime a card is played, need to check if it is allowed: 1165
% 1056, Sure Gamble
% -p->
% Play after you reveal a chaos token with a negative modifier
% Switch that token's "-" to a "+"
single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	TokenVal @< 0,
	inv_get(["Resources","Hand","Discard"],I,[R,H,D]),
	choose(H,1056,H1),
	TokenVal1 is TokenVal * -1,
	R @>= 2, R1 is R-2,
	inv_set(["Resources","Hand","Discard"], [R1, H1, [1056|D]], I, I1),
	Head1 = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1],GS,GSN),
	string_builder(["Fast. Play Sure Gamble. Token Value now: ",TokenVal1],Str),
	Action = [Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.5: Determine investigator's modified skill value.
%
% Start with the base skill (of the skill that matches the type of test that is resolving) of 
% the investigator performing this test, and apply all active modifiers, including the
% appropriate icons that have been committed to this test, effects of the chaos token(s)
% revealed, and all active card abilities that are modifying the investigator's skill value.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get(["Stack", "CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, TokenVal),
	Token \= "AutoFail",
	% 1. Base Skill Value
	inv_get([Skill, "Active"],I,[BaseVal,Active]),
	% 2. Committed Value
	committed_skill_value(Skill,AllCommit,CommitVal),
	% 3. Active Card Abilities Value
	choose(Active,("SkillBonus",ActiveVal),Active1),
	inv_set("Active",Active1,I,I1),
	% 4. Passive Card Abilities Value
	passive_skill_value(Skill,GS,PassVal),
	% Look for 'external' contributors: 1098, 1117, 
	ModifiedValue is BaseVal + CommitVal + ActiveVal + PassVal + TokenVal,
	Head1 = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedValue),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1],GS,GSN),
	string_builder(["Modified Skill Value: ",ModifiedValue],Str),
	Action = [Str].

single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 5, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", _),
	Head1 = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", 0),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Modified Skill Value: 0"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.6: Determine success/failure of skill test.
%
% Compare the investigator's modified skill value to the difficulty of the skill test.  If the
% investigator's skill value equals or exceeds the difficulty for this test (as indicated by 
% the card or game mechanic invoking the test), the investigator succeeds at the test.
% - If an investigator automatically succeeds at a test via a card ability, the total difficulty
%   of that test is considered 0
% If the investigator's skill value is less than the difficulty for this test, the investigator
% fails at the test.
% - If an investigator automatically fails at a test via a card ability or revealing the 
%   AutoFail symbol, their total skill value for that test is considered 0.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	Token \= "AutoFail",
	ModifiedVal @>= Difficulty,
	SumVal is ModifiedVal - Difficulty,
	Head1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Success", SumVal),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Success"].

single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	Token \= "AutoFail",
	ModifiedVal @< Difficulty,
	SumVal is ModifiedVal - Difficulty,
	Head1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Fail", SumVal),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Fail"].

% AutoFail always fails
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, "AutoFail", 0),
	SumVal is 0 - Difficulty,
	Head1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, "AutoFail", "Fail", SumVal),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Fail"].
	
% 1080, Lucky!
% -p->
% Play when you would fail a skill test.  Get +2 to your skill value for that test.
% Note: if AutoFail was drawn, then you don't get +2

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	ModifiedVal @< Difficulty,
	% play Lucky!
	inv_get(["Hand","Resources","Discard"],I,[Hand,R,Discard]),
	choose(Hand,1080,Hand1),
	R @>= 1, R1 is R - 1,
	inv_set(["Hand","Resources","Discard"],[Hand1,R1,[1080|Discard]],I,I1),
	((Token \= "AutoFail",
		ModifiedVal1 is 2 + ModifiedVal)
	;(Token = "AutoFail",
		ModifiedVal1 = ModifiedVal)),
	Head1 = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal1),
	game_state_set(["Stack","Inv"],[[Head1|Stack],I1],GS,GSN),
	Action = ["Fast. Play Lucky!"].

% 1084, Lucky!
% -p->
% Play when you would fail a skill test.  
%   Get +2 to your skill value for that test.
%   Draw 1 card.
% Note: if AutoFail was drawn, then you don't get +2

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal),
	ModifiedVal @< Difficulty,
	% play Lucky!
	inv_get(["Hand","Resources","Discard"],I,[Hand,R,Discard]),
	choose(Hand,1084,Hand1),
	R @>= 1, R1 is R - 1,
	inv_set(["Hand","Resources","Discard"],[Hand1,R1,[1084|Discard]],I,I1),
	((Token \= "AutoFail",
		ModifiedVal1 is 2 + ModifiedVal)
	;(Token = "AutoFail",
		ModifiedVal1 = ModifiedVal)),
	Head1 = ("Skill Test", 6, Skill, Difficulty, ChaosBag, AllCommit, PsCommit, Token, ModifiedVal1),
	game_state_set(["Stack","Inv"],[[("Draw Card"),Head1|Stack],I1],GS,GSN),
	Action = ["Fast. Play Lucky!"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.7: Apply skill test results
%
% The card ability or game rule that initiated a skill test usually indicates the consequences
% of success and/or failure for that test. Additionally, some other card abilities may 
% contribute additional consequences, or modify existing consequences, at this time. Resolve
% the appropriate consequences (based on the success or failure established during ST.6) at this
% time.  If there are multiple results to be applied during this step, the investigator 
% performing the test applies those results in the order of his or her choice.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Investigate Location, Success
% Discover clues at your location
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head,Head1|Stack]),
	Head = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Success", SumVal),
	Head1 = ("Investigate",("Location",LocId),false),
	Head2 = ("Investigate",("Location",LocId),true),
	bonus_clues_total(AllCommit,N,AllCommit1),
	Head3 = ("Skill Test", 7, Skill, ChaosBag, AllCommit1, PsCommit, Token, "Success", SumVal),
	NClues is 1 + N,
	Head4 = ("Discover Clues",LocId,NClues),
	game_state_set("Stack",[Head4,Head3,Head2|Stack],GS,GSN),
	string_builder(["Discover ",NClues," clue(s)."], Str),
	Action = [Str].

% Investigate Location, Fail
% Don't discover any clues
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head,Head1|Stack]),
	Head = ("Skill Test", 7, _,_,_,_,_, "Fail",_),
	Head1 = ("Investigate",("Location",LocId),false),
	Head2 = ("Investigate",("Location",LocId),true),
	game_state_set("Stack",[Head,Head2|Stack],GS,GSN),
	Action = ["Didn't discover any clues."].

% Investigate Location
% Finish applying skill test results
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head,Head1|Stack]),
	Head = ("Skill Test",7,_,_,[],PsCommit,_,_,_),
	Head1 = ("Investigate",_,true),
	Head2 = ("Skill Test",8,PsCommit),
	game_state_set("Stack",[Head2|Stack],GS,GSN),
	Action = ["Done applying results"].

% 1033, Dr. Milan Christopher
% -r->
% After you successfully investigate: Gain 1 resouce
single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"],GS,[[Head,Head1|Stack],I]),
	Head = ("Skill Test", 7, _,_,_,_,_, "Success",_),
	Head1 = ("Investigate",_,_),
	inv_get(["Assets","Limits"],I,[Assets,Limits]),
	choose(Assets,((1033,CId),_,_),_),
	\+choose(Limits,("Action",(1033,CId)),_),
	inv_set("Limits",[("Action",(1033,CId))|Limits],I,I1),
	game_state_set(["Stack","Inv"],[[("Gain Resources",1),Head,Head1|Stack],I1],GS,GSN),
	Action  = ["-r-> Dr. Milan Christopher."].

% 1045, Burglary
% Investigate, if you succeed, instead of discovering clues, gain 3 resources.
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head,Head1|Stack]),
	Head = ("Skill Test", 7, _,_,_,_,_, "Success",_),
	Head1 = ("Investigate",("Special",1045),false),
	Head2 = ("Investigate",("Special",1045),true),
	Head3 = ("Gain Resources",3),
	game_state_set("Stack",[Head3,Head,Head2|Stack],GS,GSN),
	string_builder(["Gain 3 resources."], Str),
	Action = [Str].

% 1073, Scavenging
% -r->
% After you successfully investigate by 2 or more,
% exhaust Scavenging:
% Choose an Item card in your discard pile and add it to your hand
single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"],GS,[[Head,Head1|_],I]),
	Head = ("Skill Test", 7, _,_,_,_,_, "Success",N),
	Head1 = ("Investigate",_,_),
	N @>= 2,
	inv_get(["Assets","Discard","Hand"],I,[Assets,Discard,Hand]),
	choose(Assets,((1073,UId),false,T),Assets1),
	choose(Discard,Item,Discard1),
	has_type("Item",Item),
	inv_set(["Assets","Discard","Hand"],[[((1073,UId),true,T)|Assets1],Discard1,[Item|Hand]],I,I1),
	game_state_set("Inv",I1,GS,GSN),
	card_get("Name",Item,IName),
	string_builder(["-r-> Scavenging, adding ",IName," to hand."],Str),
	Action = [Str].

% 1168, Obscuring Fog
% -F->
% After attached location is successfully investigated:
% Discard Obscuring Fog
single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv","EncDiscard"],GS,[[Head,Head1|_],I,EDiscard]),
	Head = ("Skill Test", 7, _,_,_,_,_, "Success",_),
	Head1 = ("Investigate",_,_),
	inv_get("Loc",I,LocId),
	game_state_get("Loc",LocId,GS,Loc),
	loc_get("Attached",Loc,Attached),
	choose(Attached,1168,Attached1),
	loc_set("Attached",Attached1,Loc,Loc1),
	game_state_set(["Loc","EncDiscard"],[Loc1,[1168|EDiscard]],GS,GSN),
	Action = ["-F-> Discard Obscuring Fog."].

%%%%%%%% Skill Card effects
% This is only for things like, draw 1 card, and not for bonuses that increase the impact
% of the skill test initiating action.

% Skip non-skill cards
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, PassFail, SumVal),
	choose(AllCommit,C,AllCommit1),
	\+card_get("Kind",C,"Skill"),
	Head1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit1, PsCommit, Token, PassFail, SumVal),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	string_builder([C," is not a skill card, continue."],Str),
	Action = [Str].

% 1039, Deduction
% Fail skill test, it has no effect.
single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 7, Skill, ChaosBag, AllCommit, PsCommit, Token, "Fail", SumVal),
	choose(AllCommit,1039,AllCommit1),
	Head1 = ("Skill Test", 7, Skill, ChaosBag, AllCommit1, PsCommit, Token, "Fail", SumVal),
	game_state_set("Stack",[Head1|Stack],GS,GSN),
	Action = ["Failed skill test, discard 1039"].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% SKILL TEST %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% ST.8 Skill test ends.
%
% This step formalizes the end of this skill test. Discard all cards that were committed to this
% skill test, and return all revealed chaos tokens to the chaos bag.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get("Stack",GS,[Head|Stack]),
	Head = ("Skill Test", 8, PsCommit),
	foldl(discard_committed,PsCommit,GS,GS1),
	game_state_set("Stack",Stack,GS1,GSN),
	Action = ["Skill test ends."].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Discover Clues %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% If an investigator discovers a clue, they take the clue from the location and place it on
% their investigator card, under their control.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv",("Loc",LocId)],GS,[[Head|Stack],I,Loc]),
	Head = ("Discover Clues",LocId,NClues),
	loc_get("Clues",Loc,LClues),
	inv_get("Clues",I,IClues),
	min(NClues,LClues,Clues),
	LClues1 is LClues - Clues,
	IClues1 is IClues + Clues,
	loc_set("Clues",LClues1,Loc,Loc1),
	inv_set("Clues",IClues1,I,I1),
	game_state_set(["Stack","Inv","Loc"],[Stack,I1,Loc1],GS,GSN),
	string_builder(["Gained ",Clues," clue(s)."],Str),
	Action = [Str].

% 1007, Cover Up
% -r->
% When you would discover 1 or more clues at your location:
% Discard that many clues from Cover Up instead.
% Only really need to do this if there are 1 or more clues

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv",("Loc",LocId)],GS,[[Head|Stack],I,Loc]),
	Head = ("Discover Clues",LocId,NClues),
	loc_get("Clues",Loc,LClues),
	min(NClues,LClues,Clues),
	Clues @> 0,
	inv_get("Threats",I,Threats),
	choose(Threats,((1007,UId),Tokens),Threats1),
	choose(Tokens,("Clues",TClues),Tokens1),
	TClues @> 0,
	min(Clues,TClues,RClues),
	TClues1 is TClues - RClues,
	inv_set("Threats",[((1007,UId),[("Clues",TClues1)|Tokens1])|Threats1],I,I1),
	game_state_set(["Stack","Inv"],[Stack,I1],GS,GSN),
	string_builder(["Discarded ",Clues," clue(s) from Cover Up."],Str),
	Action = [Str].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Gain Resource %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% An investigator gains a number of resources
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

single_step(GS, GSN, Action) :-
	game_state_get(["Stack","CurrInv"],GS,[[Head|Stack],I]),
	Head = ("Gain Resources",N),
	inv_get("Resources",I,R),
	R1 is R + N,
	inv_set("Resources",R1,I,I1),
	game_state_set(["Stack","Inv"],[Stack,I1],GS,GSN),
	string_builder(["Gained ",N," resource(s)."],Str),
	Action = [Str].

% 1039, Deduction
% If this skill test is successful while investigating a location,
% discover 1 additional clue at that location.
bonus_clues(1039,1).
bonus_clues_sum(C,(N,L),(N1,L)) :-
	bonus_clues(C,M),
	N1 is N + M.
bonus_clues_sum(C,(N,L),(N,[C|L])) :-
	\+bonus_clues(C,_).
bonus_clues_total(L,N,L1) :- foldl(bonus_clues_sum,L,(0,[]),(N,L1)).

run_test(Test,Action) :-
	sample_gs(Test,GS),
	single_step(GS,_,Action).

run_test(N,Test,Actions) :-
	sample_gs(Test,GS),
	step_n(N,GS,_,Actions).

eq(X,X).

token_value("+1",1).
token_value("0",0).
token_value("-1",-1).
token_value("-2",-2).
token_value("AutoFail",0).

passive_skill("Fight",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1018,_),_,_),_).
passive_skill("Will",   GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1027,_),_,_),_).
passive_skill("Fight",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1028,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1030,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1033,_),_,_),_).
passive_skill("Intel",  GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1040,_),_,_),_).
passive_skill("Agility",GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1055,_),_,_),_).
passive_skill("Will",   GS,1) :- game_state_get("CurrInv",GS,I), inv_get("Assets",I,Assets), choose(Assets,((1059,_),_,_),_).
passive_skill("Fight",  GS,1) :- 
	% if an investigator at my location (including me) controls Lita Chantler
	game_state_get(["CurrInv","Invs"],GS,[I,Is]),
	inv_get("Loc",I,LId),
	include(inv_filter("Loc",LId),Is,Is1),
	maplist(inv_get("Assets"),Is1,AllAssets),
	flatten(AllAssets,FlatAssets),
	choose(FlatAssets,((1117,_),_,_),_).

passive_skill(_,GS,-1) :- game_state_get("CurrInv",GS,I), inv_get("Threats",I,Threats), choose(Threats,((1098,_),_),_).
passive_skill(_,_,0).

passive_skill_value(Skill,GS,V) :- aggregate(sum(X), passive_skill(Skill,GS,X), V).

stuck_states(N,State,GS1) :- 
	sample_gs(State,GS), 
	step_n(N,GS,GS1,_), 
	\+game_state_get("Stack",GS1,[]),
	\+step_n(1,GS1,_,_).
stuck_state(State,GS) :-
	sample_gs(State,G),
	step_all(G,GS,_),
	\+game_state_get("Stack",GS,[]).

discard_committed((InvId,Cards),GS,GSN) :-
	game_state_get("Inv",InvId,GS,I),
	inv_get("Discard",I,D),
	append(Cards,D,D1),
	inv_set("Discard",D1,I,I1),
	game_state_set("Inv",I1,GS,GSN).

insert_card(Card,[],[Card]).
insert_card(Card,[X|L],[Card,X|L]) :-
	Card @=< X.
insert_card(Card,[X|L],[X|L1]) :-
	Card @> X,
	insert_card(Card,L,L1).

step_all(GS,GS,[]) :- 
	\+single_step(GS,_,_).
step_all(GS,GSN,[A|L]) :- 
	single_step(GS,GS1,A),
	step_all(GS1,GSN,L).

step_all(GS,(GSN,A)) :-
	step_all(GS,GSN,A).

step_all_unique(GS,GSN,A) :-
	setof((GS1,A1),step_all(GS,GS1,A1),(GSN,A)).

step_all_atom(GS,GSN,A) :-
	term_to_atom(GST,GS),
	step_all(GST,GSNT,AT),
	term_to_atom(GSNT,GSN),
	term_to_atom(AT,A).

step_action(GS,GSN,A) :-
	step_all(GS,GS1,A),
	game_state_get("Stack",GS1,["End Action"|Stack]),
	game_state_set("Stack",["Next Action"|Stack],GS1,GSN).

step_action(GS,GSN) :- 
	setof(GS1, step_action(GS,GS1,_), GS2),
	choose(GS2, GSN, _).

step_action_atom(GS,GSN,A) :-
	term_to_atom(GST,GS),
	step_action(GST,GSNT,AT),
	term_to_atom(GSNT,GSN),
	term_to_atom(AT,A).
% Stepping an action
