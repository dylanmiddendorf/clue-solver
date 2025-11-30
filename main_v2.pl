% Modeling the Game Clue Using Answer Set Programming
%
% --------------------------------------------------------------------------
% KRR Methodology:
% This project uses the generate-and-test methodology combined with constraint satisfaction
% to solve a combinatorial deduction problem based on the game Clue (Cluedo).
%
% 1) Generate: Possible card assignments (which players hold which cards, and which cards are in the case file).
% 2) Test/Constraint: Observations are converted into logical constraints or negative facts, which prune
%    impossible models.
% 3) Define/Deduce: Rules are used to propogate knowledge, leading to a set of consistent answer sets.
% --------------------------------------------------------------------------
%
% INPUT PREDICATES AND DESCRIPTIONS:
% player(P): Defines P as a player in the game.
% pos(P,I): Defines the clockwise position I (0 to n-1, clockwise) of player P.
% #const n = N: The total number of players.
% suspect(S), weapon(W), room(R): Define the possible cards in each category.
% suggestion(E,A,Sus,Wp,Rm): Event E where asker A suggests the combination of Sus, Wp, and Rm.
% responder(E,R): Indicates that player R responded to suggestion E. (Implies R showed *a* card).
% showed_card(E,R,C): The specific card C shown by responder R during event E (if known).
%
% OUTPUT PREDICATES AND DESCRIPTIONS:
% case(C): A card C is in the confidential case file (the solution).
% deduced_case(C,Reason): A card C is logically forced into the case file for the given Reason (e.g., 'no_one_has').
% deduced_hold(P,C,Reason): Player P is logically forced to hold card C for the given Reason (e.g., 'shown_event(E)').
% deduced_not_have(P,C,Reason): It is logically certain that P does not hold card C for the given Reason (e.g., 'from_suggestion(E)').
% --------------------------------------------------------------------------

% ------------ GLOBAL CARD ASSIGNMENT ("world") ----------------
% Every card is either in the case file OR held by exactly one player.
% (Choice: either case(C) or exactly one hold(P,C))

% Helper rule: defines all suspects as members of the general card set.
card(S) :- suspect(S).
% Helper rule: defines all weapons as members of the general card set.
card(W) :- weapon(W).
% Helper rule: defines all rooms as members of the general card set.
card(R) :- room(R).

% Generate all possible distributions by asserting that every card must be in the case file OR held by exactly one player.
{ case(C) ; holds(P,C) : player(P) } = 1 :- card(C).

% Enforce that exactly one suspect, one weapon, and one room are in the case file.
{ case(S) : suspect(S)} = 1.
{ case(W) : weapon(W)} = 1.
{ case(R) : room(R)} = 1.


% A card cannot be simultaneously in the case file and held by a player.
:- case(C), holds(P,C).

% ------------- SUGGESTION/RESPONSE LOGIC -----------------------
% Helper: suggested_in(E,C) lists the three cards in a suggestion.
suggested(E,S) :- suggestion(E,_,S,_,_), suspect(S).
suggested(E,W) :- suggestion(E,_,_,W,_), weapon(W).
suggested(E,Rm) :- suggestion(E,_,_,_,Rm), room(Rm).

% clockwise distance (0..n-1) from From to To
dist(From,To,D) :- pos(From, PF), pos(To, PT), N = PT - PF + n, Q = N / n, D = N - (Q * n). % PT - PF (mod n)

% Derives that player P passed at event E if they were asked after the asker but before
% the eventual responder R, based on calculated position distance.
passed(E,P) :- suggestion(E,A,_,_,_), responder(E,R), dist(A,P,Dp), dist(A,R,Dr), Dp > 0, Dp < Dr.

% If a player is deduced to have passed, they cannot hold *any* of the suggested cards.
% This prunes any generated model where the passing player holds one of those cards.
-holds(P,C) :- passed(E,P), suggested(E,C).

% If player explicitly showed card C at event E, then that player is certain to hold C.
holds(R,C) :- showed_card(E,R,C).

% If the public fact 'showed(E,R)' is known (R showed *something*), then the model
% must ensure that R holds **at least one** of the suggested cards.
1 { holds(R,C) : suggested(E,C) } :- showed(E,R).

% If no player is recorded as a responder for an event E, then
% EVERY player must lack ALL three suggested cards, pruning models where they have one.
:- suggestion(E,_,_,_,_), not responder(E,_), player(P), suggested(E,C), holds(P,C).

% % --------------- POSSIBILITY / PROVABLE / EXPLANATIONS -----------------
% Helper: defines a card as 'possible' for a player if there's no explicit knowledge that they don't have it.
possible_holder(P,C) :- player(P), card(C), not not_have(P,C).

% Define not_have/2 to be true when the strong negation -holds/2 is proven.
not_have(P,C) :- -holds(P,C).

% Helper: detects when all players are proven *not* to hold a card C.
no_player_possible(C) :- card(C), not possible_holder(_,C).

% If no player can possibly hold a card C (i.e., every player has -holds(P,C)
% proved for them), then C MUST be in the case file.
:- no_player_possible(C), not case(C).

% Helper: detects when there is exactly one player left who *could* possibly hold a card C.
unique_possible(P,C) :- possible_holder(P,C), not other_possible(P,C).
other_possible(Q,C) :- possible_holder(Q,C), possible_holder(P,C), player(P), P != Q.

% If there is exactly one unique player P who can possibly hold card C,
% and C is NOT in the case file, then P must hold C (Propagating deduction).
:- unique_possible(P,C), not case(C), not holds(P,C).

% Records the reason why a card was forced into the case file (for explanation/output).
% If no one responds to suggestion E, the suggested SUSPECT (C1) must be in the case file.
deduced_case(C1, reason(no_response_to_suggestion(E))) :-
    suggestion(E, _, C1, _, _), not responder(E, _), case(C1).

% If no one responds to suggestion E, the suggested WEAPON (C2) must be in the case file.
deduced_case(C2, reason(no_response_to_suggestion(E))) :-
    suggestion(E, _, _, C2, _), not responder(E, _), case(C2).

% If no one responds to suggestion E, the suggested ROOM (C3) must be in the case file.
deduced_case(C3, reason(no_response_to_suggestion(E))) :-
    suggestion(E, _, _, _, C3), not responder(E, _), case(C3).

deduced_hold(P,C,reason(only_possible_holder)) :- unique_possible(P,C), holds(P,C), not case(C).
deduced_not_have(P,C,reason(passed_event(E))) :- passed(E,P), suggested(E,C), -holds(P,C).
deduced_hold(P,C,reason(shown_event(E))) :- showed_card(E,P,C), holds(P,C).
% --------------- OUTPUT (what to show in answer-sets) ---------------
% players and positions (must match #const n)
player(alice). pos(alice,0).
player(bob). pos(bob,1).
player(carol). pos(carol,2).
#const n = 3.

% cards (small toy domain)
suspect(mustard). suspect(plum). suspect(scarlet).
weapon(knife). weapon(rope). weapon(candlestick).
room(study). room(kitchen). room(ballroom).

suggestion(e1, alice, mustard, knife, study).
responder(e1, bob).
%showed_card(e1, bob, mustard).

suggestion(e2, carol, plum, rope, kitchen).

#show case/1.
#show holds/2.
#show deduced_case/2.
#show deduced_hold/3.
#show deduced_not_have/3.