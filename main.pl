% -------------- DOMAIN (declare in instance) ----------------
% player(P).         % players in play
% pos(P,I).          % position index 0..n-1 (clockwise), used to compute who is asked before who
% #const n = N.      % number of players (must match number of pos facts)
% suspect(S). weapon(W). room(R).   % card categories
% card(C) :- suspect(C).  card(C) :- weapon(C).  card(C) :- room(C).

% ---------- EVENTS / OBSERVATIONS (declare in instance) ----------
% suggestion(E,A,Sus,Wp,Rm).   % event id E: A (asker) suggests triple Sus,Wp,Rm
% responder(E,R).             % if someone responded (exists R). If absent => no responder (nobody had the 3 cards).
% showed_card(E,R,C).         % optional: responder R actually showed specific card C to asker (private but available to model)
% showed(E,R).                % optional: public fact "R showed something" but card unknown
% (If you don't have showed_card, you can use only 'responder' or 'showed' depending on what is known.)

% ------------ GLOBAL CARD ASSIGNMENT ("world") ----------------
% Every card is either in the case file OR held by exactly one player.
% (Choice: either case(C) or exactly one hold(P,C))
card(S) :- suspect(S).
card(W) :- weapon(W).
card(C) :- room(C).

{ case(C) ; holds(P,C) : player(P) } = 1 :- card(C).
{ case(S) : suspect(S)} = 1.
{ case(W) : weapon(W)} = 1.
{ case(R) : room(R)} = 1.


% case and hold cannot both be true (redundant with the choice above but kept for clarity)
:- case(C), holds(P,C).

% A player cannot have the same card twice (obvious by above) but we ensure unique by the choice.

% ------------- SUGGESTION/RESPONSE LOGIC -----------------------
% Helper: suggested_in(E,C) lists the three cards in a suggestion.
suggested(E,S) :- suggestion(E,_,S,_,_), suspect(S).
suggested(E,W) :- suggestion(E,_,_,W,_), weapon(W).
suggested(E,Rm) :- suggestion(E,_,_,_,Rm), room(Rm).

% Determine which players are "asked before responder" using circular positions.
% You must provide pos(P,I) and #const n = number of players.
% If there is a responder R for event E, players P with distance Dp from asker satisfying 0 < Dp < Dr
% are those who were asked and did NOT respond (so they must lack the suggested cards).
dist(From,To,D) :- pos(From, PF), pos(To, PT),
                   N = PT - PF + n, Q = N / n, D = N - (Q * n). % PT - PF (mod n)


% passed(E,P): Player P passed at event E
passed(E,P) :- suggestion(E,A,_,_,_), responder(E,R),
                   dist(A, P, Dp), dist(A, R, Dr), 0 < Dp, Dp < Dr.

% If a player passes, then they don't have any suggested card
-holds(P,C) :- passed(E,P), suggested(E,C).

% If player explicitly showed card C at event E, they must have it:
holds(R,C) :- showed_card(E,R,C).

% If responder R showed something, then R must hold one of the suggested cards
1 { holds(R,C) : suggested(E, C) } :- showed(E,R).

% If we only know that responder R showed something (but not which), then require that R
% has at least one of the suggested cards:
% helper that connects suggested card to a possibility
% show_possible(E,S) :- suggestion(E,_,S,_,_).
% show_possible(E,W) :- suggestion(E,_,_,W,_).
% show_possible(E,R) :- suggestion(E,_,_,_,R).

% % enforce: if 'showed(E,R)' is declared, at least one of the suggested cards must belong to R.
% :- showed(E,R), not has_shown_by_holder(E,R).
% has_shown_by_holder(E,R) :- suggestion(E,_,S,_,_), hold(R,S).
% has_shown_by_holder(E,R) :- suggestion(E,_,_,W,_), hold(R,W).
% has_shown_by_holder(E,R) :- suggestion(E,_,_,_,Rm), hold(R,Rm).

% % --------------- POSSIBILITY / PROVABLE / EXPLANATIONS -----------------
% % possible_holder(P,C) means P could still hold C (no explicit not_have)
% possible_holder(P,C) :- player(P), card(C), not not_have(P,C).

% % detect if no player can hold a card (then it must be in case file)
% no_player_possible(C) :- card(C), not possible_holder(_,C).
% % If no player possible then case(C) must hold
% :- no_player_possible(C), not case(C).
% % explanation atom: we deduced C is in case_file because no player could hold it
% deduced_case(C,reason(no_one_has)) :- no_player_possible(C), case(C).

% % If exactly one possible holder remains for C, and C is not in case, then that holder must have it.
% other_possible(Q,C) :- possible_holder(Q,C), possible_holder(P,C), player(Q), card(C), Q != P.
% unique_possible(P,C) :- possible_holder(P,C), not other_possible(P,C).
% % force: if unique_possible(P,C) and not case(C) then P must hold C.
% :- unique_possible(P,C), not case(C), not hold(P,C).
% % explanation atom: this hold was forced because only one possible holder
% deduced_hold(P,C,reason(only_possible_holder)) :- unique_possible(P,C), hold(P,C).

% % If a holder is forced because they showed a specific card, record that reason
% deduced_hold(P,C,reason(shown_event(E))) :- showed_card(E,P,C), hold(P,C).

% % record not_have provenance (useful for explanations)
% deduced_not_have(P,C,from_suggestion(E)) :- in_between(E,P), suggested_in(E,C), not_have(P,C).

% --------------- OUTPUT (what to show in answer-sets) ---------------
% #show case/1.
% % #show hold/2.
% #show deduced_case/2.
% #show deduced_hold/3.
% #show deduced_not_have/3.

% players and positions (must match #const n)
player(alice). pos(alice,0).
player(bob).   pos(bob,1).
player(carol). pos(carol,2).
#const n = 3.

% cards (small toy domain)
suspect(mustard). suspect(plum). suspect(scarlet).
weapon(knife). weapon(rope). weapon(candlestick).
room(study). room(kitchen). room(ballroom).

% events
holds(alice, mustard).
holds(alice, kitchen).
holds(alice, ballroom).

suggestion(e1, alice, mustard, knife, study).
responder(e1, bob).
% showed(e1,bob).

suggestion(e2, carol, plum, rope, kitchen).
% no responder(e2) -> nobody showed for e2

#show case/1.
