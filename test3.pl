% -------------------------------------------------
% TEST 3
% This test case is designed to validate the deduction of our encoding
% by demonstrating a scneario where the all players' hands and the case file
% can be fully deduced via only suggestions and responses.
%
% Setup: 3 players, we are p3, 2 cards per hand
% -------------------------------------------------

% Players and Positions
player(p1). player(p2). player(p3).
pos(p1,0). pos(p2,1). pos(p3,2).

% Cards
suspect(mustard). suspect(scarlet). suspect(plum).
weapon(knife). weapon(rope). weapon(candlestick).
room(kitchen). room(study). room(lounge).

% Known Hand
holds(p3, mustard). 
holds(p3, kitchen).

% Observations

% Event 1: p2 -> mustard / knife / kitchen
% p1 responds
suggestion(e1, p3, mustard, knife, kitchen).
responder(e1, p1).

% Event 2: p1 -> plum / candlestick / kitchen
% p3 responds
suggestion(e2, p1, plum, candlestick, kitchen).
responder(e2, p3).

% Event 3: p3 -> mustard / candlestick / lounge
% p2 responds
suggestion(e3, p3, mustard, candlestick, lounge).
responder(e3, p2).
holds(p2, lounge).

% Event 4: p2 -> mustard / candlestick / kitchen
% p3 responds
suggestion(e4, p2, mustard, candlestick, kitchen).
responder(e4, p3).

% Event 5: p2 -> scarlet / candlestick / lounge
% p1 responds
suggestion(e5, p2, scarlet, candlestick, lounge).
responder(e5, p1).

% Event 6: p1 -> scarlet / rope / kitchen
% p2 responds
suggestion(e6, p1, scarlet, rope, kitchen).
responder(e6, p2).
