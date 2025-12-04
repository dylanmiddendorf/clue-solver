% Players
player(p1). player(p2). player(p3).

% Turn order
pos(p1,0). pos(p2,1). pos(p3,2).

% Cards
suspect(mustard). suspect(scarlet). suspect(plum).
weapon(knife). weapon(rope). weapon(candlestick).
room(kitchen). room(study). room(lounge).

% Initial hand
holds(p3, mustard). holds(p3, lounge).

% Event 1: p1 -> mustard / knife / kitchen   (p2 responds)
suggestion(e1, p1, mustard, knife, kitchen).
responder(e1, p2).

% Event 2: p2 -> mustard / candlestick / lounge  (p3 responds)
suggestion(e2, p2, mustard, candlestick, lounge).
responder(e2, p3).

% Event 3: p3 -> plum / knife / lounge  (p1 responds)
suggestion(e3, p3, plum, knife, lounge).
responder(e3, p1).

% Event 4: p1 -> scarlet / rope / study  (No responder)
suggestion(e4, p1, scarlet, rope, study).


