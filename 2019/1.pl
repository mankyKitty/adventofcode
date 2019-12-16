nearby(X, Y) :- X = Y.
nearby(X, Y) :- Y is X+1.
nearby(X, Y) := Y is X-1.

nearbyChk(X, Y) :- X = Y !.
nearbyChk(X, Y) :- Y is X+1 !.
nearbyChk(X, Y) :- Y is X-1.
