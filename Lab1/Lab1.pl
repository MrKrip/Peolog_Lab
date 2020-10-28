parent(limalisha,luna).
parent(limalisha,jeremy).
parent(jack,luna).
parent(jack,jeremy).
parent(jeremy,betty).
parent(luna,sam).

sex(limalisha,f).
sex(jack,m).
sex(luna,f).
sex(jeremy,m).
sex(betty,f).
sex(sam,m).

dad(X,Y):- sex(X,m), parent(X,Y).
mom(X,Y):- sex(X,f), parent(X,Y).
son(X,Y):- sex(Y,m),parent(X,Y).
daughter(X,Y):- sex(Y,f),parent(X,Y).
brother(X,Y):- sex(X,m),parent(Z,X),parent(Z,Y).
sister(X,Y):- sex(X,f),parent(Z,X),parent(Z,Y).
uncle(X,Y):-sex(X,m),sister(X,Z),daughter(Z,Y);sex(X,m),brother(X,Z),son(Z,Y).
aunt(X,Y):-sex(X,f),sister(X,Z),daughter(Z,Y);sex(X,f),brother(X,Z),son(Z,Y).
grandfather(X,Y):-sex(X,m),parent(X,Z),parent(Z,Y) .
grandmother(X,Y):-sex(X,f),parent(X,Z),parent(Z,Y) .
grandson(X,Y):- sex(X,m),parent(Z,X),parent(Y,Z) .
andson(X,Y):- sex(X,f),parent(Z,X),parent(Y,Z) .

sum(X,Y,Z):-X=<Y,Z is X ,!.
sum(X,_,0):-X=<0.
sum(X,Y,Z):-X>=0,Y>0,X1 is X-Y,sum(X1,Y,Z1), Z is X+Z1.

step(_,0,1):-!.
step(X,1,X):-!.
step(0,_,0):-!.
step(X,Y,Z):-Y1 is Y mod 2,Y1=0,Y2 is Y/2, step(X,Y2,Z1), Z is Z1*Z1.
step(X,Y,Z):-Y2 is Y mod 2,Y2=1,Y>0,Y1 is Y-1,step(X,Y1,Z1),Z is Z1*X,!.
step(X,Y,Z):-Y2 is Y mod 2,Y2=1,Y<0,Y1 is Y+1,step(X,Y1,Z1),Z is Z1/X,!.

sumN(0,0).
sumN(X,Y):-X>0,X1 is X-1,sumN(X1,Y1),Y is Y1+X.

