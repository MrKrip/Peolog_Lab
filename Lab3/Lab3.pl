defendant('knight').
defendant('liar').

testimony('knight',X,X).
testimony('liar',X,Y):-not(X=Y).

solution(A,B):-
    defendant(A),
    defendant(B),
    testimony(A,B,'liar'),
    testimony(A,A,'knight');
    testimony(A,A,'liar'),
    testimony(A,B,'knight');
    testimony(A,B,'liar'),
    testimony(A,A,'liar').

sol(A,B):-A='liar',not(solution(A,B));A='knight',solution(A,B).

puzzle([C,E,L,E,R,Y]+[P,E,P,P,E,R]+[S,A,G,E]=[G,A,R,L,I,C]):-
    Vars = [C,E,L,R,Y,P,S,A,G,I],
    Vars ins 0..9,
    all_different(Vars),C*100000+E*10000+L*1000+E*100+R*10+Y
    +P*100000+E*10000+P*1000+P*100+E*10+R+S*1000+A*100+G*10+E
    #= G*100000+A*10000+R*1000+L*100+I*10+C,C#\=0,P#\=0,S#\=0,G#\=0.



