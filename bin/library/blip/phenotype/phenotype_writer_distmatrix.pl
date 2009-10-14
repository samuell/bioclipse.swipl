:- module(phenotype_writer_distmatrix,[]).

:- use_module(phenotype_db).
:- use_module(pkb_db).
:- use_module(bio(bioprolog_util)).

io:format_writer(distmatrix,phenotype_writer_distmatrix).

% semi-hack: indicates that this writes to stdout
io:redirect_stdout(distmatrix).
        
io:write_all(distmatrix,_,_Filter):-
        solutions(F1,feature_pair_simnr(F1,_,_,_,_,_),Fs),
        length(Fs,Num),
        format(' ~d~n',[Num]),
        findall(F-ID,(member(F,Fs),gensym(fnode,ID)),FMap),
        forall(member(F-ID,FMap),
               (   (   organism_label(F,N)
                   ->  true
                   ;   N=F),
                   format(user_error,'~w ~w~n',[ID,N]))),
        forall(member(F1,Fs),
               write_row(FMap,Fs,F1)).

write_row(FMap,Fs,F1) :-
        member(F1-F1X,FMap),
        writef('%10L',[F1X]),
        forall(member(F2,Fs),
               (   (   F1=F2
                   ->  Score = 0
                   ;   (   feature_pair_simnr(F1,F2,_,_,MaxIC,ICCS)
%                       ->  Score is 10 - (MaxIC + ICCS/5) / 5
                       ->  Score is  0 - (MaxIC + ICCS/5) / 5
                       ;   Score is 10)),
                   format(' ~4f',[Score]))),
        nl.

