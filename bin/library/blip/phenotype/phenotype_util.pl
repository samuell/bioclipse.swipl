
:- use_module(bio(ontol_db)).
:- use_module(bio(curation_db)).

% HP
gene_mp(F,P) :-
        curation_statement(_,F,_,P).

mp_phenotype(MP,(E,Q,D,W)) :-
        genus(MP,Q),
        belongs(Q,quality),
        (   differentium(MP,'OBO_REL:inheres_in',E)
        ->  true
        ;   E=thing),
        (   differentium(MP,'OBO_REL:inheres_in_part_of',W)
        ->  true
        ;   W=thing),
        (   differentium(MP,'OBO_REL:towards',D)
        ->  true
        ;   D=(-)).

mp_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P).

mp_nr_subsumed_by_phenotype(MP,P) :-
        subclassRT(MP,MP1),
        mp_phenotype(MP1,P),
        \+ ((subclassRT(MP,MP2),
             mp_phenotype(MP2,_),
             subclassT(MP2,MP1))).

feature_phenotype(F,P) :-
        gene_mp(F,MP),
        mp_nr_subsumed_by_phenotype(MP,P).

feature_phenotype(F,P,Root) :-
        gene_mp(F,MP),
        subclassRT(MP,Root),
        mp_nr_subsumed_by_phenotype(MP,P).





