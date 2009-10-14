:- use_module(bio(metadata_db)).

feature_phenotype(FID,(E,Q,D,-)) :-
        phenorow(F,_,_,_,_,_,__,EN,_,QN,_,DN,_Tag,_,_,_),
        idzfin(F,FID),
        lookup(EN,E),
        lookup(QN,Q),
        lookup(DN,D).

lookup(N,X) :- entity_label(X,N),!.
lookup(N,X) :- entity_synonym(X,N),!.
lookup(_,-).

gene_phenotype(GID,P) :-
        feature_phenotype(FID,P),
        idzfin(F,FID),
        genotype_features(F,_,_,_,_,_,_,_,_,G,_),
        G\='',
        idzfin(G,GID).

idzfin(Local,ID) :-
        atom_concat('ZFIN:',Local,ID).







        
