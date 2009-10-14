:- module(pkb_from_obo,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(bio(tabling)).
:- use_module(bio(metadata_db)).
:- use_module(bio(ontol_db)).

:- multifile metadata_db:entity_label/2.

pkb_db:species(S) :- species_label(S,_).
pkb_db:species_label('NCBITaxon:7955',zebrafish). 
pkb_db:species_label('NCBITaxon:9696',human).
pkb_db:species_label('NCBITaxon:10090',mouse). 
foo('0').
metadata_db:entity_label(S,L) :- species_label(S,L).

% simple - treat each gene/genotype as organism for now (must have annotation)
% TODO - transgenes
pkb_db:organism(Org) :- inst_ofRT(Org,'SO:0000704'), entity_label(Org,_), \+ \+ feature_phenotype(Org,_).
pkb_db:organism_species(Org,Species) :- inst_rel(Org,encoded_by,Species).

pkb_db:organism_label(Org,X) :- organism(Org),entity_label(Org,X).

pkb_db:organism_type(Org,Type) :- organism_species(Org,Type). % TODO - transgene construct etc

pkb_db:organism_role(Org,model) :- organism(Org). % TODO

pkb_db:organism_description(Org,Desc) :-
        organism(Org),
        entity_comment(Org,Desc).

% RQ
pkb_db:organism_phenotype(Org,(E1,Q1,D1,W1)) :-
        organism(Org),
        feature_phenotype(Org,(E,Q,D,W)),
        iduri(E,E1),
        iduri(Q,Q1),
        iduri(D,D1),
        iduri(W,W1).

iduri(-,-) :- !.
iduri(thing,-) :- !.
%iduri(URI,URI) :- var(URI),!.
iduri(ID,URI) :-
        concat_atom([S,Loc],':',ID),
        !,
        concat_atom(['http://purl.org/obo/owl/',S,'#',S,'_',Loc],URI).
iduri(URI,URI) :- !.



