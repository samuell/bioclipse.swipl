:- module(pkb_to_phenotype,[]).

:- use_module(pkb_db).
:- use_module(phenotype_db).
:- use_module(bio(ontol_db)).
:- use_module(bio(metadata_db)).
:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(library('thea2/owl2_model'),[]).

phenotype_db:feature_phenotype(O,P) :- organism_phenotype(O,P).

% also include organism type
phenotype_db:feature_phenotype(T,P) :- setof(T,O^organism_type(O,T),Ts),member(T,Ts),\+species(T),organism_type(O,T),organism_phenotype(O,P).

% temp
ontol_db:subclass(X,Y) :- entailed(subClassOfReflexive(X,Y)),Y\='http://www.ifomis.org/bfo/1.1/snap#Object'.
ontol_db:restriction(X,part_of,Y) :- entailed(subClassOf(X,someValuesFrom('http://www.obofoundry.org/ro/ro.owl#part_of',Y))),atom(X),atom(Y).
%ontol_db:restriction(X,part_of,Y) :- subclass(X,Y). % reflexivity

metadata_db:entity_label(X,V) :- owl2_model:labelAnnotation_value(X,V).




