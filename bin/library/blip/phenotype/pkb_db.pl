:- module(pkb_db,
          [
           organism/1,
           organism_label/2,
           organism_type/2,
           organism_species/2,
           organism_role/2,
           organism_description/2,
           organism_phenotype/2,
           organism_phenotype_inst/2,
           organism_inferred_type/2,
           organism_role_disease/3,
           inferred_organism_role_disease/3,
           species/1,
           species_label/2,
           disease/1,
           disease_label/2,
           disease_description/2,

           disease_phenotype/2
           ]).

%:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_basic_reasoner'),
              [entailed/1       % for organism_inferred_type/2
              ]).
:- use_module(bio(bioprolog_util),[solutions/3]).

:- use_module(phenotype_db).
:- use_module(bio(dbmeta)).

%% organism(?Org)
:- extensional(organism/1).

%% organism_label(?Org,?Label)
:- extensional(organism_label/2).

%% organism_type(?Org,?Type)
:- extensional(organism_type/2).

%% organism_role(?Org,?Role)
:- extensional(organism_role/2).

%% organism_role_disease(?Org,?Role,?Disease)
:- extensional(organism_role_disease/3).

%% organism_role_disease(?Org,?Role,?Disease,?Species)
:- extensional(organism_role_disease_species/4).

%% organism_species(?Org,?Species)
:- extensional(organism_species/2).

%% organism_description(?Org,?Desc)
:- extensional(organism_description/2).

%% organism_phenotype_inst(?Org,?PhenoInst)
:- extensional(organism_phenotype_inst/2).

%% organism_phenotype(?Org,?Phenotype)
% Phenotype = (E,Q,D,W)
:- extensional(organism_phenotype/2).

%% species(?Sp)
:- extensional(species/1).

%% species_label(?Sp,?Label)
:- extensional(species_label/2).

%% disease(?D)
:- extensional(disease/1).

%% disease_label(?D,?Label)
:- extensional(disease_label/2).

%% disease_description(?D,?Desc)
:- extensional(disease_description/2).

%% inferred_organism_role_disease(?Model,?Role,?Disease)
inferred_organism_role_disease(Model,model,D) :-
        solutions(MaxIC-Model,
                  (   organism_role_disease(Patient,patient,D),
                      feature_pair_simnr(Patient,Model,_,_,_,MaxIC),
                      \+ organism_role_disease(Model,patient,_)
                  ),
                  L),           % TODO
        member(MaxIC-Model,L),
        \+ ((member(MaxIC2-_,L),
             MaxIC2 > MaxIC)).

%% inferred_organism_role_disease_species(?Model,?Role,?Disease,?Sp)
inferred_organism_role_disease_species(Model,model,D,S) :-
        species(S),
        solutions(Sc-Model,
                  (   organism_role_disease(Patient,patient,D),
                      organism_pair_combined_score(Patient,Model,Sc),
                      Sc > 2.0, % arbitrary for now...
                      organism_species(Model,S),
                      \+ organism_role_disease(Model,patient,_)
                  ),
                  L),           % TODO
        member(Sc-Model,L),
        \+ ((member(Sc2-_,L),
             Sc2 > Sc)).

%% organism_pair_combined_score(?O1,?O2,?S)
% ad-hoc combination of similarity scores between two organisms, based on feature_pair_simnr/6
organism_pair_combined_score(O1,O2,S) :-
        feature_pair_simnr(O1,O2,_,_,AvgIC,MaxIC),
        S is MaxIC + AvgIC/5.

%% organism_inferred_type(?Org,?Type)
% combination of organism_type/2 and inferred (reflexive) subClassOf/2 using entailed/1
organism_inferred_type(Org,Type) :-
        organism_type(Org,Type1),
        entailed(subClassOfReflexive(Type1,Type)).

%% disease_phenotype(?D,?P)
% true if disease is associated with phenotype, by virtue of some organism with the disease carrying the phenotype
disease_phenotype(D,P) :-
        organism_role_disease(O,_,D),
        organism_phenotype(O,P).

/** <module> model for NIF neurodegenerative diseases phenotype knowledgebase

  ---+ Synopsis

==
:- use_module(bio(pkb_db)).

% 
demo:-
  nl.
  

==

---+ Details

---++ Requirements

* Thea2 - uses subClassOf/2 and entailed/1


*/
