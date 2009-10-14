/* -*- Mode: Prolog -*- */


:- module(homol_db,
          [
           % extensional
           homologset/1,
           homologset_type/2,
           homologset_taxon/2,
           homologset_member/2,
           homologset_member_taxon/3,
           homologset_member_symbol/3,
           homologous_to/2,
           reflexive_homologous_to/2,
	   interspecies_homologous_to/2,
	   interspecies_reflexive_homologous_to/2,
	   intraspecies_reflexive_homologous_to/2,
	   intraspecies_homologous_to/2
          ]).

% metadata on the data predicates used in this module
:- use_module(bio(metadata_db)).
:- use_module(bio(dbmeta)).
:- use_module(bio(seqfeature_db)).

%% homologset(?ID)
:- extensional(homologset/1).
%% homologset_type(?Homologset,?Type)
:- extensional(homologset_type/2).
%% homologset_taxon(?Homologset,?Taxon)
:- extensional(homologset_taxon/2).
%% homologset_member(?Homologset,?Feature)
:- extensional(homologset_member/2).

%% homologset_member_taxon(?Homologset,?Feature,?Organism)
% Composes homologset_member/2 and feature_organism/2
homologset_member_taxon(H,G,S):-
	homologset_member(H,G),
	feature_organism(G,S).


homologset_member_phenotype(H,G,P):-
        homologset_taxid_xref(H,TaxID,P,'Phenotypes'),
        homologset_member(H,G,TaxID).

homologset_member_symbol(H,G,S):-
	homologset_member(H,G,_),
	entity_label(G,S).

%% homologous_to(?X,?Y)
% parent sharing by homologset_member/2, non-reflexive
homologous_to(X,Y):-
        homologset_member(H,X),
        homologset_member(H,Y),
        X\=Y.                   % non-reflexive

%% reflexive_homologous_to(?X,?Y)
% as homologous_to/2 but includes self-references.
% this is useful for queries; e.g. given a gene G, find information on G and its homologs
reflexive_homologous_to(X,Y):-
        homologset_member(H,X),
        homologset_member(H,Y).

%% interspecies_homologous_to(?X,?Y)
% as homologous_to/2 different by feature_organism/2, non-reflexive
interspecies_homologous_to(X,Y):-
	interspecies_reflexive_homologous_to(X,Y),
        not(X=Y).                   % non-reflexive

%% interspecies_reflexive_homologous_to(?X,?Y)
% as homologous_to/2 different by feature_organism/2, reflexive
interspecies_reflexive_homologous_to(X,Y):-
        homologset_member(H,X,T1),
        homologset_member(H,Y,T2),
        not(T1=T2).

%% intraspecies_homologous_to(?X,?Y)
% as homologous_to/2 same by feature_organism/2
intraspecies_homologous_to(X,Y):-
	intraspecies_reflexive_homologous_to(X,Y),
        X\=Y.                   % non-reflexive

%% intraspecies_reflexive_homologous_to(X,Y)
% as homologous_to/2 same by feature_organism/2, reflexive
intraspecies_reflexive_homologous_to(X,Y):-
        homologset_member(H,X,T),
        homologset_member(H,Y,T).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(homol,
             [],
             (   ensure_loaded(bio(homol_db)),
                 load_biofile(homologene,'EYA1-homologene.xml'),
                 homologset_taxid(H,'NCBITaxon:33316'),
                 homologset_member(H,_,'NCBITaxon:9606')),
            true)).

/** <module> Model for homologous sets of genes or gene products

  ---+ Synopsis

==
  :- use_module(bio(io)).
  :- use_module(bio(homol_db)).
  :- use_module(bio(ontol_db)).
  
  demo:-
          load_biofile(homologene,'EYA1-homologene.xml'),
          load_bioresource(taxonomy),
          load_bioresource(mammalian_phenotype),
          forall(homologset(Char),
                 show_homologset(Char)).
  
  show_homologset(Char):-
          homologset_taxid(Char,Tax),
          class(Tax,TaxName),
          format('HomologSet: ~w // Tax: ~w ~w~n',[Char,Tax,TaxName]),
          forall(homologset_member(Char,Member,MTax),
                 show_hmember(Char,Member,MTax)).
  
  show_hmember(Char,Member,Tax):-
          class(Tax,TaxName),
          format('  Member: ~w // Tax: ~w ~w~n',[Member,Tax,TaxName]),
          findall(Phen,(homologset_taxid_xref(Char,Tax,Xref,_),
                        class(Xref,Phen)),
                  Phens),
          maplist(format('    Phen: ~w~n'),Phens).
==

  ---+ Description

This module contains predicates for
representing and querying sets of homologous genes or gene products.

The model here is deliberately simplistic, and follows homology-set
resources such as HomoloGene. For deeper investigations into
evolutionary relationships, use phylo_db.pro as a model. See for example phylonode/1

**/
