/* -*- Mode: Prolog -*- */

:- module(macros_lookup,
          [lookup_predicate/4]).

%:- module_transparent lookup_predicate/4.

:- use_module(bio(textmatch)).

lookup_predicate(A,B,C,D):-
	throw(error(context_error(nodirective, lookup_predicate(A,B,C,D)), _)).

:- multifile system:term_expansion/2.
:- dynamic system:term_expansion/2.

system:term_expansion((:- lookup_predicate(PredName,
                                         ID,
                                         _Pred,
                                         LookupTerms)),
                    LookupClause):-
        !,
        maplist(lookup_term_to_query_pred(ID,SearchTerm),
                LookupTerms,
                QueryPreds),
        list_to_disj(QueryPreds,QueryTuple),
        atom_concat(lookup_,PredName,LookupPredName),
        LookupClauseHead =.. [LookupPredName,
                              SearchTerm,
                              ID],
        LookupClause = (LookupClauseHead :- QueryTuple).

lookup_term_to_query_pred(_ID,SearchTerm,N-Term,(Term,textmatch:textmatch(N,SearchTerm))).

list_to_disj([G],G):- !.
list_to_disj([G|Gs],(G;Tup)):-
        list_to_disj(Gs,Tup).

/** <module> macros to generate lookup/search predicates


  ---+ Description

  ==
  lookup_predicate(Predicate,Template,Goal,Queries)
  ==

  
  
**/
