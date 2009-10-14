/* -*- Mode: Prolog -*- */

/*

  DEPRECATED: use seqfteaure_sqlmap_chado
  */

:- module(seqfeature_bridge_from_chadosql,
          [
          ]).

:- multifile seqfeature_db:goal_expansion/2.

:- use_module(bio(seqfeature_db),[]).
:- use_module(bio(rdb_util)).

getrdb(Rdb):-
        nb_getval(rdb,Rdb).
setrdb(Rdb):-
        nb_setval(rdb,Rdb).

set_bridge_resource(Resource):-
        rdb_connect(Rdb,Resource),
        setrdb(Rdb).

rdb_util:qmap(feature(ID,N,T),
              sql(cols=['f.feature_id'=ID,
                        'f.name'=N,
                        't.name'=T],
                  from=['feature AS f INNER JOIN cvterm AS t ON (f.type_id=t.cvterm_id)'])).

rdb_util:qmap(feature_relationship(ID,T,PID),
              sql(cols=['subject_id'=ID,
                        't.name'=T,
                        'object_id'=PID],
                  from=['feature_relationship AS fr INNER JOIN cvterm AS t ON (fr.type_id=t.cvterm_id)'])).

seqfeature_db:feature(ID,N,T):-
        getrdb(Rdb),
        rdb_lookup(Rdb,feature(ID,N,T)).
seqfeature_db:feature_relationship(ID,T,PID):-
        getrdb(Rdb),
        rdb_lookup(Rdb,feature(ID,T,PID)).

% rewrite view 'parent'
seqfeature_db:goal_expansion(parent(ID,T,PID),seqfeature_bridge_from_chadosql:parent(ID,T,PID)).
parent(ID,T,PID):-
        getrdb(Rdb),
        rdb_lookup(Rdb,parent(ID,T0,PID)),
        (T0 = is_a -> T=subclass; T=T0).

% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(load(chado)=
      (seqfeature_bridge_from_chadosql:set_bridge_resource(cjmchado))/[]).

unittest(test(basic,
            [_=load(chado)],
            (   ensure_loaded(bio(seqfeature_db)),
                ensure_loaded(bio(seqfeature_bridge_from_chadosql)),
                forall(feature(_ID,N,T),
                       format('~w ~w~n',[N,T]))),
            true)).
/** <module>   
  @author Chris Mungall
  @version  $Revision: 1.4 $
  @date  $Date: 2006/03/25 01:57:15 $
  @license LGPL

  ---+ Name
%  seqfeature_bridge_from_chadosql

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_chadosql)).
  :- seqfeature_bridge_from_chadosql:set_bridge_resource(chado_dmel).

  demo:-
    gene(GID,GN),
    gene_transcript(GID,TID),
    transcript(TID,TN),
    feature_residues(TID,Seq),
    format('Gene:~w ~w ~w~n',[GN,TN,Seq]),
    fail.
  ==

  ---+ Description

  This allows a SQL Database using the Chado Schema to masquerade as seqfeature
  predicates

  See <http://www.gmod.org/schema> Chado

  */
