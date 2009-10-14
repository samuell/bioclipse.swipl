/* -*- Mode: Prolog -*- */

:- module(genome_bridge_from_seqfeature_db,
          []).

:- use_module(seqfeature_db).
:- use_module(genome_db).

% TYPES

% TODO: consider automating this mapping; mostly a case of moving the type from argument to predicate
genome_db:gene(X):- feature_type(X,gene).
genome_db:gene_label(X,N):- feature_type(X,gene),feature_label(X,N).
genome_db:transcript(X):- implied_feature_type(X,transcript). 
genome_db:exon(X):- feature_type(X,exon).
genome_db:cds(X):- feature_type(X,'CDS').
genome_db:intron(X):- feature_type(X,intron).
genome_db:transposable_element(X):- feature_type(X,transposable_element).
genome_db:psuedogene(X):- feature_type(X,pseudogene).

genome_db:three_prime_utr(X):- feature_type(X,three_prime_UTR).
genome_db:five_prime_utr(X):- feature_type(X,five_prime_UTR).

genome_db:sequence_variant(X):- feature_type(X,sequence_variant).
genome_db:sequence_variant(X):- feature_type(X,point_mutation). % TODO: check

% RELATIONSHIPS

% horrible hack for now: assume transcript
%genome_db:gene_transcript(G,T):- feature_type(G,gene),feature_relationship(T,G),feature_type(T,transcript).
genome_db:gene_transcript(G,T):- feature_type(G,gene),seqfeature_db:feature_relationship(T,G).
%genome_db:exon_transcript_order(X,T,Rank):- feature_type(X,exon),feature_relationship(X,T,_,Rank),feature_type(T,transcript).
%genome_db:exon_transcript_order(X,T,Rank):- feature_type(X,exon),seqfeature_db:feature_relationship(X,T,_,Rank).
genome_db:exon_transcript(X,T):- feature_type(X,exon),seqfeature_db:feature_relationship(X,T,_).

genome_db:transcript_cds(T,C):- feature_type(C,'CDS'),seqfeature_db:feature_relationship(C,T).

genome_db:regulatory_region(X):- implied_feature_type(X,regulatory_region). 
genome_db:regulates_gene(R,G):- implied_feature_type(R,regulatory_region),seqfeature_db:feature_relationship(R,G).

% POS

genome_db:exon_dnaseq_pos(X,Seq,B,E,Str) :- exon(X),featureloc(X,Seq,B,E,Str).
genome_db:transcript_dnaseq_pos(X,Seq,B,E,Str) :- transcript(X),featureloc(X,Seq,B,E,Str).
genome_db:intron_dnaseq_pos(X,Seq,B,E,Str) :- intron(X),atom(X),featureloc(X,Seq,B,E,Str).
genome_db:cds_dnaseq_pos(X,Seq,B,E,Str) :- cds(X),featureloc(X,Seq,B,E,Str).



/** <module>
  @author Chris Mungall
  @version  $Revision: 1.1 $
  @date  $Date: 2005/10/14 20:16:57 $
  @license LGPL

  ---+ Name
  ---++ seqfeature_bridge_from_fasta
- view layer

  ---+ Synopsis

  ==
  :- use_module(bio(seqfeature_bridge_from_fasta)).

  ==

  ---+ Description

  feature_residues/2 is a view over fastaseq/3 sequences

  featureprop/3 is a view over fastaseq/3 with type 'description'
  
**/
