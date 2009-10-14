/* -*- Mode: Prolog -*- */




:- module(web_fetch_ncbi,
          [
           web_search_ncbi/3,
           web_search_ncbi/4,
           web_fetch_ncbi_db_by_ids/2,
           web_fetch_ncbi_omim_by_ids/1,
           web_fetch_ncbi_pubmed_by_ids/1,
           gilist_to_accmap/2
           ]).

:- use_module(bio(io)).
:- use_module(bio(web_fetch)).
:- use_module(bio(xml_transform)).

url(esearch,'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi').
url(efetch,'http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi').
url_param(db).
url_param(cmd).
url_param(term).
url_param(retmax).

%% web_fetch(ncbi,+S,?Items)
%
%  ==
%  web_fetch(ncbi,[service=efetch,db=omim,id=601100,retmode=xml,rettype=full],
%  ==
%
%  Search params:
%
%  service - efetch/esearch
%  db
%  retmode
%  rettype
%  
%  
%  

% (+,+,?,+)
web_fetch:search_term_to_url(ncbi,S1,URLFull,_):-
        select(service=Service,S1,S),
        url(Service,URL),
        maplist(http_get_parameter,S,Params),
        concat_atom(Params,'&',URLSuffix),
        concat_atom([URL,URLSuffix],'?',URLFull).

flatten_ids(IDs,ID):-
        is_list(IDs),
        !,
        concat_atom(IDs,',',ID).
flatten_ids(ID,ID).

db_format(gene,entrezgene).
db_format(X,X):- !.

web_fetch_ncbi_db_by_ids(DB,IDs):-
        flatten_ids(IDs,ID),
        % prefiltering is necessary for homolgene due to ncbi bug
        (   DB=homologene
        ->  Opts=[prefilter(1)]
        ;   Opts=[]),
        web_fetch(ncbi,[service=efetch,db=DB,id=ID,retmode=xml,rettype=full],Items,Opts),
        db_format(DB,Format),
        format_module_xmlmap(Format,Schema,XmlMap),
        ensure_loaded(bio(XmlMap)),
        apply_xmlpred(XmlMap,Items,Terms),
        Schema:maplist(assert,Terms).


%% web_search_ncbi(+DB,+S,?IDs)
%
%  ==
%  web_search_ncbi(homologene,query('BRCA1','gene name'),Results)
%  ==
web_search_ncbi(DB,S,Items):-
        web_search_ncbi(DB,S,Items,[]).
web_search_ncbi(DB,S,Items,Opts):-
        convert_search_term(S,SExpanded),
        web_fetch(ncbi,[service=esearch,db=DB,term=SExpanded|Opts],ResultNode),
        apply_xmlpred(web_fetch_ncbi,ResultNode,Items).
web_fetch_ncbi_omim_by_ids(IDs):- web_fetch_ncbi_db_by_ids(omim,IDs).
web_fetch_ncbi_pubmed_by_ids(IDs):- web_fetch_ncbi_db_by_ids(pubmed,IDs).

convert_search_term(query(S,Field),SOut):-
        sformat(SOut,'~w[~w]',[S,Field]),
        !.
convert_search_term(S,S).

% NCBI sometimes returns things embedded in html, aarggh
xmlpred(html,_,[],translate(head)).
xmlpred(head,_,[],translate(sSearchResult)).
% eSearchResult/IdList/Id
xmlpred(eSearchResult,_,[],
        [let(_C='Count'),
         let(_RM='RetMax'),
         let(_RS='RetStart'),
        translate('IdList')]).
xmlpred('IdList',_,[],
        translate('Id')).
xmlpred('Id',_,ID,
        [let(ID='.')]).

%% gilist_to_accmap(+GIs,?Map) is det
% given a list of GI numbers, return a map from GI numbers to accessions
% @param Map [GI1-Acc1,GI2-Acc2,....]
gilist_to_accmap(GIs,Map):-
        concat_atom(GIs,',',IDAtom),
        web_fetch(ncbi,[service=efetch,db=nucleotide,rettype=acc,id=IDAtom],ResultAtom,[noparse(1)]),
        concat_atom(Accs,'\n',ResultAtom),
        zip(GIs,Accs,Map).

zip([],_,[]):- !.
%zip([],L,_):- throw(lists_must_be_same_size(L)).
zip([A|L],[B|M],[A-B|N]):-
        !,
        zip(L,M,N).






% -------------------- TESTS --------------------
% regression tests to ensure behaviour of module is correct;
% lines below here are not required for module functionality

unittest(test(gi,
            [],
            (   ensure_loaded(bio(web_fetch_ncbi)),
                gilist_to_accmap([2,3,4],Accs),
                writeln(Accs)),
            true)).

unittest(test(omim_cancer,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_search_ncbi(omim,cancer,Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).
unittest(test(homologene_search,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_search_ncbi(homologene,query('BRCA1','gene name'),Results),
                 forall(member(Result,Results),
                        format(' ID: "~w"~n',[Result]))),
            true)).

unittest(test(homologene_BRCA1,
             [],
             (   ensure_loaded(bio(web_fetch_ncbi)),
                 web_fetch_ncbi_db_by_ids(homologene,'5276'),
                 write_biofile(homol_db:pro,_)),
            true)).
/** <module> wrapper for NCBI eutils

  ---+ Synopsis

==
  :- use_module(bio(web_fetch_ncbi)).
==

  ---+ Description

  ---++ EUtils URLs

  http://www.ncbi.nlm.nih.gov/entrez/query/static/linking.html

  http://eutils.ncbi.nlm.nih.gov/entrez/query/static/esearch_help.html
  
  examples

  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=cancer&reldate=60&datetype=edat&retmax=100&usehistory=y

  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=11748933,11700088&retmode=xml  

  by GeneID:
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=homologene&term=373983%5Bgene%20id%5D&retmode=xml
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=homologene&term=BRCA1%5Bgene%20name%5D&retmode=xml
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=homologene&id=5276

  GEO:
  
  http://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=gds&term=GSE[ETYP]&retmax=5000&usehistory=y
  
  ---++ TODO

  sgml2pl complains about lack of DTD
  
**/
