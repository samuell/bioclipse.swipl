/* -*- Mode: Prolog -*- */

:- module(pkb_web,
          [
           start_server/0,
           start_server/1
          ]).

:- use_module(library('thea2/owl2_model')).
:- use_module(library('thea2/owl2_basic_reasoner')).
:- use_module(pkb_db).
:- use_module(bio(bioprolog_util),[solutions/3]).
:- use_module(bio(tabling),[table_pred/1]).

% pkb uses generic phenotype model
:- use_module(phenotype_db,
              [feature_pair_simnr/6, % typically pre-computed
               phenotype_subsumed_by/2, % required for queries
               phenotype_frequency/2
              ]).
% the following manifests subclass/2 and restriction/2 from Thea, required for phenotype subsumption tests.
% it is assumed you are using Thea/OWL for pkb_web and pkb_db
:- use_module(pkb_to_phenotype).


:- use_module(library('http/thread_httpd')).
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).
:- use_module(library('http/http_parameters')).

:- op(800, xfy, foreach).
foreach(Template, Goal, In, Rest) :-
        findall(Val,(Goal,phrase(Template,Val,[])),Vals),
        flatten(Vals,ValsF),
        append(ValsF,Rest,In).

:- table_pred(owl2_basic_reasoner:entailed/1).

:- multifile http:location/3.
:- dynamic   http:location/3.

http:location(www, /, []).
http:location(pkb, '/pkb', []).
http:location(script, www(script), []).
http:location(css, www(css), []).

background:-
        repeat,
        fail.

start_server :-
        start_server(9000).

start_server(Port) :-
        http_server(http_dispatch, [port(Port)]).

:- http_handler(pkb(.), root, []).
:- http_handler(pkb(organisms), all_organisms, []).
:- http_handler(pkb(phenotypes), all_phenotypes, []).
:- http_handler(pkb(view), view_entity, []).
:- http_handler(pkb(hier), browse_hierarchy, []).
:- http_handler(pkb('organism/'), view_organism, [prefix]).
:- http_handler(pkb('orgtype/'), view_organism_type, [prefix]).
:- http_handler(pkb('compare/'), view_organism_pair, [prefix]).
:- http_handler(pkb('phenoblast/'), phenoblast, [prefix]).
:- http_handler(pkb(phenoquery), phenoquery, []).
:- http_handler(pkb('disease/'), view_disease, [prefix]).
:- http_handler(pkb('diseases/'), all_diseases, [prefix]).
:- http_handler(pkb('class/'), view_class, [prefix]).
:- http_handler(pkb(classes), used_classes, []).

:- http_handler(pkb(js), js_dir, [prefix]).

param(title,     [optional(true)]).
param(name,      [length >= 2 ]).
param(age,       [integer]).
param(url,       [optional(true),default('http://')]).
param(open,      [zero_or_more]).
param(letter,    [zero_or_more]).
param(organism,  [zero_or_more]).
param(entity,    []).
param(action,    [optional(true)]).

% TODO
:- html_resource(script('tabber.js'),
	[	requires([
		    css('tabbers.css')
                         ])
	]).	


js_dir(Request) :-
	http_location_by_id(js_dir, ResRoot),
	memberchk(path(Path), Request),
	atom_concat(ResRoot, File, Path),  % e.g. /img/, hi.jpg, /img/hi.jpg
        atom_concat('js',File,Local), % e.g. img/, hi.jpg, img/hi.jpg
	http_reply_file(Local, [], Request).

js(_URL) -->
        html_post(js,
                  script([type='text/javascript',src='/pkb/js/all.js'])).


root(_Request) :-
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \db_summary
                        ]).

all_organisms(Request) :-
        http_parameters(Request,
                        [ letter(Letters)
                        ],
                        [ attribute_declarations(param)
                        ]),
        Letters=[_|_],
        solutions(Org,
                  (   member(Letter,Letters),
                      atom_codes(A,[Letter]),
                      downcase_atom(A,A2),
                      organism_label(Org,Label),
                      downcase_atom(Label,Label2),
                      sub_atom(Label2,0,1,_,A2),
                      debug(phenotype,'label=~w',[Label])),
                  Orgs),
        !,
        all_organisms_page(Orgs).
all_organisms(Request) :-
        http_parameters(Request,
                        [ action(Action),
                          organism(Orgs)
                        ],
                        [ attribute_declarations(param)
                        ]),
        debug(phenotype,'act: ~w orgs: ~w',[Action,Orgs]),
        Orgs=[_|_],
        !,
        selected_organisms(Action,Orgs).
all_organisms(_Request) :-
        setof(Org,organism(Org),Orgs),
        length(Orgs,N),
        (   N>200
        ->  organisms_by_label_index_page
        ;   all_organisms_page(Orgs)).

all_organisms_page(Orgs) :-
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \db_summary,
                          \organism_list(Orgs)
                        
                        ]).

% split list because too many orgs
organisms_by_label_index_page :-
        findall(Letter,
                 (   between(1,26,N),
                     Code is N+64,
                     atom_codes(Letter,[Code])),
                 Letters),
        findall(li(a(href(location_by_id(all_organisms)+'?'+'letter='+Letter),Letter)),
                 member(Letter,Letters),
                LetterList),
        reply_html_page([ title('OBD-PKB'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          \html_receive(js)
                        ],
                        [ \page_header('Main'),
                          \db_summary,
                          ul(LetterList)]).



selected_organisms(compare,Orgs) :-
        debug(phenotype,'comparing: ~w',[Orgs]),
        solutions(P,
                  (   (   member(O,Orgs)
                      ;   member(O,Orgs)),
                      organism_phenotype(O,P)),
                  Ps),
        debug(phenotype,'Ps: ~w',[Ps]),
        solutions(C,
                  (   member(P,Ps),
                      class_relevant_phenotype(C,P,_)),
                  Cs),
        debug(phenotype,'Cs: ~w',[Cs]),
        subsumed_by_lca_set(Cs,CAs),
        % TODO: speed this up
        class_hrefs_indented(CAs,CMap),
        debug(phenotype,'CAs: ~w',[CAs]),
        findall(th(\organism_href(O)),
                member(O,Orgs),
                HdrCols),
        findall(tr([td(CInfo)|ColVals]),
                (   member(C-CInfo,CMap),
                    findall(td(\phenotype_infos(SubPs)),
                            (   member(O,Orgs),
                                solutions(P,
                                          (   organism_phenotype(O,P),
                                              class_relevant_phenotype(C,P,_)),
                                          SubPs)),
                            ColVals)),
                Rows),
        reply_html_page([ title('Compare Multiple Organisms'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Compare Multiple Organisms'),
                          h2('Compare Multiple Organisms'),
                          table(class(std_table),
                                [tr([td('Class')|HdrCols])
                                |
                                Rows])]).

% defaults to view
selected_organisms(_,Orgs) :-
        setof(Org-P,(member(Org,Orgs),organism_phenotype(Org,P)),OrgPs),
        reply_html_page([ title('Selected Organism Phenotypes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Selected Organism Phenotypes'),
                          h2('Selected Organism Phenotypes'),
                          html(table(class('sortable std_table'),
                                     [\organism_phenotype_tblhdr,
                                      \organism_phenotype_rows(OrgPs,[])
                                     ]))
                        ]).

class_hrefs_indented(Cs,CMap) :-
        solutions(C,
                  (   member(C,Cs),
                      \+ ((member(C2,Cs),
                           C2\=C,
                           subsumed_by(C,C2)))),
                  RootCs),
        class_hrefs_indented(RootCs,Cs,'',CMap).

class_hrefs_indented([],_,_,[]).
class_hrefs_indented([C|Roots],Cs,I,[C-span([I,\class_info(C)]) | CMap]) :-
        !,
        solutions(SubC,
                  (   member(SubC,Cs),
                      proper_subsumed_by(SubC,C),
                      \+ ((member(SubC2,Cs),
                           proper_subsumed_by(SubC2,C),
                           proper_subsumed_by(SubC,SubC2)))),
                  SubCs),
        debug(phenotype,'C: ~w subCs: ~w',[C,SubCs]),
        atom_concat('>',I,I2),
        class_hrefs_indented(SubCs,Cs,I2,CMap1),
        solutions(DC,member(DC-_,CMap1),DCs),
        subtract(Cs,DCs,NextCs),
        class_hrefs_indented(Roots,NextCs,I,CMap2),
        append(CMap1,CMap2,CMap).

        
all_phenotypes(_Request) :-
        reply_html_page([ title('Phenotypes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Phenotypes'),
                          h2('All phenotypes'),
                          \organism_phenotype_list
                        ]).

all_diseases(_Request) :-
        solutions(S,(species(S),
                     \+ \+ pkb_db:inferred_organism_role_disease_species(Model,model,Disease,S)),
                  SL),
        findall(th(\organism_type_href(S)),
                member(S,SL),
                SpHdrs),
        findall(tr([td(\disease_href(Disease)),td(Desc),td(NumOrgs),td(NumPhenotypes),td(Models)|SpeciesModelVals]),
                (   disease_description(Disease,Desc),
                    aggregate(count,Org,Role^organism_role_disease(Org,Role,Disease),NumOrgs),
                    aggregate(count,P,disease_phenotype(Disease,P),NumPhenotypes),
                    solutions(\organism_href(Model),
                              inferred_organism_role_disease(Model,model,Disease),
                              Models),
                    findall(td(\organism_hrefs(ModelsBySpecies)),
                            (   member(S,SL),
%                                solutions(div([\organism_href(Model)]),
%                                          pkb_db:inferred_organism_role_disease_species(Model,model,Disease,S),
%                                          ModelsBySpeciesVal)),
                                solutions(Model,
                                          pkb_db:inferred_organism_role_disease_species(Model,model,Disease,S),
                                          ModelsBySpecies)
                            ),
                            SpeciesModelVals)
                ),
                Rows),
        reply_html_page([ title('Diseases'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Diseases'),
                          h2('All Diseases'),
                          p(class(info),
                            'this table shows all diseases with organismal phenotype data associated. Click on column headings to sort.'),
                          table(class('sortable std_table'),
                                [tr([th('Disease'),th('Description'),th('Organisms'),th('Phenotypes'),th('Inferred Models')|SpHdrs])|
                                 Rows])
                        ]).

organism_hrefs(Orgs) -->
        multi(div(\organism_href(X)),X,Orgs).

used_classes(_Request) :-
        solutions(C,(organism_phenotype(_,P),class_relevant_phenotype(C,P,_),class(C)),Cs),
        length(Cs,NumCs),
        debug(phenotype,'Classes=~w',[NumCs]),
        findall(tr([td(Ont),
                    td(SuperClassCol),
                    td(\class_info(C)),
                    td(NumPhenotypes)]),
                (   member(C,Cs),
                    class_ontology(C,Ont),
                    % TODO: speed this up?
                    %aggregate(count,
                    %         P,
                    %         Org^Aspect^class_relevant_organism_phenotype(C,Org,P,Aspect),
                    %         NumPhenotypes),
                    NumPhenotypes=0,
                    solutions(\class_info(Super),
                              (   subClassOf(C,Super),
                                  class(Super)),
                              SuperClassCol)),
                Rows),
        reply_html_page([ title('Ontology Classes'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Ontology Classes'),
                          h2('Classes used in phenotype descriptions'),
                          table(class('sortable std_table'),
                                [tr([th('Ontology'),th('Superclass'),th('Class'),th('Phenotypes')])
                                |
                                 Rows])
                        ]).


browse_hierarchy(Request) :-
        http_parameters(Request,
                        [ open(OpenClasses)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title('Browser'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])

                        ],
                        [ \page_header('Ontology browser'),
                          h2('Browser'),
                          p(class(info),'NOT READY YET!!'),
                          \subclass_tree(OpenClasses)
                        ]).



phenoquery(Request) :-
        http_parameters(Request,
                        [
                         bearer(E, [optional(true),default(-)]),
                         quality(Q, [optional(true),default(-)]),
                         towards(D, [optional(true),default(-)]),
                         anatomical_context(W, [optional(true),default(-)])
                        ],
                        [
                         attribute_declarations(param)
                        ]),
        P=(E,Q,D,W),
        debug(phenotype,'Query=~w',[P]),
        findall(Org-P1,
                (   organism_phenotype(Org,P1),
                    phenotype_subsumed_by(P1,P)),
                OrgPs),
        debug(phenotype,'Results=~w',[OrgPs]),
        reply_html_page([ title('Phenotype Query'),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])
                        ],
                        [ \page_header('Phenotype Query'),
                          form(id(phenoquery),
                               [
                                \ontol_selectbox(anatomical_context,W),
                                \ontol_selectbox(bearer,E),
                                \ontol_selectbox(quality,Q),
                                \ontol_selectbox(towards,D),
                                input([name(submit),type(submit),value(query)])
                               ]),
                          h3('Current query:'),
                          table(\phenotype_rows([(E,Q,D,W)])),
                          table(class('sortable std_table'),
                                [\organism_phenotype_tblhdr,
                                 \organism_phenotype_rows(OrgPs,[])
                                ])
                        ]).

ontol_selectbox(Param,Class) -->
        {debug(phenotype,' param(~w)=~w',[Param,Class]),
         %solutions(SuperClass,subsumed_by(Class,SuperClass),SuperClasses),
         solutions(SuperClass,(subClassOf(Class,SuperClass),class(SuperClass)),SuperClasses),
         solutions(SubClass,(subClassOf(SubClass,Class),class(SubClass)),SubClasses),
         append(SuperClasses,[Class|SubClasses],AllClasses),
         findall(option([value(X)|Selected],[XN]),
                 (   member(X,AllClasses),
                     (   X=Class
                     ->  Selected=[selected(yes)]
                     ;   Selected=[]),
                     display_label(X,XN)),
                 Opts)},
        html(select([name(Param)],
                    Opts)).



% show detail on an organism
view_organism(Request) :-
        request_path_local(Request,view_organism,Org),
        (   organism_label(Org,Label)
        ->  true
        ;   Label=Org),
        organism_type(Org,Type),
        (   organism_description(Org,Desc) -> true ; Desc='?'),
        solutions(P,organism_phenotype(Org,P),Phenotypes),
        reply_html_page([ title(['Organism ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Organism'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Org)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Description'),td(Desc)]),
                                tr([td('Type'),td(\organism_type_href(Type))])
                                ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2('Phenotypes'),
                                   table([class('sortable std_table')],
                                         [\phenotype_tblhdr,
                                          \phenotype_rows(Phenotypes)])]),
                              div(class(tabbertab),
                                  [h2('Similar organisms'),
                                   \phenoblast_table(Org)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(Org)])

                             ])
                        ]).

% strain etc 
view_organism_type(Request) :-
        request_path_local(Request,view_organism_type,OrgType),
        view_organism_type(Request,OrgType).

view_organism_type(_Request,OrgType) :-
        display_label(OrgType,Label),
        solutions(Org-P,(organism_inferred_type(Org,OrgType),
                         organism_phenotype(Org,P)),OrgPhenotypePairs),
        reply_html_page([ title(['Organism Type ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Organism Type'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(OrgType)]),
                                tr([td('Label'),td(Label)])]),

                         h3('Parent taxa'),
                         \superclass_list(OrgType),
                         h3('Child taxa'),
                         \subclass_list(OrgType),
                         
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2('Phenotypes'),
                                   table([class('sortable std_table')],
                                         [\organism_phenotype_tblhdr,
                                          \organism_phenotype_rows(OrgPhenotypePairs,[])])]),
                              div(class(tabbertab),
                                  [h2('Similar organisms'),
                                   %p(soon)
                                   \phenoblast_table(OrgType)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(OrgType)])
                             ])
                        ]).



phenoblast(Request) :-
        request_path_local(Request,phenoblast,Org),
        (   organism_label(Org,Label)
        ->  true
        ;   Label=Org),
        reply_html_page([ title(['Phenoblast ',Label]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])

                        ],
                        [
                         \page_header('Phenoblast'),
                         \phenoblast_table(Org)
                        ]).

view_organism_pair(Request) :-
        request_path_local(Request,view_organism_pair,OrgPairAtom),
        concat_atom([Q,S],' ',OrgPairAtom),
        format(user_error,'comparing ~w vs ~w~n',[Q,S]),
        reply_html_page([ title(['Organism Phenotype Comparison']),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[])

                        ],
                        [
                         \page_header('Comparison'),
                         \comparison_table(Q,S)
                        ]).

superclass_list(Class) -->
        {solutions(li(\class_info(Super)),subClassOf(Class,Super),List)},
        html(ul(List)).

subclass_list(Class) -->
        {solutions(li(\class_info(Sub)),subClassOf(Sub,Class),List),
         debug(phenotype,'sc ~w => ~w',[Class,List])},
        html(ul(List)).

comparison_table(F1,F2) -->
        {feature_pair_simnr(F1,F2,P1Xs,P2Xs,_MaxIC,_ICCS),
         append(P1Xs,P2Xs,PXs),
         setof(IC-P,P1^P2^member(P1-P2-P-IC,PXs),ICPsRev),
         reverse(ICPsRev,ICPs),
         findall(\phenoblast_summary(P,IC,P1Xs,P2Xs),
                 member(IC-P,ICPs),
                 Sections)
         },
        html(table(border(1),
                   [tr([th(\organism_href(F1)),
                        th(' '),
                        th(\organism_href(F2))])
                   |
                   Sections])).

phenoblast_summary(P,IC,P1Xs,P2Xs) -->
        {solutions(P1,
                   (   member(P1-_-P-_,P1Xs)
                   ;   member(_-P1-P-_,P2Xs)),
                   P1s),
         solutions(P2,
                   (   member(P2-_-P-_,P2Xs)
                   ;   member(_-P2-P-_,P1Xs)),
                   P2s),
         phenotype_frequency(P,Freq)
        },
        html([tr(td([colspan(3),align(center)],
                    \phenotype_info(P))),
              tr(td([colspan(3),align(center)],
                    [IC,' freq:',Freq])),
              tr([td(ul(\phenotype_infos(P1s))),
                  td(''),
                  td(ul(\phenotype_infos(P2s)))])]).

phenotype_infos([]) --> [].
phenotype_infos([H|L]) --> phenotype_info(H),phenotype_infos(L).

db_summary -->
        {aggregate(count,O,organism(O),OC)},
        {aggregate(count,P,O^organism_phenotype(O,P),PC)},
        {aggregate(count,O-P,organism_phenotype(O,P),OPC)},
        html(table(class(foo),
                   [tr([td('Organisms'),td(OC)]),
                    tr([td('Phenotypes'),td(PC)]),
                    tr([td('Annotations'),td(OPC)])])).


phenotype_info(P) -->
        {P=(E,Q,D,W)},
        %{debug(phenotype,'p=~w/~w/~w/~w',[E,Q,D,W])},
        html(div([\class_info(W),
              '/',
              \class_info(E),
              '/',
              \class_info(Q),
              '/',
              \class_info(D),
              ' ( ',
              \phenoquery_href(P),
              ' ) '
             ])).

phenoblast_table(Org) -->
        {solutions(ICCS-(Org,Hit,P1Xs,P2Xs,MaxIC,ICCS),
                   feature_pair_simnr(Org,Hit,P1Xs,P2Xs,MaxIC,ICCS),
                   HitKeyValsR),
         reverse(HitKeyValsR,HitKeyVals),
         findall(\phenoblast_matchrow(Org,Hit,P1Xs,P2Xs,MaxIC,ICCS),
                 member(_-(Org,Hit,P1Xs,P2Xs,MaxIC,ICCS),HitKeyVals),
                 Rows)},
        html(table(class('sortable std_table'),
                   [tr([th('Organism/Type'),
                        th('Type'),
                        th('Species'),
                        th('MaxIC'),
                        th('ICCS')])
                          |
                          Rows])).


phenoblast_matchrow(Org,Hit,_P1Xs,_P2Xs,MaxIC,ICCS) -->
        {(   organism_species(Hit,Sp)
        ->  true
        ;   Sp='-')
        },
        {(   organism_type(Hit,Type)
        ->  true
        ;   Type='-')
        },
        html(tr([td(\organism_href(Hit)),
                 td(\organism_type_href(Type)),
                 td(\organism_type_href(Sp)),
                 td(MaxIC),
                 td(ICCS),
                 td(\organism_pair_href(Org,Hit))])
            ).

view_disease(Request) :-
        request_path_local(Request,view_disease,Disease),
        disease_label(Disease,Label),
        disease_description(Disease,Desc),
        findall(tr([td(\organism_href(Org)),
                    td(Role)]),
                organism_role_disease(Org,Role,Disease),
                OrgRows),
        solutions(P,disease_phenotype(Disease,P),Phenotypes),
        reply_html_page([ title(['Disease ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Disease'),
                         h1(['Disease: ',Label]),
                         table(disease('sortable std_table'),
                               [tr([td('URI'),td(Disease)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Description'),td(Desc)]),
                                te([td('Neurolex'),
                                    td(\neurolex_href(Disease))])
                               ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                [h2('Organisms'),
                                 p(class(info),'the following organisms are associated with this disease. All column headings are sortable'),
                                 table(class('sortable std_table'),
                                       [tr([th('Organism'),th('Role')])
                                       |
                                       OrgRows
                                        ])
                                ]),
                              div(class(tabbertab),
                                  [h2('Phenotypes'),
                                   p(class(info),'the following phenotypes are associated with this disease (via an organism). All column headings are sortable'),
                                   table(class('sortable std_table'),
                                         [\phenotype_tblhdr,
                                          \phenotype_rows(Phenotypes)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Info'),
                                   table([
                                         \class_annotation_assertions(Disease),
                                         \class_property_assertions(Disease)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   p('This tab allows you to view the underlying OWL (expert use only)'),
                                   \axiom_infos(Disease),
                                   \axiom_infos_references(Disease)
                                   ])
                             ])
                        ]).

view_class(Request) :-
        request_path_local(Request,view_class,Class),
        view_class(Request,Class).

% redirect
view_class(Request,Class) :-
        entailed(subClassOf(Class,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_2')),
        \+ organism(Class),
        !,
        debug(phenotype,'rerouting to ~w',[Class]),
        view_organism_type(Request,Class).

        

view_class(_Request,Class) :-
        (   labelAnnotation_value(Class,Label)
        ->  true
        ;   Label=Class),
        debug(phenotype,'fetching relevant organisms for ~q',[Class]),
        solutions(Org-P-Aspect,class_relevant_organism_phenotype(Class,Org,P,Aspect),OrgPsA),
        solutions(P-Aspect,member(_-P-Aspect,OrgPsA),Map),
        solutions(Org-P,member(Org-P-_,OrgPsA),OrgPs),
        debug(phenotype,'fetching class assertions for ~q',[Class]),
        solutions(tr([td(\entity_info(I)),td(\class_info(AssertedClass))]),
                  (   entailed(classAssertion(Class,I)),
                      classAssertion(AssertedClass,I)),
                  InstRows),
        debug(phenotype,'rendering ~q',[Class]),
        reply_html_page([ title(['Class ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Ontology Class'),
                         h1(['Ontology Class: ',Label]),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Class)]),
                                tr([td('Label'),td(Label)]),
                                tr([td('Neurolex'),
                                    td(\neurolex_href(Class))])
                               ]),
                         h3('Parent classes'),
                         \superclass_list(Class),
                         div(class(tabber),
                             [div(class(tabbertab),
                                [h2('Phenotypes'),
                                 table(class('sortable std_table'),
                                       [\organism_phenotype_tblhdr,
                                        \organism_phenotype_rows(OrgPs,Map)
                                       ])
                                ]),
                              div(class(tabbertab),
                                  [h2('Info'),
                                   table(class('sortable std_table'),
                                         [
                                         \class_annotation_assertions(Class),
                                         \class_property_assertions(Class)])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Child classes'),
                                   \subclass_list(Class)]),
                              div(class(tabbertab),
                                  [h2('Instances'),
                                   p(class(info),'Inferred instances'),
                                   table(class('sortable std_table'),
                                         [tr([th('Instance'),
                                              th('Type (asserted)')])
                                         |
                                         InstRows])
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Diseases'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   \axiom_infos(Class),
                                   \axiom_infos_references(Class)
                                   ])
                             ])
                        ]).
        /*
        solutions(Aspect,member(_-_-Aspect,OrgPsA),Aspects),
        solutions(div(class(x),
                      [h3(Aspect),
                       table(class(sortable),
                             \organism_phenotype_rows(OrgPs))]),
                  (   member(Aspect,Aspects),
                      solutions(Org-P,
                                member(Org-P-Aspect,OrgPsA),
                                OrgPs)),
                  Divs),
        reply_html_page([ title(['Class ',Label])
                        ],
                        [ \class_detail(Class),
                          h2('phenotypes of relevance')|Divs]).
          */
        

% redirect
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        class(E),
        !,
        view_class(Request,E).
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        classAssertion(_,E),
        !,
        view_instance(Request,E).
view_entity(Request) :-
        http_parameters(Request,
                        [ entity(E)
                        ],
                        [ attribute_declarations(param)
                        ]),
        reply_html_page([ title(E)],
                        [ h2('Axioms'),
                          \axiom_infos(E),
                          h2('Referenced in'),
                          \axiom_infos_references(E)
                        ]).

view_instance(_Request,Instance) :-
        (   labelAnnotation_value(Instance,Label)
        ->  true
        ;   Label=Instance),
        findall(tr([td('Type'),td(\class_info(C))]),
                classAssertion(C,Instance),
                TypeRows),
        reply_html_page([ title(['Instance ',Label]),
                          script([type='text/javascript',src='/pkb/js/sorttable.js'],[]),
                          script([type='text/javascript',src='/pkb/js/tabber.js'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/obd-main.css'],[]),
                          link([rel=stylesheet,type='text/css',href='/pkb/js/tabber.css',media=screen],[])
                        ],
                        [
                         \page_header('Instance'),
                         h1(['Ontology Instance: ',Label]),
                         p('You have burrowed down to the instance level. This is intended for debugging only.'),
                         table(class('sortable std_table'),
                               [tr([td('URI'),td(Instance)]),
                                tr([td('Label'),td(Label)])
                               |
                               TypeRows
                               ]),
                         div(class(tabber),
                             [div(class(tabbertab),
                                  [h2('Info'),
                                   table( \class_annotation_assertions(Instance)),
                                   table( \class_property_assertions(Instance))
                                  ]),
                              div(class(tabbertab),
                                  [h2('Similar'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Diseases'),
                                   p(soon)
                                  ]),
                              div(class(tabbertab),
                                  [h2('Axioms'),
                                   h3('About'),
                                   \axiom_infos(Instance),
                                   h3('References'),
                                   \axiom_infos_references(Instance)
                                   ])
                             ])
                        ]).

class_annotation_assertions(Class) -->
        {solutions(tr([td(\property_href(P)),td(V)]),
                   annotationAssertion(P,Class,literal(type(_,V))),
                   Rows)},
        html(Rows).

class_property_assertions(Class) -->
        {solutions(tr([td(\property_href(P)),td(V)]),
                   propertyAssertion(P,Class,literal(type(_,V))),
                   Rows)},
        html(Rows).

        
phenotype_tblhdr --> html(tr([th('Anatomical Context'),
                              th('Bearer'),
                              th('Quality'),
                              th('Depends on')
                              ])).

phenotype_rows(L) --> multi(phenotype_row,L).

phenotype_row(P) -->
        {debug(phenotype,'phenotype=~w',[P])},
        html(tr(\phenotype_cols(P,[]))).

phenotype_cols(P,Map) -->
        {P=(E,Q,D,W),
         (   member(P-e,Map) -> HE=true ; HE=false),
         (   member(P-q,Map) -> HQ=true ; HQ=false),
         (   member(P-d,Map) -> HD=true ; HD=false),
         (   member(P-w,Map) -> HW=true ; HW=false)
        },
        html(
             [td(\class_info(W,HW)),
              td(\class_info(E,HE)),
              td(\class_info(Q,HQ)),
              td(\class_info(D,HD)),
              td(\phenoquery_href(P))
             ]
            ).

% split list because too many orgs
organisms_by_label_index-->
        {findall(Letter,
                 (   between(1,26,N),
                     Code is N+64,
                     atom_codes(Letter,[Code])),
                 Letters)},
        {findall(li(a(href(location_by_id(all_organisms)+'?'+'letter='+Letter),Letter)),
                 member(Letter,Letters),
                 LetterList)},
        html(div(class(organism_list),
                 ul(LetterList))).

organism_list(Orgs) -->
        html(div(class(organism_list),
                  form([id(organism_list)],
                       [table(class('std_table sortable'),
                              [\organism_tblhdr,
                               \organism_rows(Orgs)]),
                        select(name(action),
                               [option(value(view)),
                                option(value(compare))
                                ]),
                        input([name(submit),type(submit),value(organism_query)])]))).



organism_tblhdr --> html(tr([th(''),
                             th('Organism'),
                             th('Species'),
                             th('Type'),
                             th('Phenotypes'),
                             th('Description')])).

organism_rows(Orgs) --> {findall(\organism_row(O),member(O,Orgs),Rows)},html(Rows).
%organism_rows([]) --> [].
%organism_rows([H|T]) --> !,html([\organism_row(H),\organism_rows(T)]).

organism_row(Org) -->
        {debug(phenotype,'org=~w',[Org])},
        {organism_species(Org,Species) -> true ; Species='?'},
        {(   aggregate(count,P,organism_phenotype(Org,P),NumP)
         ->  true
         ;   NumP = 0)},
        {organism_type(Org,Type)
        ->  true
        ;   Type='?'
        },
        {organism_description(Org,Desc) -> true ; Desc='?'},
        html(tr([td(input([type=checkbox,name=organism,value=Org])),
                 td(\organism_href(Org)),
                 td(\organism_type_href(Species)),
                 td(\organism_type_href(Type)),
                 td(NumP),
                 td(Desc)])).

organism_href(Org) -->
        {organism(Org)},!,
        {organism_label(Org,Label) -> true ; Label=Org},
        html(a(href(location_by_id(view_organism) + encode(Org)),Label)).
organism_href(Org) -->
        {\+ \+ organism_type(_,Org)},!,
        html([\organism_type_href(Org),
              ' [multiple]']).

disease_href(X) -->
        {disease_label(X,Label) -> true ; Label=X},
        html(a(href(location_by_id(view_disease) + encode(X)),Label)).

organism_type_href(OrgType) -->
        {display_label(OrgType,Label)},
        html(a(href(location_by_id(view_organism_type) + encode(OrgType)),Label)).

organism_pair_href(Org,Hit) -->
        html(a(href(location_by_id(view_organism_pair) + encode(Org) + ' ' + encode(Hit)),show)).

phenoblast_href(Org) -->
        html(a(href(location_by_id(phenoblast) + encode(Org)),'Phenoblast')).

phenoquery_href((E,Q,D,W)) -->
        {sformat(E2,'~w',[E]),
         sformat(Q2,'~w',[Q]),
         sformat(D2,'~w',[D]),
         sformat(W2,'~w',[W])},
        html(a(href(location_by_id(phenoquery)+'?bearer='+encode(E2)+'&quality='+encode(Q2)+'&towards='+encode(D2)+'&anatomical_context='+encode(W2)),query)).

neurolex_href(Class) -->
        {(   labelAnnotation_value(Class,Label)
         ->  true
         ;   Label=Class),
         sformat(URL,'http://www.neurolex.org/wiki/Category:~w',[Label])},
        html(a(href(URL),URL)).

% detail
organism_phenotype_list -->
        {setof(Org-P,organism_phenotype(Org,P),OrgPs),
         length(OrgPs,NumOPs),
         debug(phenotype,'ops=~w',[NumOPs]),
         NumOPs < 2000},
        html(table(class('sortable std_table'),
                   [\organism_phenotype_tblhdr,
                    \organism_phenotype_rows(OrgPs,[])
                   ])
            ).
organism_phenotype_list -->
        html(h2('too many annotations')).

organism_phenotype_tblhdr --> html(tr([th('Organism'),
                                       th('Type'),
                                       th('Anatomical Context'),
                                       th('Entity'),
                                       th('Bearer'),
                                       th('Quality')])).
        
organism_phenotype_rows([],_) --> [].
organism_phenotype_rows([Org-P|T],Map) --> !,html([\organism_phenotype_row(Org,P,Map),\organism_phenotype_rows(T,Map)]).

organism_phenotype_row(Org,P,Map) -->
        {organism_label(Org,Label) -> true ; Label=Org},
        %{organism_role(Org,Role) -> true ; Role='?'},
        {organism_type(Org,Type)
        ->  (   labelAnnotation_value(Type,TypeLabel)
            ->  true
            ;   TypeLabel=Type)
        ;   Type='',TypeLabel='?'
        },
        {organism_description(Org,Desc) -> true ; Desc='?'},
        html(tr([td([a(href(location_by_id(view_organism) + encode(Org)),Label),' ',p([id(Org),class(def)],Desc)]),
                 td(\class_info(Type)),
                 \phenotype_cols(P,Map)
                 ])).


class_detail(Class) -->
        {labelAnnotation_value(Class,Label) -> true ; Label=Class},
        html(div(class(detail),
            [table(class(sortable),
                   [tr(td(id),td(Class)),
                    tr(td(label),td(Label))]),
             h2('axioms'),
             \axiom_infos(Class)]
           )).

inst_class_info(X) -->
        {classAssertion(Class,X)},
        !,
        html(\class_info(Class)).
inst_class_info(_X) -->
        html('?').

property_href(X) -->
        {display_label(X,Label)},
        !,
        html(a(href(location_by_id(view_entity) + encode(X)),Label)).


class_info(Class,true) --> html(b(\class_info(Class))).
class_info(Class,false) --> html(\class_info(Class)).
        
class_info((-)) --> !,html('-').

class_info(Class) -->
        {atom(Class)},
        {labelAnnotation_value(Class,Label) -> true ; Label=Class},
        html(a(href(location_by_id(view_class) + encode(Class)),Label)).

class_info(intersectionOf(L)) -->
        {select(someValuesFrom(P,Y),L,[X])},
        class_info_simple_gd(X,P,Y),
        !.
class_info(Class) -->
        % generic class expression
        entity_info(Class).

% e.g. nitrated protein
class_info_simple_gd(X,'http://ontology.neuinfo.org/NIF/Backend/BIRNLex-OBO-UBO.owl#birnlex_17',Y) -->
        html([\class_info(Y),' ',\class_info(X)]).

class_info_simple_gd(X,'http://purl.org/obo/owl/OBO_REL#part_of',Y) -->
        html([\class_info(X),' of ',\class_info(Y)]).

axiom_infos(E) -->
        {solutions(li(\axiom_info(A)),axiom_directly_about(A,E),L)},
        html(ul(L)).
axiom_infos_references(E) -->
        {solutions(li(\axiom_info(A)),axiom_directly_references(A,E),L)},
        html(ul(L)).

        




subclass_tree(OpenClasses) -->
        {findall(\subclass_tree(C,OpenClasses),(class(C),\+subClassOf(C,_)),Elts)},
        html([p(open),
              p(OpenClasses)|
              Elts]
            ).

subclass_tree(Class,OpenClasses) -->
        {
         (   member(Class,OpenClasses)
         ->  findall(\subclass_tree(SubClass,OpenClasses),
                     subClassOf(SubClass,Class),
                     ChildElts)
         ;   (   \+ \+ subClassOf(_,Class)
             ->  findall(open(C),member(C,[Class|OpenClasses]),Params),
                 ChildElts=[' ',a(href(location_by_id(browse_hierarchy)+Params),'+')]
             ;   ChildElts=[]))
         },
        html([ul(li([\class_info(Class)|
                     ChildElts]))]).

axiom_info(A) -->
        {A=..[P|L]},
        html([P,'( ',\axiom_args(L),' )']).

axiom_info_old(A) -->
        {A=..[P|L],
         findall(\entity_info(E),member(E,L),EL)},
        html([P,'( ',span(class(args),EL),' )']).

axiom_args([A]) --> html(\entity_info(A)).
axiom_args([A|L]) --> html([\entity_info(A), ', ', \axiom_args(L)]).


%class_info(C) --> entity_info(C).

entity_info(literal(type(_,X))) --> !,html(X).
entity_info(literal(X)) --> !,html(X).


entity_info(C) -->
        {atom(C),
         display_label(C,Label)},
        !,
        html([' ',a(href(location_by_id(view_entity)+'?entity='+encode(C)),Label),' ']).
%        html(a(href(location_by_id(view_entity)+'entity='+encode(C)),C)).

entity_info(L) -->
        {is_list(L),
         findall(\entity_info(E),member(E,L),EL)},
        html(EL).

entity_info(A) -->
        {A=..[P|L]},
        html([P,'( ',\axiom_args(L),' )']).



display_label(C,Label) :- species_label(C,Label),!.
display_label(C,Label) :- labelAnnotation_value(C,Label),!. % TODO - speed this up
display_label(C,Label) :- concat_atom([_,Label],'#',C),!.
display_label(C,C).

page_header(Title) -->
        html(div(class(new_page_header),
                 [
                  a([href(location_by_id(root)),class(new_page_logo)],'OBD'),
                  div(class(full_div_blue),''),
                  div(class(menu_bar),
                      [div(class(menu_current),
                           ['OBD Home',&(nbsp),&(raquo),&(nbsp),Title]),
                       div(class(menu_items),
                           ['< ',
                            a(href(location_by_id(root)),'Home'),
                            ' | ',
                            a(href(location_by_id(all_organisms)),'Organisms'),
                            ' | ',
                            a(href(location_by_id(browse_hierarchy)),'Class Browser'),
                            ' | ',
                            a(href(location_by_id(used_classes)),'Class List'),
                            ' | ',
                            a(href(location_by_id(all_diseases)),'Diseases'),
                            ' | ',
                            a(href(location_by_id(all_phenotypes)),'Phenotypes'),
                            ' >'])
                      ]),
                  div(class(full_div_blue),'')])).

                  
 % HTTP UTIL

request_path_local(Request,Loc,X) :-
        memberchk(path(ReqPath), Request),
	http_location_by_id(Loc, Me),
	atom_concat(Me, X, ReqPath).

% GENERIC UTIL

multi(Goal,Var,L) -->
        {findall(Goal,member(Var,L),Goals)},
        multi(Goals).

multi(P,L) -->
        {findall(\Goal,
                 (   member(X,L),
                     Goal=..[P,X]),
                 Goals)},
        multi(Goals).

multi(Goals) -->
        html(Goals).


% LOGIC

class_ontology(C,O) :- concat_atom([Base,_],'#',C),concat_atom(Parts,'/',Base),reverse(Parts,[O|_]),!.
class_ontology(_,unknown).

% TODO: this recapitulates some phenoblast logic, and does not take parthood into account
class_relevant_organism_phenotype(Class,Org,P,Aspect) :-
        organism_phenotype(Org,P),
        class_relevant_phenotype(C2,P,Aspect),
        subsumed_by(C2,Class).
class_relevant_phenotype(C,(C,_,_,_),e).
class_relevant_phenotype(C,(_,C,_,_),q).
class_relevant_phenotype(C,(_,_,C,_),d).
class_relevant_phenotype(C,(_,_,_,C),w).

% TODO: move to phenotype_db?
subsumed_by(X,Y) :- entailed(subClassOf(X,Y)).
subsumed_by(X,X).

proper_subsumed_by(X,Y) :- subsumed_by(X,Y),X\=Y.

subsumed_by_lca_set(Cs,CAs) :-
        solutions(A,
                  (   member(C,Cs),
                      subsumed_by(C,A),
                      A\='-'),
                  As),
        findall(A,
                (   member(A,As),
                    \+ \+ ((member(C,Cs),
                            subsumed_by(C,A),
                           member(C2,Cs),
                            atomic_subsumed_by_lca(C,C2,A)))),
                CAs).


/** <module> OWL server

---+ Synopsis

  Run this on command line:
==
swipl -g "[prolege_server],[owl2_io],load_axioms('testfiles/wine.owl'),start_server(9000)"
==

Then point your browser at

  http://localhost:9000
  
---+ Details

This is a web front end to the Thea OWL library. It will allow basic ontology browsing and editing.
  
Documentation will be online as part of the server

---+ Status

PRE-ALPHA. HIGHLY INCOMPLETE
  
*/
