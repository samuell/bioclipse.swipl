:- module(phenoblast,
         []).

:- use_module(phenotype_db).
:- use_module(bio(bioprolog_util),[solutions/3]).

phenotype_db:feature_pair_simnr(F1,F2,P1Xs,P2Xs,MaxIC,ICCS) :-
        setof(P1-P2,(feature_phenotype(F1,P1),feature_phenotype(F2,P2)),PPairs),
        debug(phenoblast,'all pairwise combos = ~w',[PPairs]),
        % all distinct phenotypes in union:
        setof(P1,P2^member(P1-P2,PPairs),P1s),
        setof(P2,P1^member(P1-P2,PPairs),P2s),
        % for each asserted phenotype, find the best match in the opposite set
        findall(P1-PX-PSub-IC,(member(P1,P1s),solutions(PX,member(P1-PX,PPairs),PXs),bestmatch(P1,PXs,PX,PSub,IC)),P1Xs),
        debug(phenoblast,'phenotype-bestmaches set[1]=~w',[P1Xs]),
        findall(P2-PX-PSub-IC,(member(P2,P2s),solutions(PX,member(PX-P2,PPairs),PXs),bestmatch(P2,PXs,PX,PSub,IC)),P2Xs),
        debug(phenoblast,'phenotype-bestmaches set[2]=~w',[P2Xs]),
        % now finding all unique subsumers (with their ICs)
        setof(PSub-IC,P^PX^(   member(P-PX-PSub-IC,P1Xs)
                           ;   member(P-PX-PSub-IC,P2Xs)),
              PSubICPairs),
        debug(phenoblast,'phenotype subsumer-IC pairs=~w',[PSubICPairs]),
        aggregate(sum(IC),PSub,member(PSub-IC,PSubICPairs),TotalIC),
        aggregate(max(IC),PSub,member(PSub-IC,PSubICPairs),MaxIC),
        aggregate(count,PSub,IC^member(PSub-IC,PSubICPairs),Total),
        ICCS is TotalIC/Total.

% bestmatch(+P1,+PXs,?PX,?PSub,?IC) 
bestmatch(P1,PXs,PX,PSub,IC) :-
        debug(phenoblast,'   finding bestmatch for: ~q',[P1]),
        setof(Freq-PX-PSub,(member(PX,PXs),phenotype_lca(P1,PX,PSub),phenotype_frequency(PSub,Freq)),[Freq-PX-PSub|_]),
        freq_ic(Freq,IC).

freq_ic(Freq,IC) :-
        feature_count(Num),
        PVal is Freq/Num,
        IC is -log(PVal)/log(2).

/** <module> computes similarity between features based on phenotypes

Here 'feature' is any phenotype-associated entity - organism, genotype, gene, disease etc.

using this module will cause feature_pair_simnr/3 in the phenotype_db module to be dynamically calculated

uses phenotype_lca/3 in phenotype_db

*/
