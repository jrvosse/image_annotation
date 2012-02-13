:- module(instance_search,
	[ instance_search/3		% +Query, -Hits, +Options

	]).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_label')).
:- use_module(library('semweb/rdf_litindex.pl')).

:- use_module(library(find_resource)).
:- use_module(library(filter)).

/*
:- use_module(serql(rdf_optimise)).
:- use_module(util(rdf_find_by_name)).
:- use_module(util(fuzzy)).

:- use_module(util(rdfs_plus_skos)).
:- use_module(util(owl_restrictions)).

:- use_module(util(rdf_util)).
:- use_module(util(util)).
:- use_module(util(iface_util)).
:- use_module(util(filter)).
*/


:- http_handler(api(autocomplete), http_autocomplete, [js(true)]).

http_autocomplete(Request) :-
	http_parameters(Request,
			[q(Query,
			   []),
			 limit(Limit,
			       [default(10)]),
			 offset(Offset,
				[default(0)]),
			 method(Method,
				[one_of([prefix,stem,exact]),
				 default(prefix),
				 description('String matching method')
				])
			]),
	Options = [match(Method)],
	instance_search(Query, Hits0, Options),
	length(Hits0, TotalNumberOfResults),
	list_offset(Hits0, Offset, Hits1),
	list_limit(Hits1, Limit, Hits, _),
	prolog_to_json(Hits, JSON),
	reply_json(json([totalNumberOfResults(TotalNumberOfResults),
			 results(JSON)])).

:- json_object
	hit(uri:uri, property:atom, label:atom, info:_).


/***************************************************
* term search
***************************************************/

%%	instance_search(+Query, -Hits:hit(uri,property,label,info), +Options)
%
%	Hits contains all resources matching Query.
%	Options are
%		match = prefix;stem;exact
%		filter =
%		compound = boolean
%		property = [property-score]
%		shortQueriesOptimized = integer
%		treeRemove = false;relation


instance_search(Query, Hits, Options) :-
	option(property(Property), Options, []),
	label_list(Property, LabelList),
	find_hits(Query, LabelList, Hits, Options).


/***************************************************
* literal matching
***************************************************/

%%	label_list(+Property, -LabelList)
%
%	LabelList is a list of Pairs with an rdf property
%	score. Lower score gets preference.

label_list([], LabelList) :- !,
	rdf_equal(rdfs:label, Label),
	rdf_equal(skos:prefLabel, PrefLabel),
	LabelList = [
		PrefLabel-0,
		Label-1
	].
label_list(Property, LabelList) :-
	atom(Property), !,
	LabelList = [Property-0].
label_list(List, LabelList) :-
    is_list(List),
    format_label_list(List, 0, LabelList),
    !.
label_list(List,_) :-
	 type_error(labellist,  List).

format_label_list([], _, []).
format_label_list([H|T], N0, [P-N|Rest]) :-
	N1 is N0+0.1,
	(   atom(H)
	->  N = N0,
	    P = H
	;   H = P-N,
	    number(N)
	->  true
	;   H = pair(P,N)
	->  true
	),
	format_label_list(T, N1, Rest).

find_hits(Query, LabelList, Hits, Options) :-
	query_hits(Query, LabelList, Hits, Options).

%%	query_hits(+Query, +LabelList, -Hits, Options)
%
%	Hits contains uris with prefix matching label.

query_hits(Query, LabelList, Hits, Options0) :-
	Options = [distance(false),attributes(LabelList)|Options0],
	find_resource_by_name(Query, Hits0, Options),
	maplist(ac_hit, Hits0, Hits1),
	filter(Hits1, Hits, Options0).

ac_hit(hit(_D,U,P,L), hit(U,P,L,[])).


%%	compound_hit(+TokenList,
%%		-Hit:hit(score,label,uri,property,info), +Options)
%
%	Same as hit/4, but now the query is a token list.
%	All compound query combinations from the beginning
%	of the list are enumarated.

compound_hit(Tokens, hit(U,P,L,compound(Rest)), Options) :-
	append(Query, Rest, Tokens),
	find_resource_by_name(Query, hit(_,U,P,L), Options),
	filter_compound_hit(Rest, U).

filter_compound_hit([], _).
filter_compound_hit([Token|Ts], R) :-
	rdf_has(R, skos:broader, Parent),
	rdf_reachable(Parent, skos:broader, V),
	rdf_has(V, rdfs:label, literal(prefix(Token,_))),
	filter_compound_hit(Ts, V).


/***************************************************
* filtering
***************************************************/

%%	filter(+Hits, -Filtered, +Options)
%
%	Fitered contains only those hits that
%	satisfy the Filter from Options.

filter(Hits0, Hits1, Options) :-
	(	option(filter(Filter), Options),
		Filter \== []
	->	filter_hits(Hits0, Filter, Hits1, Options)
	;	Hits1 = Hits0
	).
filter(Hits, Hits, _Options).


filter_hits([], _, [], _) :- !.
filter_hits(HitsIn, Filter, HitsOut, Options) :-
	option(rdfs_plus_skos(Reasoning), Options, []),
	filter_to_goal(Filter, R, Goal, Reasoning),
	findall(Hit, (member(Hit, HitsIn),
		      Hit = hit(R,_,_,_),
		      once(Goal)),
		HitsOut).

%%	remove_tree_hits(+Tree, -Hits)
%
%	Hits only contains the top node Hits
%	from Tree.

remove_tree_hits(node(_Cat,[Hit],_Children), [Hit]) :- !.
remove_tree_hits(node(_Cat,[],Children), Hits) :-
	remove_child_hits(Children, Hits).

remove_child_hits([], []).
remove_child_hits([Node|T], Hits) :-
	remove_tree_hits(Node, Hits0),
	remove_child_hits(T, Hits1),
	append(Hits0, Hits1, Hits).




%%	list_offset(+List, +N, -SmallerList)
%
%	SmallerList starts at the nth element of List.

list_offset(L, N, []) :-
	length(L, Length),
	Length < N,
	!.
list_offset(L, N, L1) :-
	list_offset_(L, N, L1).

list_offset_(L, 0, L) :- !.
list_offset_([_|T], N, Rest) :-
	N1 is N-1,
	list_offset_(T, N1, Rest).

%%	list_limit(+List, +N, -SmallerList, -Rest)
%
%	SmallerList ends at the nth element of List.

list_limit(L, N, L, []) :-
	N < 0,
	!.
list_limit(L, N, L, []) :-
	length(L, Length),
	Length < N,
	!.
list_limit(L, N, L1, Rest) :-
	list_limit_(L, N, L1, Rest).

list_limit_(Rest, 0, [], Rest) :- !.
list_limit_([H|T], N, [H|T1], Rest) :-
	N1 is N-1,
	list_limit_(T, N1, T1, Rest).
