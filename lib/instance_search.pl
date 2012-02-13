:- module(instance_search,
	[ instance_search/3,		% +Query, -Hits, +Options
	  feel_lucky_search/2,		% +Query, -URI
	  organize_hits/4		% +Hits, +Query, -Groups, +Options
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


%%	feel_lucky_search(+Query, -URI)
%
%	URI is the highest ranked resource matching Query on label.

feel_lucky_search(Query, URI) :-
	instance_search(Query, Hits, [match(stem)]),
	organize_hits(Hits, Query, [_Group-Hits1], [sort([inlink])]),
	Hits1 = [hit(URI,_,_,_)|_].


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


%%	find_hits(+Query, +LabelList, -Hits, +Options)
%
%	Hits contains terms hit(uri,property,label,info)
%	from which label matches with Query and uri is related
%	to label by property. Property or super property of it
%	is in LabelList.

%	compound query
find_hits(Query, LabelList, Hits, Options) :-
	option(compound(true), Options),
	fuzzy:tokens(Query, Tokens),
	Tokens = [_,_|_],
	!,
	compound_query_hits(Tokens, LabelList, Hits, Options).

%   full query
find_hits(Query, LabelList, Hits, Options) :-
	query_hits(Query, LabelList, Hits, Options).

%%	compound_query_hits(+Query, +LabelList, -Hits, Options)
%
%	Hits contains uris with matching label. Extra Tokens are
%	stored as compound(Tokens) and must be filtered out later.
%	See add_info/3.

compound_query_hits(Tokens, LabelList, Hits, Options):-
	Opt = [distance(false),attributes(LabelList)|Options],
	findall(Hit, compound_hit(Tokens, Hit, Opt), Hits0),
	filter(Hits0, Hits, Options).

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
	),
	(	option(treeRemove(true), Options)
	->	rdf_equal(skos:broader, Relation),
		hits_tree(Hits1, Relation, Tree),
		remove_tree_hits(Tree, Hits)
	;	Hits = Hits1
	),
	!.
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


/***************************************************
* hit organization
***************************************************/

%%  organize_hits(+Hits, +Query, -Groups, +Options)
%
%  Groups grouped list of hits.
%  Operations are defined in Options.
%  We first augment all hits before organizing them,
%  this prevents duplicate augmentation steps.
%  Options are
%		smushing = boolean
%		preferred = property-value
%		sort = list of RDF properties or as defined in plugin
%		merge = list of RDF properties
%		cluster =

organize_hits(Hits, Query, Groups, Options) :-
	option(sort(Sort), Options, []),
	option(sortDirection(Direction), Options, forward),
	option(cluster(Cluster), Options, false),
	option(merge(Merge), Options, []),
	flatten([Sort,Cluster,Merge], Info),
	remove_infoless(Info, InfoTerms),

	infoterm_args(Cluster, InfoTerms, ClusterArgs),
	infoterm_args(Merge, InfoTerms, MergeArgs),
	infoterm_args(Sort, InfoTerms, SortArgs),

	smushing(Hits, Hits1, Options),
	hits_info(Hits1, InfoTerms, Query, Hits2),
	cluster(Hits2, ClusterArgs, Groups1),
	merge(Groups1, MergeArgs, Groups2),
	sort_hits(Groups2, SortArgs, Direction, Groups).
%	top_hits(Hits1, TopHits, TopGroup, Options),
%	append(TopGroups, Groups3, HitsOut).


%%	remove_infoless(+Terms, -InfoTerms)
%
%	InfoTerms is a list of resource info descriptions
%	that should be collected.

remove_infoless([], []).
remove_infoless([Term|T], InfoTerms) :-
	infoless(Term), !,
	remove_infoless(T, InfoTerms).
remove_infoless([Term|T], [Term|Info]) :-
	remove_infoless(T, Info).

infoless(false).
infoless(label).
infoless([]).


%%  infoterm_args(+Terms, +InfoTerms, -ArgList)
%
%   Convert a list of infoterms to a list
%   with argument numbers from infoterm.

infoterm_args(false, _, []).
infoterm_args([], _, []) :- !.
infoterm_args([Term|T], InfoTerms, [Arg|Args]) :- !,
	infoterm_arg(Term, InfoTerms, Arg),
	infoterm_args(T, InfoTerms, Args).
infoterm_args(Term, InfoTerms, [Arg]) :-
	infoterm_arg(Term, InfoTerms, Arg).

infoterm_arg(label, _, label) :- !.
infoterm_arg(prop, _, prop) :- !.
infoterm_arg(Term, InfoTerms, Arg) :-
	nth1(Arg, InfoTerms, Term).


%%  key_info_args(+Hits, +Args, -KeyedHits)
%
%   Key Hits with arguments from info term.

key_info_args([], _, []).
key_info_args([Hit|T], Args, [Key-Hit|Rest]) :-
	maplist(info_arg(Hit), Args, Key),
	key_info_args(T, Args, Rest).


info_arg(hit(_,_,L,_), label, L) :- !.
info_arg(hit(_,P,_,_), prop, P) :- !.
info_arg(hit(_,_,_,I), Arg, Key) :-
    arg(Arg, I, Key).


/***************************************************
* owl:sameAs smushing on hits
***************************************************/

%%  smushing(+HitsIn, -HitsOut, +Options)
%
%   Remove all uris that are sameAs related

smushing(HitsIn, Hits, Options) :-
    option(smushing(true), Options), !,
    same_key(HitsIn, Pairs0, Hits, Tail),
    sort(Pairs0, Pairs),
    group_pairs_by_key(Pairs, Groups),
    smushed_hits(Groups, Tail, Options).
smushing(Hits, Hits, _Options).


%%  same_key(+Hits, -SameAsPairs, -RestHits, -TailofRestHits)
%
%   Add sameAs uri to hit if it exists.
%   RestHits contains all hits without a sameAs uri.

same_key([], [], Rest, Rest).
same_key([Hit|T], [Key-Hit|Pairs], Rest, Tail) :-
    Hit = hit(R, _,_,_),
    representative(R, Key), !,
    same_key(T, Pairs, Rest, Tail).
same_key([Hit|T], Pairs, [Hit|Rest], Tail) :-
    same_key(T, Pairs, Rest, Tail).

representative(R, Represent) :-
	%rdf_equal(skos:exactMatch, P),
	findall(R1, owl_ultra_lite:similar(R, R1), Rs0),
	sort(Rs0, [Represent|_]).


%%  pairs_first_of_value_list(+Pairs, -FirstOfValues)
%
%   FirstOfValues contains all first elements of the values
%   of Pairs.

smushed_hits([], [], _).
/*
smushed_hits([_-Hits|T], [Hit|Rest], Options) :-
    preferred_hit(Hits, R, P, L, I, Options),
    Hit = hit(R, P, L, I),
    smushed_hits(T, Rest, Options).
*/
smushed_hits([_-Hits|T], [Hit|Rest], Options) :-
    preferred_hit(Hits, R, P, L, I, Options),
    Hit = hit(R, P, L, I),
    smushed_hits(T, Rest, Options).


%%	preferred_hit(+Hits, -R,-P,-L,-I, +Options)
%
%	R,P,L and I are the attributes of the preferred hit.

% resource from preferred thesaurus
preferred_hit(Hits, R, P, L, I, Options) :-
    option(preferred(Scheme), Options),
    \+ Scheme = false,
    member(hit(R,P,L,I), Hits),
    atom(P),
    rdf_has(R, skos:inScheme, Scheme),
    rdfs_subproperty_of(P, skos:prefLabel),
    !.
% resource found by preferred label
preferred_hit(Hits, R, P, L, I, _Options) :-
    member(hit(R,P,L,I), Hits),
    atom(P),
    rdfs_subproperty_of(P, skos:prefLabel), !.
% first match
preferred_hit([hit(R,P,L,I)|_], R, P, L, I, _Options).


/***************************************************
* add info
***************************************************/

%%	hits_info(+Hits:hit(r,p,l,i), +Query, +InfoTerms,
%%		-NewHits:hit(r,p,l,i))
%
%	Add additional information to hits.

hits_info([], _, _, []).
hits_info([Hit0|T], Terms, Query, [Hit1|Rest]) :-
	Hit0 = hit(R,P,L,_),
	Hit1 = hit(R,P,L,Info),
	hit_info(Terms, Hit0, Query, Values),
	Info =.. [info|Values],
	hits_info(T, Terms, Query, Rest).

hit_info([], _Hit, _, []).
hit_info([P|Ps], Hit, Query, [Value|Rest]) :-
	(   P == count % hack for facet counts
	->  Hit = hit(_,Value,_,_)
	;   P == prefix
	->  prefix_score(Hit, Query, Value)
	;   P == exact
	->  exact_score(Hit, Query, Value)
	;   Hit = hit(Rs,_,_,_),
	    first(Rs,R),
	    rdf_has(R, P, Value)
	->  true
	;   Hit = hit(_,_,_,List),
	    List \== [],
	    Term =.. [P,Value],
	    memberchk(Term, List)
	->  true
	;   Value = []
	),
	hit_info(Ps, Hit, Query, Rest).

exact_score(hit(Rs,P,L,_), Query, Score) :-
	first(Rs,R),
	(   rdf(R,P,literal(exact(Query), L))
        ->  Score = 0
        ;   Score = 1
        ).
prefix_score(hit(Rs,P,L,_), Query, Score) :-
	first(Rs,R),
	(   rdf(R, P, literal(exact(Query), L))
        ->  Score = 0
        ;   rdf(R, P, literal(prefix(Query), L))
        ->  Score = 1
        ;   Score = 2
        ).

first([R|_],R) :- !.
first(R,R).


/***************************************************
* clustering
***************************************************/

%%	cluster(+Hits, +Args, -Groups)
%
%	Cluster the hits by InfoArgs to Groups.

cluster([], _, []) :- !.
cluster(Hits, [], [all-Hits]) :- !.
cluster(Hits, Args, Groups) :-
	key_info_args(Hits, Args, Pairs0),
	keysort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Groups).


/***************************************************
* merge
***************************************************/

%%	merge(+Groups, +Args, -NewGroups)
%
%	In NewGroups hits are merged according to
%	mergeType from Options.

merge([], _, []) :- !.
merge(Groups, [], Groups) :- !.
merge(Groups, Args, NewGroups) :-
	merge_group_hits(Groups, Args, NewGroups).


merge_group_hits([], _, []).
merge_group_hits([C-Hits0|T], Args, [C-Hits|Rest]) :-
	key_info_args(Hits0, Args, Pairs0),
	keysort(Pairs0, Pairs),
	group_pairs_by_key(Pairs, Grouped),
	merge_hits(Grouped, Hits),
	merge_group_hits(T, Args, Rest).


merge_hits([], []).
merge_hits([_-[Hit]|T], [Hit|Hits]) :-
    merge_hits(T, Hits).
merge_hits([Key-Hits|T], [hit(Key,P,L,Is)|Rest]) :-
    Hits = [hit(_,P,L,_)|_],
    merge_hit_data(Hits, Is),
    merge_hits(T, Rest).


merge_hit_data([], []).
merge_hit_data([hit(_,_,_,I)|Hits], [I|Is]) :-
    merge_hit_data(Hits, Is).


/***************************************************
*
***************************************************/

%%	sort_hits(+Groups, +InfoArgs, +Direction, -NewGroups)
%
%	Sort hits within groups according to value of InfoArgs

sort_hits([], _, _, []) :- !.
sort_hits(Groups, [], _, Groups) :- !.
sort_hits(Groups, Args, Direction, NewGroups) :-
	sort_key_group(Groups, Args, Direction, Sorted0),
	keysort(Sorted0, Sorted1),
	pairs_values(Sorted1, NewGroups).


sort_key_group([], _, _, []).
sort_key_group([Cat-Hits|T], Args, Direction, [Key-(Cat-Sorted)|Rest]) :-
	group_key(Cat, Key),
	sort_key_hits(Hits, Args, KeyedHits),
	keysort(KeyedHits, Sorted0),
	(   Direction == desc
	->  reverse(Sorted0, Sorted)
	;   Sorted = Sorted0
	),
	sort_key_group(T, Args, Direction, Rest).

group_key([], 'ZZZ') :- !.
group_key(all, aaa) :- !.
group_key(Cat, Label) :-
	atom(Cat), !,
	rdf_display_label(Cat, Label).
group_key(Cats, Labels) :-
	maplist(rdf_display_label, Cats, Labels).


%%	sort_key_hits(+Hits, +SortList, -Items:key-item)
%
%	Construct a display item key with a term for sorting.

sort_key_hits([], _, []).
sort_key_hits([Hit|Hits], Args, [Key-Hit|Rest]) :-
	Hit  = hit(_,_,L,Info),
	(   is_list(Info)
	->  Info = [I|_]
	;   I = Info
	),
	sort_keys(Args, L, I, Key),
	sort_key_hits(Hits, Args, Rest).


%%	sort_key(+Args, +Label, +InfoTerm, -Key)
%
%	Key is a list of elements from SortElem
%	as indicated by SortList.

sort_keys([], _, _, []).
sort_keys([Arg|T], L, Info, [Key|Keys]) :-
	sort_key(Arg, L, Info, Key),
	sort_keys(T, L, Info, Keys).

sort_key(label, L, _, L) :- !.
sort_key(Arg, _, Info, Key) :-
	arg(Arg, Info, Elem),
	(   number(Elem)
	->  Key = Elem
	;   atom(Elem)
	->  rdf_display_label(Elem, L),
	    collation_key(L, Key)
	;   Elem = []
	->  L = ''
	;   is_list(Elem)
	->  maplist(rdf_display_label, Elem, L),
	    maplist(collation_key, L, Key)
	;   Key = Elem
	).



/***************************************************
* Create tree of hits
***************************************************/

%%	hits_tree(+Hits, +Property, -Tree)
%
%	Construct hierarchical tree from hits.

hits_tree(Rs, Property, Tree) :- !,
	Tree = node(root,_,_),
	make_tree(Rs, Property, Tree),
	close_tree(Tree).


hit_format(hit(R,P,L,I), R, [hit(R,P,L,I)]).
hit_format(R-I, R, [hit|I]).

%	make_tree(+Hits, +Property, +Attributes, -Tree)
%
%	Tree is of the form node(Resource, Hit, Children)

make_tree([], _, _).
make_tree([Hit|T], P, Tree) :-
	hit_format(Hit, R, Attr),
	in_tree(node(R,Attr,_), Tree), !,
	make_tree(T, P, Tree).
make_tree([Hit|T], P, Tree) :-
	hit_format(Hit, R, Attr),
	parent(R, P, Parent), !,
	add_node(node(Parent,_,Children), P, Tree),
	memberchk(node(R, Attr, _), Children),
	make_tree(T, P, Tree).
make_tree([Hit|T], P, Tree) :-
	hit_format(Hit, R, Attr),
	Tree = node(_,_,Children),
	memberchk(node(R, Attr, _), Children),
	make_tree(T, P, Tree).


add_node(Node, _P, Tree) :-
	in_tree(Node, Tree),
	!.
add_node(Node, P, Tree) :-
	Node = node(R,_,_),
	parent(R, P, Parent), !,
	add_node(node(Parent,_,Children), P, Tree),
	memberchk(Node, Children).
add_node(Node, _P, node(_,_,Children)) :-
	memberchk(Node, Children).


parent(H, P, Parent) :-
	rdf_has(H, P, Parent).


in_tree(Node, Node) :- !.
in_tree(Node, node(_, _, Children)) :-
	in_children(Node, Children).


in_children(_, Var) :-
	var(Var), !,
	fail.
in_children(Node, [H|_]) :-
	in_tree(Node, H), !.
in_children(Node, [_|T]) :-
	in_children(Node, T), !.


close_tree(Var) :-
	var(Var), !,
	Var = [].
close_tree(node(_, A, Children)) :- !,
	close_list(A),
	close_tree(Children).
close_tree([H|T]) :-
	close_tree(H),
	close_tree(T).

close_list([]) :- !.
close_list([_|T]) :-
	close_list(T).


prune_tree(node(R,[Hit],Children), node(R,[Hit],Pruned)) :- !,
	prune_children(Children, Pruned).
prune_tree(node(_,_,[Child]), Pruned) :-
	\+ Child = node(_,[_Hit],_),
	prune_tree(Child, Pruned).
prune_tree(node(R,[],Children), node(R,[],Pruned)) :-
	prune_children(Children, Pruned).

prune_children([], []).
prune_children([Node|T], [SubTree|Rest]) :-
	prune_tree(Node, SubTree),
	prune_children(T, Rest).


leaf_count(node(R, Attr, N0), Count-node(R,Attr,N)) :-
	leaf_count_list(N0, 0, N, CN),
	(	Attr = [_]
	->	Count is CN + 1
	;	Count = CN
	).

leaf_count_list([], C, [], C).
leaf_count_list([Node|T], C0, [C1-CNode|Rest], Count) :-
	leaf_count(Node, C1-CNode),
	C2 is C0 + C1,
	leaf_count_list(T, C2, Rest, Count).






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
