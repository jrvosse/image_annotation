:- module(graph_version,
	  [gv_resource_commit/5,
	   gv_resource_head/2,
	   gv_resource_graph/2,
	   gv_move_resource_head/2
	  ]).


:- meta_predicate
	gv_resource_commit(+,+,0,-,-).

%%      gv_resource_commit(+Resource, :Action, +User, -Commit, -Graph)
%
%

gv_resource_commit(Resource, User, Action, Commit, Graph) :-
	get_time(TimeStamp),
	gv_resource_head(Resource, ParentCommit),
	gv_resource_graph(ParentCommit, ParentGraph),
	rdf_bnode(Graph),
	rdf_bnode(Commit),
	copy_rdf_graph(ParentGraph, Graph),
	rdf_transaction(Action),
	rdf_transaction((rdf_assert(Commit, gv:parent, ParentCommit, Commit),
			 rdf_assert(Commit, gv:graph, Graph, Commit),
			 rdf_assert(Commit, dc:creator, User, Commit),
			 rdf_assert(Commit, dc:date, literal(TimeStamp), Commit))),
	gv_move_resource_head(Resource, Commit).

gv_resource_head(Resource, Commit) :-
	rdf(Commit, gv:head, Resource),
	!.
gv_resource_head(_, init).

gv_resource_graph(init, init) :- !.
gv_resource_graph(Commit, Graph) :-
	rdf(Commit, gv:graph, Graph).

gv_move_resource_head(Resource, Commit) :-
	(   rdf(Parent, gv:head, Resource)
	->  rdf_retractall(Parent, gv:head, Resource, heads)
	;   true
	),
	rdf_assert(Commit, gv:head, Resource, heads).



copy_rdf_graph(init, _) :- !.
copy_rdf_graph(From, To) :-
	rdf_transaction(forall(rdf(S,P,O,From),
			       rdf_assert(S,P,O,To))).
