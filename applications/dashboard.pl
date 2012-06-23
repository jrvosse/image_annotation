:- module(dashboard, []).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).
:- use_module(components(label)).


:- http_handler(cliopatria(dashboard), http_dashboard, []).

http_dashboard(_Request) :-
	dashboard_page([]).

dashboard_page(_Options) :-
	find_users(Users),
	reply_html_page([title('Tag experiment dashboard')],
			[
			 table(\show_users(Users))
			]).


show_users([]) --> !.
show_users([U|T]) -->
	show_user(U),
	show_users(T).

show_user(U) -->
	{
	 option(id(Uid), U),
	 option(done(Done), U)
	},
	html(tr([td(\rdf_link(Uid)), td(Done)])).


find_users(Users) :-
	findall(User, participant(User), Users0),
	sort(Users0, Users).

participant(User) :-
	current_user(Uid),
	user_property(Uid, user_count(_Number)),
	find_annotated_targets(User, Targets),
	length(Targets, WorksDone),
	User= [
	       id(Uid), done(WorksDone)
	      ].


find_annotated_targets(User, Targets) :-
	findall(Target, annotated_target(User, Target), Targets).

annotated_target(User, Target) :-
	rdf(Annotation, oa:hasTarget, Target, Graph),
	rdf(Annotation, oa:annotator, User, Graph),
	rdf(Commit, gv:graph, Graph),
	\+ rdf(Commit, gv:parent, init).



delete_all_annotations :-
	findall(G, annotation_graph(G), Graphs),
	forall(member(G, Graphs), rdf_unload(G)).

annotation_graph(G) :-
	rdf(_Commit, gv:graph, G),
	rdf_graph(G).

annotation_graph(Commit) :-
	rdf(Commit, gv:graph, _G),
	rdf_graph(Commit).

annotation_graph(heads).

