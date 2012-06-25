:- module(an_dashboard, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(user(user_db)).
:- use_module(components(label)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).
:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).

cliopatria:menu_item(90=experiment/http_dashboard, 'Dashboard').

:- multifile
	show_user_annotations//1.

http_dashboard_user(Request) :-
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).


http_dashboard_home(_Request) :-
	dashboard_page([]).


user_page(User, _Options) :-
	findall(Prop, user_property(User, Prop), Props),
	findall(T-B, annotated_target(User, T, B), Annotations),
	reply_html_page([title(User)],
			[style([],['.an_dashboard_table { text-align: right}']),
			 table(\show_user_props(Props)),
			 \show_annotations(Annotations)
			     ]).

dashboard_page(_Options) :-
	find_users(Users),
	length(Users, NrOfUsers),
	reply_html_page([title('Tag experiment dashboard')],
			[
			 div(['Total number of users so far: ', NrOfUsers]),
			 table([
			     tr([th('User id'), th('Number of annotations')]),
			        \show_users(Users)
			       ])
			]).


show_users([]) --> !.
show_users([U|T]) -->
	show_user(U),
	show_users(T).

show_user(U) -->
	{
	 option(id(Uid), U),
	 option(done(Done), U),
	 http_link_to_id(http_dashboard_user, [user(Uid)], UserLink)
	},
	html(tr([td(a([href(UserLink)],['~p'-Uid])),
		 td([class='an_nr_of_annotations'],Done)])).


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
	findall(Target-Body, annotated_target(User, Target, Body), Targets0),
	sort(Targets0, Targets).

annotated_target(User, Target, Body) :-
	rdf(Annotation, oa:hasTarget, Target, Graph),
	rdf(Annotation, oa:annotator, User, Graph),
	rdf(Annotation, oa:hasBody,   literal(Body), Graph),
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

show_user_props([]) --> !.

show_user_props([connection(_,_)|Tail]) -->
	show_user_props(Tail).
show_user_props([allow(_)|Tail]) -->
	show_user_props(Tail).

show_user_props([Prop|Tail]) -->
	{
	 Prop =.. [K,V]
	},
	html(tr([td(K), td(V)])),
	show_user_props(Tail).

show_annotations(L) --> show_user_annotations(L),!.
show_annotations(L) -->
  html(table(
	   [class(an_dashboard_table)],
	   [tr([class(an_dashboard_header)],
	       [th('Target'), th('Tag')]),
	    \do_show_user_annotations(L)])).

do_show_user_annotations([]) --> !.
do_show_user_annotations([T-B|Tail]) -->
	{
	 rdf(A, oa:hasTarget, T),
	 rdf(A, oa:hasBody, literal(B)),
	 http_link_to_id(list_resource, [r(A)], ALink)
	},
	html(tr([td(\rdf_link(T)), td(a([href(ALink)],B))])),
	do_show_user_annotations(Tail).

