:- module(an_dashboard, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(graph_version)).
:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(cliopatria(hooks)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).
:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).

cliopatria:menu_popup_order(accurator, 120).
cliopatria:menu_label(accurator,			'Niche Accurator').
cliopatria:menu_item(100=accurator/http_dashboard_home, 'Dashboard').
cliopatria:menu_item(110=accurator/http_annotation,     'Denice annotation').

:- multifile
	show_user_annotations//3.

http_dashboard_user(Request) :-
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

http_dashboard_home(_Request) :-
	dashboard_page([]).


user_page(User, _Options) :-
	findall(Prop, user_property(User, Prop), Props),
	find_annotations(User, Annotations),
	find_deletions(User, Deletions),
	reply_html_page([title(User)],
			[style([],['.an_dashboard_table { text-align: right}']),
			 table(\show_user_props(Props)),
			 \show_annotations(User, Annotations, Deletions)
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
	find_annotations(Uid, Annotations),
	length(Annotations, Done),
	User= [
	       id(Uid), done(Done)
	      ].


find_annotations(User, Annotations) :-
	findall(A, annotation_by_user(User, A), Anns0),
	maplist(ann_time, Anns0, APairs),
	group_pairs_by_key(APairs, AGrouped),
	keysort(AGrouped, AGroupedS),
	pairs_values(AGroupedS, Values),
	append(Values, Annotations).

ann_time(Ann, Time-Ann) :-
	rdf(Ann, oa:annotated, TimeLit),
	literal_text(TimeLit, Time).

annotation_by_user(User, Annotation) :-
	rdf(Annotation, oa:annotator, User).

find_deletions(User, Deletions) :-
	gv_current_branch(Branch),
	gv_branch_head(Branch, Head),
	find_user_commits(Head, User, [], Commits),
	partition(is_deletion, Commits, Deletions, _Additions).

is_deletion(Commit) :-
	gv_commit_property(Commit, comment(Comment)),
	sub_atom(Comment, 0, _, _, 'rm annotation').


find_user_commits(Commit, User, Accum, Result) :-
	(   \+ gv_commit_property(Commit, parent(Parent))
	->  Result = Accum
	;   (   gv_commit_property(Commit, committer_name(User))
	    ->  find_user_commits(Parent, User, [Commit|Accum], Result)
	    ;   find_user_commits(Parent, User, Accum, Result)
	    )
	).

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

show_annotations(User, A, D) --> show_user_annotations(User, A, D),!.
show_annotations(User, A, D) -->
  html(table(
	   [class(an_dashboard_table)],
	   [tr([class(an_dashboard_header)],
	       [th('Target'), th('Tag')]),
	    \do_show_user_annotations(User, A, D)])).

do_show_user_annotations(_User, [], _) --> !.
do_show_user_annotations(User, [A|Tail], D) -->
	{
	 rdf(A, oa:hasTarget, T),
	 rdf(A, oa:hasBody, literal(B)),
	 http_link_to_id(list_resource, [r(A)], ALink)
	},
	html(tr([td(\rdf_link(T)), td(a([href(ALink)],B))])),
	do_show_user_annotations(User, Tail, D).




