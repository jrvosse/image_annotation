:- module(an_dashboard, []).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).

:- use_module(library(graph_version)).
:- use_module(user(user_db)).
:- use_module(components(label)).
:- use_module(cliopatria(hooks)).

:- http_handler(cliopatria(annotate/dashboard/home), http_dashboard_home, []).
:- http_handler(cliopatria(annotate/dashboard/user), http_dashboard_user, []).

:- setting(annotation:dashboard_admin_only, boolean, true,
	   'Dashboard only for users with admin rights').

cliopatria:menu_popup_order(accurator, 120).
cliopatria:menu_label(accurator,			'Accurator').
cliopatria:menu_item(100=accurator/http_dashboard_home, 'dashboard').
cliopatria:menu_item(110=accurator/http_annotation,     'annotation').

:- multifile
	show_user_annotations//3.

http_dashboard_user(Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	http_parameters(Request, [user(User, [])]),
	user_page(User, []).

http_dashboard_home(_Request) :-
	(setting(annotation:dashboard_admin_only, true)
	-> authorized(admin(dashboard)); true),
	dashboard_page([]).


user_page(User, _Options) :-
	findall(Prop, user_property(User, Prop), Props),
	user_property(User, url(Url)),
	find_actions(Url, Additions, Deletions),
	reply_html_page([title(User)],
			[\html_requires(css('dashboard.css')),
			 table([class('dashboard user props')],
			       \show_user_props(Props)),
			 \show_annotations(User, Additions, Deletions)
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
	% user_property(Uid, user_count(_Number)),
	user_property(Uid, url(URL)),
	find_annotations(URL, Annotations),
	length(Annotations, Done),
	User= [
	       id(Uid), url(URL), done(Done)
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

find_actions(User, Additions, Deletions) :-
	gv_current_branch(Branch),
	(   gv_branch_head(Branch, Head)
	->  find_user_commits(Head, User, [], Commits),
	    partition(is_deletion, Commits, Deletions, Additions)
	;   Additions = [],    % No git repo ...
	    Deletions = []
	).


is_deletion(Commit) :-
	gv_commit_property(Commit, comment(Comment)),
	sub_atom(Comment, 0, _, _, 'rm annotation').

find_user_commits(Commit, User, Accum, Result) :-
	(   gv_commit_property(Commit, committer_name(User))
	->  (   gv_commit_property(Commit, parent(Parent))
	    ->	find_user_commits(Parent, User, [Commit|Accum], Result)
	    ;	Result = [Commit|Accum]
	    )
	;   (   gv_commit_property(Commit, parent(Parent))
	    ->	find_user_commits(Parent, User, Accum, Result)
	    ;	Result = Accum
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
show_user_props([password(_)|Tail]) -->
	show_user_props(Tail).

show_user_props([Prop|Tail]) -->
	{
    Prop =.. [K,V]
},
	html(tr([td(K), td(V)])),
	show_user_props(Tail).

show_annotations(User, A, D) --> show_user_annotations(User, A, D),!.
show_annotations(_, [], []) -->
	html(div([class('warning no_annotations')],['no annotations yet'])).

show_annotations(User, Annotations, Deletions) -->
	{
    maplist(at, Annotations, APairs),
    keysort(APairs, ASorted),
    group_pairs_by_key(ASorted, AGrouped),

    maplist(dt, Deletions, DPairs),
    keysort(DPairs, DSorted),
    group_pairs_by_key(DSorted, DGrouped)
},

  html(table(
	   [class('dashboard an_dashboard_table')],
	   [tr([class(an_dashboard_header)],
	       [th('Target Image'), th('Additions'), th('Deletions')]),
	    \do_show_user_annotations(User, AGrouped, DGrouped)])).

at(Commit, Id-(Commit, Target, AddedTriples)) :-
        (   gv_commit_property(Commit, parent(Parent))
        ->  true
        ;   Parent = null
        ),
        gv_diff(Parent, Commit, Changed, _O1, O2, _Same),
	(   member(Target-AddedTriples, O2)
	;   member(Target-AddedTriples, Changed)
	),
	rdf_equal(ann_ui:tag, AN_TAG),
	member(rdf(Id,_,AN_TAG), AddedTriples).

/*
	(  O2 \= []
        ->  member(Target-AddedTriples, O2)
        ;   Changed \= []
        ->  member(Target-([], AddedTriples), Changed)

        ;   % empty commit bug,
            gv_commit_property(Parent, parent(GrandParent)),
            gv_diff(GrandParent, Parent, PChanged, _, _, _),
            member(Target-(DeletedTriples, AddedTriples), PChanged),
            DeletedTriples \= AddedTriples
        ),
	rdf_equal(ann_ui:tag, AN_TAG),
	(   member(rdf(_,_,AN_TAG), AddedTriples)
	->  Id = Target
	;   (  rdf(Target, oa:hasTarget, MetaTarget)
	    ->	Id = MetaTarget
	    ;	Id = deleted_target
	    )
	).
*/

dt(D, DelTarget-(D,DelTarget,DeletedTriples)) :-
        gv_commit_property(D, parent(Parent)),
        gv_diff(Parent, D, C, [], [], _Same),
        member(DelTarget-(DeletedTriples, _E), C).


do_show_user_annotations(_User, [], []) --> !.

do_show_user_annotations(User, [Id-Annotations|Tail], Deletions) -->
        {
         Annotations = [ H | _],
         H = (Commit, T, Triples),
         rdf_equal(HT, oa:hasTarget),
         rdf_equal(AT, oa:annotated),
         (   member(rdf(A1, HT, T), Triples)
         ->  member(rdf(A1, AT, TimeLit), Triples),
             literal_text(TimeLit, Time),
             parse_time(Time, iso_8601, BeginStamp)
	 ;   gv_commit_property(Commit, committer_date(BeginStampA)),
             atom_number(BeginStampA,BeginStamp)
         ),
         (   selectchk(Id-DelList, Deletions, NewDeletions)
         ->  true
         ;   NewDeletions = Deletions, DelList = []
         ),

         (   Id \= deleted_target
	 ->  http_link_to_id(http_thumbnail, [uri(T)], Href),
	     Image =  [Id, a([href(T)],img([style('width: 100px'),src(Href)]))]
	 ;   Image = Id
	 )

        },
        html(tr([td([style('width: 110px')], Image),
                 td(\do_show_user_annotation(Annotations, Id ,
                                             [start(BeginStamp)])),
                 td(\show_deletions(DelList,[start(BeginStamp)]))
                ])),
                do_show_user_annotations(User, Tail, NewDeletions).

show_deletions([],_) --> !.
show_deletions([H|T], Options) -->
        {
         H = (Commit, _Target, Triples),
         rdf_equal(HB, oa:hasBody),
         rdf_equal(US, ann_ui:unsure),
         member(rdf(A,HB,BodyLit), Triples),
         (   member(rdf(A, US, literal(true)), Triples)
         ->  Unsure = 'not sure'; Unsure = '-'
         ),
         (   gv_commit_property(Commit, comment(FullComment))
         ->  atomic_list_concat([_SystemPart|UserParts], '\n', FullComment),
             atomic_list_concat(UserParts, Comment)
         ;   Comment = '-'
         ),
         literal_text(BodyLit, Body),
         gv_commit_property(Commit, committer_date(Time)),
         atom_number(Time, Stamp),
         option(start(Start), Options),
         Delta is Stamp - Start,
         http_link_to_id(list_resource, [r(Commit)], CommitLink)

        },
        html(div([class(deletion)],
                 a([href(CommitLink)], '~1fs:~w (~w, ~w)'-[Delta,Body,Unsure, Comment]))),
        show_deletions(T, Options).



do_show_user_annotation([],_,_) --> !.
do_show_user_annotation([H|Tail], Id, Options) -->
        {
         H = (_Commit, _Target, Triples),
         rdf_equal(HB, oa:hasBody),
         rdf_equal(AT, oa:annotated),
         rdf_equal(CM, rdfs:comment),
         rdf_equal(RT, rdf:type),
         member(rdf(A, HB, literal(B)), Triples),
         member(rdf(A, AT, TimeLit), Triples),
	 member(rdf(A, RT, Type), Triples),
         rdf_global_id(_:LocalType, Type),
         literal_text(TimeLit, Time),
         parse_time(Time, iso_8601, Stamp),
         option(start(BeginStamp), Options),
         % atom_number(BeginStampA, BeginStamp),
         Delta is Stamp - BeginStamp,
         (   member(rdf(A, CM, literal(Comment)), Triples)
         ->  true; Comment = '-'
         ),
	 (   rdf_subject(A) -> Status = normal; Status = deleted),
         format(atom(Class), 'oa hasBody ~w ~w', [LocalType, Status]),
         http_link_to_id(list_resource, [r(A)], Aref)
        },
        html([div([class(Class)], a([href(Aref)],['~1fs:~w (~w)' - [Delta, B, Comment]]))]),
        do_show_user_annotation(Tail, Id, Options).
