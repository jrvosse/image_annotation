/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(filter,
	  [ target_goal/3,            % +Goal, +R, +Options
	    filter_to_goal/4,		% +FilterList, +R, -Goal, +Options

	    filter_from_parameters/2,   % +Options, -Filter
	    filter_from_parameters/3,   % +Options, -Filter, -Rest
	    filter_to_parameter/2,	% +FilterList, -JSON
	    filter_to_json/2,
	    json_filter_to_prolog/2
	  ]).

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library(semweb/rdf_label)).

%:- use_module(serql(rdf_optimise)).

:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(lists)).
:- use_module(library(assoc)).
:- use_module(library(settings)).

/*
:- use_module(util(rdf_graph)).
:- use_module(util(rdf_search)).
:- use_module(util(iface_util)).
:- use_module(util(multilingual)).
:- use_module(util(parameters)).
:- use_module(util(rdfs_plus_skos)).
:- use_module(search(graph_search)).
*/

:- use_module(library(http/json_convert)).
:- use_module(library(http/json)).



% hack for rdfs_plus_skos

rdfs_plus_skos(_RMap, _QMap, S, P, O) :-
	rdf_has(S, P, O).


%%	target_goal(+Goal, +R, -URI)
%
%	Succeeds if R passes all Filters.

target_goal(Goal, R, R) :-
	Goal, !.

%%	filter_to_goal(+Filter, +R, -Goal, +Options)
%
%	Goal is a prolog goal of Filter that succeeds if R passes all filters.
%       Allowed filters:
%
%	    * keyword(Keyword)
%	    * type(Class)
%	    * prop(Prop, Value)
%	    * reachable(Prop, TransitiveProp, Concept)
%
%	@see Options are described with rdfs_plus_skos/5

filter_to_goal(Filter, R, Goal, Options) :-
	%resource_ext_map(Options, RMap),
	%query_ext_map(Options, QMap),
	RMap = 1,
	QMap = 1,
	filter_to_goal(Filter, R, RMap, QMap, Goal, Options).

filter_to_goal(Filter, R, RMap, QMap, Goal, _Options) :-
	is_list(Filter), !,
	(   Filter = []
	->  Goal = true
	/*;   select(keyword(Keyword), Filter, Filter1)
	->  search_filter(Filter1, SearchFilter),
	    graph_search(Keyword, State, [filter(SearchFilter)|Options]),
	    rdf_search_property(State, state_targets(Assoc)),
	    Goal = gen_assoc(R, Assoc, _)*/
	;   filter_to_conj_goal(Filter, R, RMap, QMap, Goal)
	).
filter_to_goal(Filter, R, RMap, QMap, Goal, _) :-
	filter(Filter, R, RMap, QMap, Goal).

filter_to_goal(Filter, R, RMap, QMap, Goal) :-
	filter(Filter, R, RMap, QMap, Goal).

search_filter([], [type(Class)]) :- !,
	setting(search:basic_search_target, Class).
search_filter(Filter, Filter).

%%	filter_to_conj_goal(+FilterList, +R, -Goal)
%
%	Goal is conjuctive prolog goal for FilterList.

filter_to_conj_goal([Filter], R, RMap, QMap, Goal) :- !,
	filter(Filter, R, RMap, QMap, Goal).
filter_to_conj_goal([Filter|T], R, RMap, QMap, (Goal,Rest)) :-
	filter(Filter, R, RMap, QMap, Goal),
	filter_to_conj_goal(T, R, RMap, QMap, Rest).

%%	filter_to_conj_goal(+FilterList, +R, RMap, QMap, -Goal)
%
%	Goal is disjunctive prolog goal for FilterList.

filter_to_disj_goal([Filter], R, RMap, QMap, Goal) :- !,
	filter(Filter, R, RMap, QMap, Goal).
filter_to_disj_goal([Filter|T], R, RMap, QMap, (Goal;Rest)) :-
	filter(Filter, R, RMap, QMap, Goal),
	filter_to_disj_goal(T, R, RMap, QMap, Rest).

%%	filter(+Filter, -Goal, ?R)
%
%	Goal is a prolog goal that succeeds if R passes Filter.

filter(or(Filter), R, RMap, QMap, Goal) :- !,
	filter_to_disj_goal(Filter, R, RMap, QMap, Goal).
filter(or(F1, F2), R, RMap, QMap, Goal) :- !,
	Goal = (G1 ; G2),
	filter_to_goal(F1, R, RMap, QMap, G1),
	filter_to_goal(F2, R, RMap, QMap, G2).
filter(type(Class), R, _, _, Goal) :- !,
	(   rdf_equal(Class,rdfs:'Resource')
	->  Goal = true
	;   rdf_equal(Type, rdf:type),
	    rdf_equal(SubClass, rdfs:subClassOf),
	    Goal = ( rdf(R,Type,C),
	             rdf_reachable(C,SubClass,Class)
		   )
	).
filter(prop(P, V), R, RMap, QMap, Goal) :- !,
	(   P = all
	->  Goal = rdfs_plus_skos(RMap,QMap, R, _, V)
	;   Goal = rdfs_plus_skos(RMap,QMap, R, P, V)
	).
/*filter(propsearch(P, Search), R, RMap, QMap, Goal) :- !,
	kwd_search:find_literals(Search, Literals, []),
	filter(prop(P, V), R, RMap, QMap, Goal0),
	findall(O, ( member(_-L,Literals),
		     (	 O = literal(L)
		     ;	 rdfs_plus_skos(RMap,QMap, O, rdfs:label, literal(L))
		     )
		   ),
		Vs),
	Goal = ( member(V,Vs),
		 Goal0
	       ).*/
filter(reachable(TransP, C), R, _, _, Goal) :- !,
	Goal = rdf_reachable(R, TransP, C).
filter(reachable(P, TransP, C), R, RMap, QMap, Goal) :- !,
	Goal = ( rdfs_plus_skos(RMap,QMap, R, P, V),
		 rdf_reachable(V, TransP, C)
	       ).
filter(value(V), R, RMap, QMap, Goal) :- !,
	Goal = rdfs_plus_skos(RMap,QMap, R, _, V).
filter(valueOfProp(P), R, RMap, QMap, Goal) :- !,
	Goal = rdfs_plus_skos(RMap,QMap, _, P, R).
filter(valueOfProp(P, Filter), R, RMap, QMap, Goal) :- !,
	Goal = ( rdfs_plus_skos(RMap,QMap, S, P, R),
		 Rest
	       ),
	filter_to_goal(Filter, S, RMap, QMap, Rest).
filter(metadata(Class), R, RMap, QMap, Goal) :- !,
	rdf_equal(Type, rdf:type),
	rdf_equal(SubClass, rdfs:subClassOf),
	Goal = (  rdf(R, Type, C),
		  rdf_reachable(C, SubClass, Class)
	       ;  rdfs_plus_skos(RMap,QMap, S,_,R),
		  rdf(S, Type, C),
		  rdf_reachable(C, SubClass, Class)
	       ).
filter(metaclass(MetaClass), R, _, _, Goal) :- !,
	rdf_equal(Type, rdf:type),
	rdf_equal(SubClass, rdfs:subClassOf),
	Goal = ( rdf(R, Type, C),
		 rdf_reachable(C, SubClass, Class),
		 rdf(Class, Type, MetaClass)
		).
filter(equal(R1), R, _, _, Goal) :- !,
	Goal = rdf_equal(R1, R).
filter(ns(Ns), R, _, _, Goal) :- !,
	Goal = (   ground(R+Ns)
	       ->  sub_atom(R, _, _, _, Ns)
	       ;   rdf_url_namespace(R, Ns)
	       ).
filter(alias(Alias), R, _, _, Goal) :- !,
	Goal = rdf_global_id(Alias:_, R).
filter(group(P, [Value]), R, _, _, Goal) :- !,
	(	Value == other
	->	Goal = (\+ iface_has(0, 0, R, P, Value, _))
	;	Goal = iface_has(0, 0, R, P, Value, _)
	).
filter(owl_satisfies(Range), R, _, _, Goal) :- !,
	Goal = owl_satisfies(Range, R).
filter(Filter, _, _, _, _) :-
	domain_error(filter, Filter).


%%	filter_from_parameters(+OptionList, -Filter) is det.
%%	filter_from_parameters(+OptionList, -Filter, -Rest) is det.
%
%	Filter is a list of filters that occur in the OptionList

filter_from_parameters(Options, Filter) :-
	filter_from_parameters(Options, Filter, _).

filter_from_parameters(Options, Filter, Rest) :-
	filters(Options, Filter0, Rest),
	filter_value_to_literal(Filter0, Filter).

%%	json_filter_to_prolog(+JSONAtom, -Filter)
%
%	Filter is a prolog term representation of the filter in
%	JSONAtom.

json_filter_to_prolog(List, Filter) :-
	list_atom_json_to_term(List, AtomList),
	json_to_prolog(AtomList, Filter0),
	filter_value_to_literal(Filter0, Filter).

list_atom_json_to_term([], []).
list_atom_json_to_term([Atom|As], [Term|Ts]) :-
	atom_json_term(Atom,Term,[]),
	list_atom_json_to_term(As, Ts).


%%	filters(+Options, -Filters, -Rest)
%
%	Directly   takes   some   filters   that     are    defined   in
%	filter_parameter/2 from the OptionList.
%
%	@tbd	Remove old style if SWI-Prolog 5.7.14 is current

filters([], [], []).
filters([O|Os], Filters, Rest) :-
	ground(O),			% non-ground are optional parameters
	filter_parameter(O, F), !,
	(   is_list(F)
	->  append(F, Fs, Filters)
	;   Filters = [F|Fs]
	),
	filters(Os, Fs, Rest).
filters([O|Os], Fs, [O|Rest]) :-
	filters(Os, Fs, Rest).

% New style for post-processing after http_parameters
filter_parameter(filter(List), List).
filter_parameter(query(Keyword), keyword(Keyword)).
filter_parameter(type(Class), type(Class)).
filter_parameter(ns(NS), ns(NS)).
filter_parameter(alias(Alias), alias(Alias)).
% old style for using the raw form-data
filter_parameter(filter=Atom, Term) :-
	atom_json_term(Atom,JSON,[]),
	json_to_prolog(JSON, Term).
filter_parameter(query=Query, keyword(Query)) :- !.
filter_parameter(type=Class, type(Class)) :- !,
	Class \== ''.
filter_parameter(ns=Ns, ns(Ns)) :- !.
filter_parameter(alias=Alias, alias(Alias)) :- !.
filter_parameter(Prop=Value, prop(Prop,Value)) :-
	rdf_current_predicate(Prop).


%%	filter_value_to_literal(+FilterIn, -FilterOut)
%
%	Replaces values with literal terms, in case the value is not a URI.

filter_value_to_literal([], []).
filter_value_to_literal([Filter0|Fs], [Filter|Rest]) :-
	(   Filter0 = prop(Prop,A)
	->  property_atom_to_literal(A, Value),
	    Filter = prop(Prop,Value)
	;   Filter = Filter0
	),
	filter_value_to_literal(Fs, Rest).

property_atom_to_literal(A, V) :-
	(  rdf_subject(A)
	-> V = A
	;  V = literal(A)
	).


%%  filter_to_parameter(+Filter, -JSON)
%
%   JSON is a prolog JSON term from Filter.
%   Uses json_object declarations.

filter_to_parameter([], []).
filter_to_parameter([Filter|T], [Object|Rest]) :-
	Object = json([filter=JSONFilter, label=Label]),
	filter_label(Filter, Label),
	prolog_to_json(Filter, JSONFilter),
	filter_to_parameter(T, Rest).

%%  filter_to_parameter(+Filter, -JSON)
%
%   JSON is a prolog JSON term from Filter.
%   Uses json_object declarations.

filter_to_json([], []).
filter_to_json([Filter|T], [JSONFilter|Rest]) :-
	prolog_to_json(Filter, JSONFilter),
	filter_to_json(T, Rest).

%%	filter_label(+Filter, -Label)
%
%	Label is a human readable description of filter.

filter_label(keyword(A), L) :- !,
	%interface_label(keyword, Keyword),
	Keyword = 'keyword',
	atomic_list_concat([Keyword, ':', A], L).
filter_label(type(Class), L) :- !,
	rdf_display_label(Class, L).
	%iface_label(Class, L).
filter_label(_Term, 'test').



:- json_object
    or(or:_),
    type(type:atom),
    metaclass(metaclass:atom),
    keyword(keyword:atom),
    metadata(metadata:atom),
    metadataOf(metadataOf:atom),
    prop(prop:atom, object:_),
    prop(prop:atom, uri:atom),
    propsearch(prop:atom, text:atom),
    prop(prop:atom),
    value(object:atom),
    valueOfProp(valueOfProp:atom),
    reachable(rel:atom, object:atom),
    reachable(prop:atom, rel:atom, uri:atom),
    group(cluster:atom, values:list),

    literal(value:_) + [type=literal],
    type(type:atom, literal:atom),
    lang(lang:atom, literal:atom).
