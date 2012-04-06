:- module(annotation,
	[annotation_in_field/5,
	 json_annotation_list/3]).


% semweb
:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdfs')).
:- use_module(library('semweb/rdf_label')).
:- use_module(library(yui3_beta)).

% http libraries
:- use_module(library('http/http_dispatch')).
:- use_module(library('http/http_parameters')).
:- use_module(library('http/html_write')).
:- use_module(library('http/html_head')).
:- use_module(library('http/http_path')).
:- use_module(library(http/json)).
:- use_module(library('http/http_json')).
:- use_module(library(http/json_convert)).
:- use_module(components(label)).
:- use_module(library(settings)).
:- use_module(user(user_db)).
:- use_module(library(graph_version)).
:- use_module(api(annotation)).
:- use_module(library(instance_search)).

/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(annotate), http_annotation, []).


/***************************************************
* settings
***************************************************/

:- setting(min_query_length, integer, 3,
	   'Minimum number of characters that must be entered before a query event will be fired. A value of 0 allows empty queries; a negative value will effectively disable all query events and turn AutoComplete off. ').
:- setting(user_restrict, boolean, false,
	   'When set to true only own annotations are shown.').


/***************************************************
* http replies
***************************************************/

%%	http_annotation(+Request)
%
%	Generate page to add and annotate a resource.

http_annotation(Request) :-
	http_parameters(Request,
		[ target(Target,
		     [uri,
		      description('URI of the object to be annotated')
		     ]),
		  field(Fields,
			[zero_or_more,
			 description('URI of annotation field')
			])
		]),
	(   setting(annotation_api:login, true)
        ->  authorized(write(_,_))
        ;   true
        ),
	html_page(Target, Fields).

/***************************************************
* annotation page
***************************************************/

%%	html_page(+Target, +Fields)
%
%	HTML page

html_page(Target, Fields) :-
	rdf_display_label(Target, Title),
	reply_html_page(
	    [ title(['Annotate -- ', Title])
	    ],
	    [ \html_requires(yui3('cssgrids/grids-min.css')),
	      \html_requires(css('annotation.css')),
	      div(class('yui3-skin-sam yui-skin-sam'),
		  [ div(id(hd), []),
		    div(id(bd),
			div([id(layout), class('yui3-g')],
			    [ div([id(fields), class('yui3-u')],
				  \html_annotation_fields(Fields)),
			      div([id(media), class('yui3-u')],
				  \html_resource(Target, Title))
			    ])
		       ),
		    div(id(ft), [])
		  ]),
	      script(type('text/javascript'),
		     \yui_script(Target, Fields))
	    ]).



%%	html_resource(+URI, Title)
%
%
%	Title image and description of the resource being annotated.

html_resource(URI, Title) -->
	html(div(class('resource'),
		 [ div(class(title), h3(Title)),
		   div(class(image), \html_resource_image(URI)),
		   div(class(link), \rdf_link(URI)),
		   div(class('description'),
		       \html_resource_description(URI)
		      )
		 ])).

html_resource_description(URI) -->
	{ rdf_has(URI, dc:comment, Desc),
	  literal_text(Desc, Txt)
	},
	html(Txt).
html_resource_description(_) --> !.

html_resource_image(URI) -->
	{ image(URI, Image)
	}, !,
	html(a(href(Image),
		img([ style('max-width:400px'),
		      src(Image)
		    ])
	      )).
html_resource_image(_) --> !.

% hack
image(R, Image) :-
	rdf_has(Image, 'http://www.vraweb.org/vracore/vracore3#relation.depicts', R).


%%	html_annotation_fields(+FieldURIs)
%
%	Write html for annotation fields.

html_annotation_fields([]) --> !.
html_annotation_fields([URI|T]) -->
	html(div(class('annotate-field'),
		 \html_annotation_field(URI))),
	html_annotation_fields(T).

html_annotation_field(URI) -->
	{ rdf_global_id(_:Id, URI),
	  rdf_label(URI, L),
	  literal_text(L, Label)
	},
	html([ div(class('annotate-header'),
		   [ h3(Label),
		     \html_annotation_field_desc(URI)
		   ]),
	       input([id(Id), type(text)])
	     ]).

html_annotation_field_desc(URI) -->
	{ rdf(URI, dc:comment, D),
	  literal_text(D, Desc)
	},
	!,
	html(div([class('annotate-description')], Desc)).
html_annotation_field_desc(_URI) --> !.



		 /*******************************
		 *	     JavaScript		*
		 *******************************/

%%	yui_script(+Graph)
%
%	Emit YUI object.

yui_script(Target, Fields) -->
	{ findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes)
	},
	yui3([json([modules(json(Modules))])],
	     ['recordset-base'|Includes],
	     [\js_annotation_fields(Fields, Target)]).

js_module('annotation', json([fullpath(Path),
				    requires(['recordset-base',
					      autocomplete,
					      'autocomplete-highlighters',
					      overlay,
					      'io','json',
					      'querystring-stringify-simple'
				  ])
			  ])) :-
	http_absolute_location(js('annotation.js'), Path, []).

%%	js_annotation_fields(+FieldURIs, +AnnotationTarget)
%
%	Write JavaScript to init annotation fields

js_annotation_fields([], _) --> !.
js_annotation_fields([URI|T], Target) -->
	js_annotation_field(URI, Target),
	js_annotation_fields(T, Target).

js_annotation_field(FieldURI, Target) -->
	{ http_location_by_id(http_add_annotation, Add),
	  http_location_by_id(http_remove_annotation, Remove),
	  rdf_global_id(_:Id, FieldURI),
	  setting(min_query_length, MinQueryLength),
	  rdf(FieldURI, an:source, literal(Source)),
	  json_annotation_list(Target, FieldURI, Tags)
	},
	yui3_plug(one(id(Id)),
		  'Y.Plugin.Annotation',
		  {target:Target,
		   field:FieldURI,
		   source:Source,
		   store:{add:Add,
			  remove:Remove
			 },
		   tags:Tags,
		   minQueryLength:MinQueryLength,
		   resultListLocator: results,
		   resultTextLocator: label,
		   resultHighlighter: phraseMatch}).



		 /*******************************
		 *               Utils		*
		 *******************************/

annotation_body(literal(L), literal(L)) :- !.
annotation_body(uri(URI), URI).


%%	json_annotation_list(+TargetURI, +FieldURI, -Annotations)
%
%	Annotation is a list with annotations represented in prolog JSON
%	notation.

json_annotation_list(Target, FieldURI, JSON) :-
	findall(annotation(A, Body, L),
		annotation_in_field(Target, FieldURI, A, Body, L),
		Annotations),
	prolog_to_json(Annotations, JSON).

annotation_in_field(Target, FieldURI, Annotation, Body, Label) :-
	gv_resource_head(Target, Commit),
	gv_resource_graph(Commit, Graph),
	(   setting(user_restrict, true)
	->  logged_on(User, anonymous)
	;   true
	),
	rdf(Annotation, oac:hasTarget, Target, Graph),
	rdf(Annotation, an:annotationField, FieldURI, Graph),
	rdf(Annotation, dcterms:creator, User, Graph),
	rdf(Annotation, oac:hasBody, Body0, Graph),
	rdf(Annotation, dcterms:title, Lit, Graph),
	annotation_body(Body, Body0),
	literal_text(Lit, Label).

:- json_object
	annotation(annotation:atom, body:_, label:atom),
	uri(value:uri) + [type=uri],
	literal(lang:atom, value:_) + [type=literal],
	literal(type:atom, value:_) + [type=literal],
	literal(value:_) + [type=literal].
