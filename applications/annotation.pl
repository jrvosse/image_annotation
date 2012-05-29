:- module(annotation, []).

% semweb

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).
:- use_module(library(yui3_beta)).

% http libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json_convert)).
:- use_module(library(settings)).
:- use_module(components(label)).
:- use_module(user(user_db)).
:- use_module(user(preferences)).
:- use_module(api(annotation)).

:- rdf_meta
	rdf_has_lang(r,r,-).

/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(annotate), http_annotation, []).


/***************************************************
* settings
***************************************************/

:- setting(min_query_length, integer, 3,
	   'Minimum number of characters that must be entered before a query event will be fired. A value of 0 allows empty queries; a negative value will effectively disable all query events and turn AutoComplete off. ').

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
	{ rdf_has_lang(URI, dcterms:comment, Txt),
	  !
	},
	html(Txt).
html_resource_description(URI) -->
	{ rdf_has_lang(URI, dcterms:description, Txt),
	  !
	},
	html(Txt).
html_resource_description(_) --> !.

html_resource_image(URI) -->
	{ image(URI, Image)
	}, !,
	html(a(href(Image),
	img([ style('max-width:400px; max-height:570px'),
		      src(Image)
		    ])
	      )).
html_resource_image(_) --> !.

% hack
image(R, Image) :-
	rdf_has(Image, 'http://www.vraweb.org/vracore/vracore3#relation.depicts', R).
image(R, Image) :-
	rdf_has(R, 'http://purl.org/collections/nl/rma/schema#imageURL', Image).


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
	  rdf_display_label(URI, Label)
	},
	html([ div(class('annotate-header'),
		   [ h3(Label),
		     \html_annotation_field_desc(URI)
		   ]),
	       input([id(Id), type(text)])
	     ]),
	!.

html_annotation_field_desc(URI) -->
	{ rdf_has_lang(URI, dcterms:comment, Desc)
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
					      autocomplete, 'event-key',
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
%
js_annotation_fields([], _) --> !.
js_annotation_fields([URI|T], Target) -->
	js_annotation_field(URI, Target),
	js_annotation_fields(T, Target).

js_annotation_field(FieldURI, Target) -->
	{ http_location_by_id(http_add_annotation, Add),
	  http_location_by_id(http_remove_annotation, Remove),
	  rdf_global_id(_:Id, FieldURI),
	  user_preference(user:lang, literal(Lang)),
	  setting(min_query_length, MinQueryLength),
	  json_annotation_list(Target, FieldURI, Tags),
	  (   rdf_has_lang(FieldURI, an:source, Source)
	  ->  Config = {
			target:Target,
			field:FieldURI,
			source:Source,
			store: { add:Add,
				 remove:Remove
			       },
			tags:Tags,
			minQueryLength:MinQueryLength,
			resultListLocator: results,
			resultTextLocator: label,
			resultHighlighter: phraseMatch}
	  ;   rdf_has(FieldURI, an:source, RdfList),
	      rdfs_member(literal(lang(Lang, _)), RdfList),
	      rdfs_list_to_prolog_list(RdfList, LiteralList),
	      maplist(literal_text, LiteralList, TextList),
	      prolog_to_json(TextList, Source)
	  ->  Config = {
			target:Target,
			field:FieldURI,
			source:Source,
			store: { add:Add,
				 remove:Remove
			       },
			tags:Tags
		       }
	  ;   Config = {
			target:Target,
			field:FieldURI,
			store: { add:Add,
				 remove:Remove
			       },
			tags:Tags
		       }
	  )
	},
	yui3_plug(one(id(Id)), 'Y.Plugin.Annotation', Config).




rdf_has_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	(   rdf_has(Subject, Predicate, literal(lang(Lang, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(lang(en, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(Text))
	).

