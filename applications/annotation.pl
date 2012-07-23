:- module(annotation,
	  [ annotation_page/1,
	    get_anfields/4,
	    get_metafields/3
	  ]).

% semweb

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/rdf_label)).


% http libraries
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/html_head)).
:- use_module(library(http/http_path)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/url_cache)).

:- use_module(library(yui3_beta)).
:- use_module(library(settings)).
:- use_module(components(label)).
:- use_module(user(user_db)).
:- use_module(user(preferences)).

:- use_module(api(annotation)). % needed for http api handlers
:- use_module(media_caching).	% needed for http api handlers
:- use_module(dashboard).

:- rdf_meta
	rdf_has_lang(r,r,-),
	rdf_has_lang(r,r,+,-).

:- multifile
	page_header//1.

/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(annotate), http_annotation, []).


/***************************************************
* settings
***************************************************/

:- setting(min_query_length, integer, 3,
	   'Minimum number of characters that must be entered before a query event will be fired. A value of 0 allows empty queries; a negative value will effectively disable all query events and turn AutoComplete off. ').

:- setting(default_target, uri, 'http://www.public-domain-image.com/plants/flowers/slides/chrysanthemum-flower.jpg', 'Default target object to annotate if none given').

:- setting(default_metadata, list(uri),
	   [ 'http://purl.org/dc/terms/title',
	     'http://semanticweb.cs.vu.nl/annotate/imageURL',
	     'http://semanticweb.cs.vu.nl/annotate/url',
	     'http://purl.org/dc/terms/description'
	   ], 'Default metadata fields to show').

/***************************************************
* http replies
***************************************************/

%%	http_annotation(+Request)
%
%	Generate page to add and annotate a resource.

http_annotation(Request) :-
	setting(default_target, DefaultTarget),
	http_parameters(Request,
		[ target(Target,
		     [uri,
		      default(DefaultTarget),
		      description('URI of the object to be annotated')
		     ]),
		  ui(UI,
		     [ uri,
		       default(''),
		       description('URI of the UI configuration')
		     ]),
		  field(ExtraFields,
			[list(uri),
			 description('URIs of annotation field, adding to UI defs')
			]),
		  metadata(MetaFields,
			   [list(uri),
			    description('URIs of metadata properties to display')
			   ])
		]),
	(   setting(annotation_api:login, true)
        ->  authorized(write(default, annotate))
        ;   true
        ),
	get_anfields(UI, ExtraFields, AnnotationFields, Labels),
	get_metafields(UI, MetaFields, MetadataFields),
	Options = [
		   target(Target),
		   annotation_fields(AnnotationFields),
		   metadata_fields(MetadataFields) | Labels
		  ],
	annotation_page(Options).

get_anfields('', [], Fields, Labels) :-
	rdfs_individual_of(URI, an:'AnnotationUI'),
	get_anfields(URI, [], Fields, Labels).
get_anfields('', Fields, Fields, [title(annotation), done_label(done)]) :-
	Fields = [_|_],
	!.
get_anfields(URI, ExtraFields, Fields,
	     [title(Title),
	      done_label(Done),
	      unsure_label(Unsure)
	     ]) :-
	(   rdf_has(URI, an:fields, RdfList)
	->  rdf_display_label(URI, Title),
	    rdf_has_lang(URI, an:doneLabel, Done, 'done'),
	    rdf_has_lang(URI, an:unsureLabel, Unsure, ''),
	    rdfs_list_to_prolog_list(RdfList, UiFields),
	    append(UiFields, ExtraFields, Fields)
	;   Title = 'Undefined UI configuration: ~p'-[URI],
	    Fields=ExtraFields
	).


get_metafields('', [], Fields) :-
	rdfs_individual_of(URI, an:'AnnotationUI'),
	get_metafields(URI, [], Fields).
get_metafields('', ExtraFields, ExtraFields) :-
	ExtraFields = [_|_],
	!.
get_metafields(URI, ExtraFields, Fields) :-
	(   rdf_has(URI, an:metadata, RdfList)
	->  rdfs_list_to_prolog_list(RdfList, Fields)
	;   setting(default_metadata, UiFields)
	),
	append(UiFields, ExtraFields, Fields).

/***************************************************
* annotation page
***************************************************/

%%	annotation_page(Options)
%
%	HTML page

annotation_page(Options) :-
	option(target(Target), Options, notarget),
	option(annotation_fields(AnFields), Options, []),
	option(footer(Footer), Options, []),
	option(buttons(Buttons), Options, DefaultButtons),
	option(stylesheet(StyleSheet), Options, ''),
	default_buttons(DefaultButtons, Options),

	reply_html_page(
	    [ \annotation_page_header(Options) ],
	    [ \html_requires(yui3('cssgrids/grids-min.css')),
	      \html_requires(css('annotation.css')),
	      \html_requires(StyleSheet),
	      div(class('yui3-skin-sam yui-skin-sam'),
		  [ div(id(hd), []),
		    div(id(bd),
			div([id(layout), class('yui3-g')],
			    [
			      div([id(media), class('yui3-u')],
				  \html_resource(Target, Options)),
			      div([id(fields), class('yui3-u')],
				  [ \html_annotation_fields(AnFields, Options),
				    div([id(anbuttons)], Buttons)
				  ])
			    ])
		       ),
		    div(id(ft), Footer)
		  ]),
	      script(type('text/javascript'),
		     \yui_script(Target, AnFields)),
	      script([type('text/javascript')],
		    \done_script(Options))

	    ]).

annotation_page_header(Options) --> annotation:page_header(Options).
annotation_page_header(Options) -->
	{
	 option(target(Target), Options, notarget),
	 option(title(Title), Options, 'Annotation'),
	 rdf_display_label(Target, TargetLabel)
	},
	html(title([Title, ': ', TargetLabel])).

%%	html_resource(+URI, Options)
%
%
%	Title image and description of the resource being annotated.

html_resource(URI, Options) -->
	{
	 option(metadata_fields(Fields), Options)
	},
	html(div(class('resource'), [ \html_metadata_fields(URI, Fields, Options)])).

html_metadata_fields(_URI, [], _) --> !.
html_metadata_fields(URI, [Field|Tail], Options) -->
	html_metadata_field(URI, Field, Options),
	html_metadata_fields(URI, Tail, Options).

html_metadata_field(URI, Field, _Options) -->
	{
	 rdfs_subproperty_of(Field, an:imageURL)
	},
	html_resource_image(URI).

html_metadata_field(URI, Field, _Options) -->
	{
	 rdfs_subproperty_of(Field, an:url)
	},
	html(div(class(link), \rdf_link(URI))).

html_metadata_field(URI, Field, _Options) -->
	{
	 rdf_has(URI, Field, Object),
	 rdf_display_label(Field, Label),
	 rdf_display_label(Object, Value),
	 rdf_global_id(_:Class, Field)
	},
	html(
	    div([class(Class)],
		[span(class(metalabel), Label),
		 span(class(metavalue), Value)
		]
	       )
	    ).

html_metadata_field(_,_,_) --> !.


html_resource_image(URI) -->
	{ image(URI, Image),
	  http_link_to_id(http_original, [uri(Image)], Href)
	}, !,
	html(a([href(Href), target('_blank')],
	       img([src(Href)])
	      )).
html_resource_image(URI) -->
	{
	 resource_link(URI, Link)
	},
	html(a([href(Link)], ['No image available for ~p' - URI])).

% hack
image(R, Image) :-
	rdf_has(R, an:imageURL, Image).

image(R, Image) :-
	rdf_has(Image, 'http://www.vraweb.org/vracore/vracore3#relation.depicts', R).

image(R,R) :-
	catch(url_cache(R, _, MimeType), _, fail),
	sub_atom(MimeType, 0, 5, _, 'image'),!.

%%	html_annotation_fields(+FieldURIs, +Options)
%
%	Write html for annotation fields.

html_annotation_fields([],_) --> !.
html_annotation_fields([URI|T], Options) -->
	html(\html_annotation_field(URI, Options)),
	html_annotation_fields(T, Options).

comment_node_id(URI, NodeId) :-
	rdf(URI, an:comment, an:enabled),
	!,
	(   rdf_global_id(_:Id, URI)
	->  true
	;   Id = URI
	),
	atomic_concat(Id, '_comment', NodeId).

comment_node_id(_, @null).

unsure_node_id(URI, NodeId) :-
	(   rdf_global_id(_:Id, URI)
	->  true
	;   Id = URI
	),
	atomic_concat(Id, '_unsure', NodeId).

html_annotation_field(URI, Options) -->
	{ rdf_display_label(URI, Label),
	  (   rdf_global_id(_:Id, URI)
	  ->  true
	  ;   Id = URI
	  ),
	  rdf_has_lang(URI, dcterms:comment, '',  FieldDescription),
	  comment_node_id(URI, Cid),
	  option(unsure_label(UnsureLabel), Options, ''),
	  (   UnsureLabel == ''
	  ->  Unsure = ''
	  ;   unsure_node_id(URI, Uid),
	      Unsure = div([class('annotate-unsure')],
			   [input([type(checkbox),
				   id(Uid)
				  ]),
			    label([for(Uid)],
				  [UnsureLabel])
			   ])
	  ),
	  (   Cid \= @null
	  ->  (rdf_has_lang(URI, an:commentLabel, CommentLabel)
	      ->  true
	      ;	  CommentLabel='Why? (optional)'
	      ),
	      Comment = div(class('annotate-comment'),
			[
			 h3(CommentLabel),
			 textarea(
			     [id(Cid),class('annotate-comment-input')],
			     [])
			])
	  ;   Comment = ''
	  )
	},
	html(div([class('annotate-field'), alt(FieldDescription)],
		 [ div(class('annotate-header'),
		       [ h3(Label),
			 div([class('annotate-description')], FieldDescription)
		       ]),
		   input([id(Id), type(text)]),
		   Comment,
		   Unsure
		 ])),
	!.


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
					      'event-mouseenter',
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
	 {
	  (   rdf_global_id(_:Id, FieldURI)
	  ->  true
	  ;   Id = FieldURI
	  ),
	  comment_node_id(FieldURI, CommentNode),
	  unsure_node_id(FieldURI, UnsureNode),
	  http_location_by_id(http_add_annotation, Add),
	  http_location_by_id(http_remove_annotation, Remove),
	  http_location_by_id(http_get_annotation, Get),
	  user_preference(user:lang, literal(Lang)),
	  setting(min_query_length, MinQueryLength),
	  setting(http:prefix, Prefix),
	  (   rdf_has_lang(FieldURI, an:source, Source)
	  ->  atomic_concat(Prefix, Source, PrefixedSource),
	      Config = {
			target:Target,
			field:FieldURI,
			source:PrefixedSource,
			store: { add:Add,
				 get:Get,
				 remove:Remove
			       },
			commentNode: CommentNode,
			unsureNode:  UnsureNode,
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
			commentNode: CommentNode,
			unsureNode:  UnsureNode,
			store: { add:Add,
				 get:Get,
				 remove:Remove
			       }
		       }
	  ;   Config = {
			    target:Target,
			    field:FieldURI,
			    commentNode: CommentNode,
			    unsureNode:  UnsureNode,
			    store: { add:Add,
				     get:Get,
				     remove:Remove
				   }
			    }
	  )},
	yui3_plug(one(id(Id)), 'Y.Plugin.Annotation', Config).



rdf_has_lang(Subject, Predicate, Text, Default) :-
	(   rdf_has_lang(Subject, Predicate, Text)
	->  true
	;   Text = Default
	).

rdf_has_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	(   rdf_has(Subject, Predicate, literal(lang(Lang, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(lang(en, Text)))
	->  true
	;   rdf_has(Subject, Predicate, literal(Text))
	).

default_buttons(B, Options) :-
	option(done_label(DoneLabel), Options, done),
	B = [ a([id('image_annotation_done')], [DoneLabel])].

:- style_check(-atom).

done_script(Options) -->
	{
	 option(done_action(DoneAction), Options),
	 format(atom(DoneHandler),
		'function done() {
			      YUI().use("node", "event-custom", function(Y)
					{
					  Y.publish("done", { broadcast: 2 });
					  Y.fire("done", {});
					  setTimeout(function() {~w;}, 200);
					});
		}
	', [DoneAction])
	},
	html('
	   YUI().use("node", "event", function(Y)
		   {
		    Y.one("#image_annotation_done").on("click", done);
		   }
		  );
	'),
       html(DoneHandler).

