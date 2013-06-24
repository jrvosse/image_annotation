:- module(cp_image_annotation,
	  [ annotation_page/1,
	    get_anfields/4,
	    get_metafields/3,
	    image_annotation:application_script//1
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

:- use_module(api(annotation)).    % needed for http api handlers
:- use_module(api(media_caching)). % needed for http api handlers
:- use_module(dashboard).

:- rdf_meta
	rdf_lang(r,r,-),
	rdf_lang(r,r,+,-).


/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(annotate), http_annotation, []).


/***************************************************
* settings
***************************************************/

:- setting(min_query_length, integer, 3,
	   'Minimum number of characters that must be entered before a query event will be fired. A value of 0 allows empty queries; a negative value will effectively disable all query events and turn AutoComplete off. ').

:- setting(default_ui, uri,
	   'http://semanticweb.cs.vu.nl/annotate/nicheAccuratorFlowerDemoUi',
	   'URI of the default UI configuration object').

:- setting(default_target, uri,
	   'http://www.public-domain-image.com/plants/flowers/slides/chrysanthemum-flower.jpg',
	   'Default target object to annotate if none given').

:- setting(default_metadata, list(uri),
	   [ % 'http://purl.org/dc/terms/title',
	     'http://semanticweb.cs.vu.nl/annotate/ui/imageURL',
	     'http://semanticweb.cs.vu.nl/annotate/ui/url',
	     'http://purl.org/dc/terms/description'
	   ], 'Default metadata fields to show').

/***************************************************
* hooks
***************************************************/

:- multifile
	image_annotation:application_script//1,
	image_annotation:page_header//1.

application_script(Options) -->
	image_annotation:application_script(Options).
application_script(_Options) --> !.

annotation_page_header(Options) --> image_annotation:page_header(Options).
annotation_page_header(Options) -->
	{
	 option(target(Target), Options, notarget),
	 option(title(Title), Options, 'Annotation'),
	 rdf_display_label(Target, TargetLabel)
	},
	html(title([Title, ': ', TargetLabel])).

/***************************************************
* http handler implementations
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
		       optional(true),
		       description('URI of the UI configuration')
		     ]),
		  stylesheet(Stylesheet,
		     [ uri,
		       optional(true),
		       description('URI of an optional stylesheet')
		     ]),
		  field(ExtraFields,
			[list(uri),
			 description('URIs of annotation field, adding to UI defs')
			]),
		  disableField(DisabledFields,
			       [list(uri),
				description('URIs of annotation field, deleted from the UI defs')
			       ]),
		  metadata(MetaFields,
			   [list(uri),
			    description('URIs of metadata properties to display')
			   ])
		]),
	user_url(User),
	get_anfields(UI, ExtraFields, DisabledFields, AnnotationFields),
	get_metafields(UI, MetaFields, MetadataFields),
	(   var(UI)
	->  Title = 'Error: UI not defined ...', UI = undefined
	;   rdf_display_label(UI, Title)
	),
	Options = [
		   title(Title),
		   stylesheet(Stylesheet),
		   ui(UI),
		   target(Target),
		   user(User),
		   annotation_fields(AnnotationFields),
		   metadata_fields(MetadataFields)
		  ],
	annotation_page(Options).

get_anfields(UI, [], [], Fields) :-
	var(UI),
	setting(default_ui, UI),
	rdfs_individual_of(UI, ann_ui:'AnnotationUI'),
	get_anfields(UI, [], [], Fields).
get_anfields(UI, Fields, [], Fields) :-
	var(UI),
	Fields = [_|_],
	!.
get_anfields(URI, ExtraFields, DisabledFields, Fields) :-
	(   rdf_has(URI, ann_ui:fields, RdfList)
	->  rdfs_list_to_prolog_list(RdfList, UiFields),
	    append(UiFields, ExtraFields, Fields1),
	    subtract(Fields1, DisabledFields, Fields)
	;   Fields=ExtraFields
	).


get_metafields('', [], Fields) :-
	rdfs_individual_of(URI, ann_ui:'AnnotationUI'),
	get_metafields(URI, [], Fields),!.

get_metafields('', Fields, Fields) :-!.

get_metafields(URI, ExtraFields, Fields) :-
	(   rdf_has(URI, ann_ui:metadata, RdfList)
	->  rdfs_list_to_prolog_list(RdfList, UiFields),
	    append(UiFields, ExtraFields, Fields)
	;   setting(default_metadata, Fields)
	),!.



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
	default_buttons(DefaultButtons, Options),

	reply_html_page(
	    [ \annotation_page_header(Options) ],
	    [ \html_requires(yui3('cssgrids/grids-min.css')),
	      \html_requires(css('annotation.css')),
	      \conditional_html_requires(Options),
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
		     \yui_script(Options)),
	      \application_script(Options)

	    ]).

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
	 rdfs_subproperty_of(Field, ann_ui:imageURL)
	},
	html_resource_image(URI).

html_metadata_field(URI, Field, _Options) -->
	{
	 rdfs_subproperty_of(Field, ann_ui:url)
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
	  http_link_to_id(http_mediumscale, [uri(Image)], Medium),
	  http_link_to_id(http_original,    [uri(Image)], Full)
	}, !,
	html(a([href(Full), target('_blank')],
	       img([src(Medium)])
	      )).
html_resource_image(URI) -->
	{
	 resource_link(URI, Link)
	},
	html(a([href(Link)], ['No image available for ~p' - URI])).

% hack
image(R, Image) :-
	rdf_has(R, ann_ui:imageURL, Image).

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

html_annotation_field(URI, _Options) -->
	{ rdf_display_label(URI, Label),
	  (   rdf_global_id(_:Id, URI)
	  ->  true
	  ;   Id = URI
	  ),
	  rdf_lang(URI, dcterms:comment, FieldDescription, '')
	},
	html(div([class('annotate-field'), alt(FieldDescription)],
		 [ div(class('annotate-header'),
		       [ h3(Label),
			 div([class('annotate-description')], FieldDescription)
		       ]),
		   input([id(Id), type(text)])
		 ])),
	!.


		 /*******************************
		 *	     JavaScript		*
		 *******************************/

%%	yui_script(Options)
%
%	Emit YUI object.

yui_script(Options) -->
	{ findall(M-C, js_module(M,C), Modules),
	  pairs_keys(Modules, Includes),
	  option(annotation_fields(Fields), Options, [])
	},
	yui3([json([modules(json(Modules))])],
	     ['recordset-base'|Includes],
	     [\js_annotation_fields(Fields, Options)]).

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

ui_labels(Field, Options, Labels) :-
	option(ui(UI), Options),
	findall(P, rdf_has(UI,    ann_ui:uiLabel, _, P), UILP),
	findall(P, rdf_has(Field, ann_ui:uiLabel, _, P), FILP),
	append(UILP, FILP, LP),
	sort(LP, LP_Uniq),
	maplist(get_label(UI, Field), LP_Uniq, List),
	Labels = json(List).

get_label(UI, Field, LabelProp, LabelOption) :-
	(   rdf_lang(Field, LabelProp, LabelText)
	->  true
	;   rdf_lang(UI, LabelProp, LabelText)
	),
	rdf_global_id(_NS:LabelName, LabelProp),
	LabelOption =.. [LabelName, LabelText].

conditional_html_requires(Options) -->
	{
	 option(stylesheet(Stylesheet), Options),
	 ground(Stylesheet),
	 !
	},
	html_requires(Stylesheet).

conditional_html_requires(_) --> !.


%%	js_annotation_fields(+FieldURIs, +AnnotationTarget)
%
%	Write JavaScript to init chain of annotation fields
%
js_annotation_fields([URI], Options) -->
	js_annotation_field(URI, Options).
js_annotation_fields([URI, Next | T], Options) -->
	js_annotation_field(URI, [next(Next)|Options]),
	js_annotation_fields([Next | T], Options).




js_annotation_field(FieldURI, Options) -->
	 {
	  option(ui(UI), Options),
	  (   rdf(UI, ann_ui:tagStyle, literal(TagStyle))
	  ->  true
	  ;   TagStyle = overlay
	  ),
	  option(next(NextURI), Options, @null),
	  (   rdf_global_id(_:Id, FieldURI)
	  ->  true
	  ;   Id = FieldURI
	  ),
	  (   rdf_global_id(_:Next, NextURI)
	  ->  true
	  ;   Next = NextURI
	  ),
	  option(target(Target), Options),
	  user_url(DefaultUser),
	  option(user(User), Options, DefaultUser),
	  (   rdf(FieldURI, ann_ui:unsureEnabled,   literal(Unsure))
	  ->  true; Unsure=always ),
	  (   rdf(FieldURI, ann_ui:agreeEnabled,    literal(Agree))
	  ->  true; Agree=yours ),
	  (   rdf(FieldURI, ann_ui:disagreeEnabled, literal(Disagree))
	  ->  true; Disagree=yours ),
	  (   rdf(FieldURI, ann_ui:commentEnabled,  literal(Comment))
	  ->  true; Comment=always ),
	  (   rdf(FieldURI, ann_ui:deleteEnabled,   literal(Delete))
	  ->  true; Delete=mine ),
	  (   rdf(FieldURI, ann_ui:type, QuiType)
	  ->  rdf_global_id(_:UiType, QuiType)
	  ;   UiType = 'Autocomplete'
	  ),
	  ui_labels(FieldURI, Options, UI_labels),
	  http_location_by_id(http_add_annotation, Add),
	  http_location_by_id(http_remove_annotation, Remove),
	  http_location_by_id(http_get_annotation, Get),
	  user_preference(user:lang, literal(Lang)),
	  setting(min_query_length, MinQueryLength),
	  setting(http:prefix, Prefix),
	  (   rdf_lang(FieldURI, ann_ui:source, Source)
	  ->  atomic_concat(Prefix, Source, PrefixedSource),
	      % Configure a field with autocompletion web service URI.
	      Config = {
			tagStyle: TagStyle,
			target:Target,
			field:FieldURI,
			next: Next,
			source:PrefixedSource,
			store: { add:Add,
				 get:Get,
				 remove:Remove
			       },
			user: User,
			uiLabels: UI_labels,
			unsureEnabled: Unsure,
			commentEnabled: Comment,
			agreeEnabled: Agree,
			disagreeEnabled: Disagree,
			deleteEnabled: Delete,
			minQueryLength:MinQueryLength,
			resultListLocator: results,
			resultTextLocator: label,
			resultHighlighter: phraseMatch}
	  ;   rdf_has(FieldURI, ann_ui:source, RdfList),
	      rdfs_member(literal(lang(Lang, _)), RdfList),
	      rdfs_list_to_prolog_list(RdfList, LiteralList),
	      maplist(literal_text, LiteralList, TextList),
	      prolog_to_json(TextList, Source)
	  ->  % Configure a field with autocomplete from given list
	      Config = {
			tagStyle: TagStyle,
			target:Target,
			field:FieldURI,
			next: Next,
			source:Source,
			type:UiType,
			user: User,
			uiLabels: UI_labels,
			unsureEnabled: Unsure,
			commentEnabled: Comment,
			agreeEnabled: Agree,
			disagreeEnabled: Disagree,
			deleteEnabled: Delete,
			store: { add:Add,
				 get:Get,
				 remove:Remove
			       }
		       }
	  ;   % Configure a field without autocompletion
	      Config = {next: Next,
			target:Target,
			field:FieldURI,
			user: User,
			uiLabels: UI_labels,
			unsureEnabled: Unsure,
			commentEnabled: Comment,
			agreeEnabled: Agree,
			disagreeEnabled: Disagree,
			deleteEnabled: Delete,
			tagStyle: TagStyle,
			store: { add:Add,
				 get:Get,
				 remove:Remove
			       }
		       }
	  )},
	yui3_plug(one(id(Id)), 'Y.Plugin.Annotation', Config).

%%	rdf_lang(+Subject, +Predicate, ?Text, +Default) is det.
%
%	Text is unified with the "preferred" textual value of literal
%	property Predicate on Subject.  Order of preference:
%	1. Text is in the user:lang defined by user_preference/2.
%	2. Text is in the English language.
%	3. Text is in a random other language
%	4. Text is unified with Default.

rdf_lang(Subject, Predicate, Text, Default) :-
	(   rdf_lang(Subject, Predicate, Text)
	->  true
	;   Text = Default
	).

rdf_lang(Subject, Predicate, Text) :-
	user_preference(user:lang, literal(Lang)),
	(   rdf(Subject, Predicate, literal(lang(Lang, Text)))
	->  true
	;   rdf(Subject, Predicate, literal(lang(en, Text)))
	->  true
	;   rdf(Subject, Predicate, literal(lang(_, Text)))
	).

default_buttons([],_).

user_url(User) :-
	(   setting(annotation_api:login, true)
        ->  ensure_logged_on(U),
	    authorized(write(default, annotate)),
	    user_property(U, url(User))
        ;   logged_on(U)
	->  user_property(U, url(User))
	;   rdf_global_id(an:anonymous, User)
        ).
