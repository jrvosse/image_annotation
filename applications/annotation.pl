:- module(cp_image_annotation,
	  [ annotation_page/1,
	    object_image/2,
	    get_anfields/4,
	    get_metafields/3,
	    annotation_page_body//1,
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

:- use_module(cliopatria(hooks)).

:- use_module(components(label)).
:- use_module(user(user_db)).
:- use_module(user(preferences)).

:- use_module(api(annotation)).    % needed for http api handlers
:- use_module(api(media_caching)). % needed for http api handlers

:- rdf_meta
	rdf_lang(r,r,-),
	rdf_lang(r,r,+,-).


/***************************************************
* http handlers
***************************************************/

:- http_handler(cliopatria(annotate), http_annotation, []).

% Add handler to ClioPatria's web interface :

cliopatria:menu_popup_order(annotation, 120).
cliopatria:menu_label(annotation, 'Annotations').
cliopatria:menu_item(100=annotation/http_annotation, 'annotate image').

/***************************************************
* settings
***************************************************/

:- setting(min_query_length, integer, 3,
	   'Minimum number of characters that must be entered before a query event will be fired. A value of 0 allows empty queries; a negative value will effectively disable all query events and turn AutoComplete off. ').

:- setting(default_ui, uri,
	   'http://semanticweb.cs.vu.nl/annotate/example#nicheAccuratorFullDemoUi',
	   'URI of the default UI configuration object').

:- setting(default_target, uri,
	   'http://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/Chrysanthemum_indicum2.jpg/800px-Chrysanthemum_indicum2.jpg',
	   'Default target object to annotate if none given').

:- setting(default_metadata, list(uri),
	   [ % 'http://purl.org/dc/terms/title',
	     'http://semanticweb.cs.vu.nl/annotate/ui/imageURL',
	     'http://semanticweb.cs.vu.nl/annotate/ui/url',
	     'http://purl.org/dc/terms/description'
	   ], 'Default metadata fields to show').

:- setting(enableFragments, boolean, true, 'enable (annotorious) fragment support').
/***************************************************
* hooks
***************************************************/

:- multifile
	image_annotation:application_script//1,
	image_annotation:page_header//1.



:- html_resource(object_annotation,
		 [ virtual(true),
		   ordered(true),
		   requires(
		       [ css('object-annotation.css')
		       ])
		 ]).

/*
Some alternative annotorious URLs that might be handy during development:
	'http://annotorious.github.com/latest/annotorious.css',
	'http://annotorious.github.com/latest/annotorious.min.js',
	'http://localhost:9810/compile?id=annotorious',


This last url is handy for debugging annotorious, but we need to tell
prolog is mime type:
*/
:- html_resource('http://localhost:9810/compile?id=annotorious',
		 [ mime_type(text/javascript) ]).

:- html_resource(fragment_annotation,
	      [ virtual(true),
		ordered(true),
		requires([
		    css('annotorious.css'),
		    css('fragment-annotation.css'),
		    js('annotorious.min.js'),
		    js('deniche-plugin.js')
		])
	      ]).

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
	reply_html_page(
	    [ \annotation_page_header(Options) ],
	    [ \annotation_page_body(Options) ]).

annotation_page_body(Options) -->
	{ option(target(Target), Options, notarget),
	  option(annotation_fields(AnFields), Options, []),
	  option(footer(Footer), Options, []),
	  option(buttons(Buttons), Options, DefaultButtons),
	  field_id(img, Target, ImageId),
	  field_id(div, Target, ContainerId),
	  field_id(fields, Target, FieldsId),
	  default_buttons(DefaultButtons, Options),
	  object_image(Target, Image),
	  (   Image \= no_image_available
	  ->  true
	  ;   fail
	  ),
	  append([[image_id(ImageId),
		   fields_id(FieldsId),
		   container_id(ContainerId),
		   image_url(Image)
		  ],
		  Options], NewOptions)
	},
	html([
	    \html_requires(yui3('cssgrids/cssgrids-min.css')),
	    \html_requires(css('common-annotation.css')),
	    \conditional_html_requires(style, NewOptions),
	    \conditional_html_requires(fragment_annotation, NewOptions),
	    div([ id(ContainerId), class('yui3-skin-sam yui-skin-sam')],
		[ div(class(hd), []),
		  div(class(bd),
		      div([ class(layout), class('yui3-g')],
			  [ div([class(mediasection), class('yui3-u')],
				\html_resource(Target, NewOptions)),
			    div([class(fields), class('yui3-u'), id(FieldsId)],
				[ \html_annotation_fields(AnFields, NewOptions),
				    div([class(anbuttons)], Buttons)
				])
			  ])
		     ),
		  div(id(ft), Footer)
		]),
	    script(type('text/javascript'),
		   \yui_script(NewOptions)),
	    \application_script(NewOptions)
	]).


annotation_page_body(Options) -->
	{ option(target(Target), Options, notarget)
	},
	html(div([class(error_msg)],
		 [ 'no image for ',
		   \rdf_link(Target)
		 ])
	    ).

%%	html_resource(+URI, Options)
%
%
%	Title image and description of the resource being annotated.

html_resource(URI, Options) -->
	{
	 option(metadata_fields(Fields), Options, [])
	},
	html(div(class('resource'), [ \html_metadata_fields(URI, Fields, Options)])).

html_metadata_fields(_URI, [], _) --> !.
html_metadata_fields(URI, [Field|Tail], Options) -->
	html_metadata_field(URI, Field, Options),
	html_metadata_fields(URI, Tail, Options).

html_metadata_field(URI, Field, Options) -->
	{
	 rdfs_subproperty_of(Field, ann_ui:imageURL)
	},
	html_resource_image(URI, Options).

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


html_resource_image(URI, Options) -->
	{ object_image(URI, Image),
	  option(image_id(Id), Options, null),
	  option(fields_id(FieldsId), Options, null),
	  option(image_link_predicate(LinkPred), Options, http_original),
	  http_link_to_id(LinkPred, [uri(Image)], Full)
	}, !,
	html([img([id(Id), class(annotatable), src(Full), fields(FieldsId)]),
	      script([type('text/javascript')],
		     [
		     ])
	     ]).

html_resource_image(URI, _Options) -->
	{
	 resource_link(URI, Link)
	},
	html(a([href(Link)], ['No image available for ~p' - URI])).

% hack
object_image(R, Image) :-
	rdf_has(R, ann_ui:imageURL, Image),!.

object_image(R, Image) :-
	rdf_has(R, foaf:depiction, Image),!.

object_image(R, Image) :-
	rdf_has(Image, 'http://www.vraweb.org/vracore/vracore3#relation.depicts', R),!.

object_image(R, Image) :-
	rdf_has(Aggregation, 'http://www.europeana.eu/schemas/edm/aggregatedCHO', R),
	rdf_has(Aggregation, 'http://www.europeana.eu/schemas/edm/isShownBy', Image),!.
object_image(R,R) :-
	catch(url_cache(R, _, MimeType), _, fail),
	sub_atom(MimeType, 0, 5, _, 'image'),!.
object_image(R,no_image_available) :-
	debug(object_image, 'No image for object ~p', [R]).

field_id(null, _, null) :- !.
field_id(FieldURI, TargetURI, Id) :-
	variant_sha1(term(FieldURI, TargetURI), SHA1),
	atom_concat(id_, SHA1, Id).


%%	html_annotation_fields(+FieldURIs, +Options)
%
%	Write html for annotation fields.

html_annotation_fields([],_) --> !.
html_annotation_fields([URI|T], Options) -->
	html(\html_annotation_field(URI, Options)),
	html_annotation_fields(T, Options).

html_annotation_field(URI, Options) -->
	{ rdf_display_label(URI, Label),
	  option(target(T), Options),
	  field_id(URI,T, Id),
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

fragments_enabled(Options) :-
	setting(enableFragments, Default),
	option(ui(UI), Options),
	(   rdf(UI, ann_ui:enableFragments, literal(type(xsd:boolean, Enable)))
	->  Enable = true
	;   Default = true
	).

conditional_html_requires(style, Options) -->
	{ option(stylesheet(Stylesheet), Options),
	  ground(Stylesheet),
	  !
	},
	html_requires(Stylesheet).

conditional_html_requires(fragment_annotation, Options) -->
	{ fragments_enabled(Options), !
	},
	html_requires(fragment_annotation).
conditional_html_requires(fragment_annotation, Options) -->
	{ \+ fragments_enabled(Options), !
	},
	html_requires(object_annotation).

conditional_html_requires(_,_) --> !.

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
	{ option(target(Target), Options),
	  option(ui(UI), Options),
	  option(user(User), Options, DefaultUser),
	  option(next(NextURI), Options, null),
	  option(image_id(ImageId), Options, null),
	  option(fields_id(FieldsId), Options, null),
	  option(lazy(Lazy), Options, false),
	  field_id(FieldURI, Target,  Id),
	  field_id(NextURI,  Target, Next),
	  user_url(DefaultUser),

	  (   rdf(UI, ann_ui:tagStyle, literal(TagStyle))
	  ->  true; TagStyle = overlay ),
	  (   rdf(FieldURI, ann_ui:unsureEnabled,   literal(Unsure))
	  ->  true; Unsure=always ),
	  (   rdf(FieldURI, ann_ui:agreeEnabled,    literal(Agree))
	  ->  true; Agree=yours ),
	  (   rdf(FieldURI, ann_ui:disagreeEnabled, literal(Disagree))
	  ->  true; Disagree=yours ),
	  (   rdf(FieldURI, ann_ui:commentEnabled,  literal(Comment))
	  ->  true; Comment=always ),
	  (   rdf(FieldURI, ann_ui:deleteEnabled,   literal(Delete))
	  ->  true
	  ;   logged_on(admin)
	  ->  Delete=always
	  ;   Delete=mine
	  ),
	  ui_labels(FieldURI, Options, UI_labels),
	  http_location_by_id(http_add_annotation, Add),
	  http_location_by_id(http_remove_annotation, Remove),
	  http_location_by_id(http_get_annotation, Get),
	  user_preference(user:lang, literal(Lang)),
	  setting(min_query_length, MinQueryLength),
	  setting(http:prefix, Prefix),

	  object_image(Target, TargetImage),
	  Default = config{
			id: Id,
			imageId: ImageId,
			fieldsId: FieldsId,
			target: Target,
			targetImage: TargetImage,
			tagStyle: TagStyle,
			field:FieldURI,
			store: store{add:Add,
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
			next: Next,
			lazy: Lazy
		    },

	  (   rdf_lang(FieldURI, ann_ui:source, Source)
	  ->  atomic_concat(Prefix, Source, PrefixedSource),
	      % Configure a field with autocompletion web service URI.
	      Config = Default.put(config{
				       config:1,
				       source:PrefixedSource,
				       minQueryLength:MinQueryLength,
				       resultListLocator: results,
				       resultTextLocator: label,
				       resultHighlighter: phraseMatch
				   })
	  ;   rdf_has(FieldURI, ann_ui:source, RdfList),
	      rdfs_member(literal(lang(Lang, _)), RdfList),
	      rdfs_list_to_prolog_list(RdfList, LiteralList),
	      maplist(literal_text, LiteralList, TextList),
	      prolog_to_json(TextList, Source)
	  ->  % Configure a field with autocomplete from given list
	      Config =  Default.put(config{
					config:2,
					source:Source
				    })
	  ;   % Configure a field without autocompletion
	      Config = Default.put(config{
				       config:3
				   })
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
	;   rdf_global_id(user:anonymous, User)
        ).
