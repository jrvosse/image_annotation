:- module(conf_image_annotation, []).

/** <module> Image annotation with configurable search fields
*/

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_library')).

:- rdf_attach_library(image_annotation(rdf)).
:- rdf_load_library('annotation-ui-schema').

:- use_module(applications(annotation)).
