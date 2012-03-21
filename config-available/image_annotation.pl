:- module(conf_image_annotation, []).

/** <module> Image annotation with configurable search fields
*/

:- use_module(library('semweb/rdf_db')).
:- use_module(library('semweb/rdf_library')).

:- rdf_load_library(dcterms).

% hack namespace
:- rdf_register_ns(oac, 'http://www.openannotation.org/ns/').
:- rdf_register_ns(an, 'http://semanticweb.cs.vu.nl/annotate/').
:- rdf_register_ns(gv, 'http://semanticweb.cs.vu.nl/graph/version/').
:- rdf_register_ns(prov, 'http://www.w3.org/ns/prov-o/').

:- use_module(applications(annotation)).
