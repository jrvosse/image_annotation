:- module(load_rma_example, []).

:- use_module(library('semweb/rdf_db')).

:- rdf_load(rdf('annotation_ui_example.ttl')). % load example config file
:- rdf_load(rdf('annotation_ui_bird.ttl')). % load bird config file
:- rdf_load(rdf('annotation_ui_bible.ttl')).