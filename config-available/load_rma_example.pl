:- module(load_rma_example, []).

:- use_module(library('semweb/rdf_db')).

:- rdf_load(rdf('rma_annotation.ttl')). % load example config file
