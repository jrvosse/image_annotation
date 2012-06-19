:- module(media_cache, []).

:- use_module(library(http/url_cache)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_dispatch)).

:- http_handler(root(cache/original),  http_original,  [spawn(media)]).

%%      original(+Request)
%
%       HTTP handler providing original content for a URI. Used together
%       with PicturePoint to avoid  Java   security  issues. Also caches
%       results from our upstream (image) providers such as Artchive and
%       and the musea.

http_original(Request) :-
        http_parameters(Request,
                        [ uri(URI0, [description('URI of the original image')])
                        ]),
        map_uri(URI0, URI),
        url_cache(URI, File, MimeType),
        debug(url_cache, 'Original for ~w (~w)', [URI,MimeType]),
        throw(http_reply(file(MimeType, File))).

map_uri(U,U).

