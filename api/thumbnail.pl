/*  This file is part of ClioPatria.

    Author:
    HTTP:	http://e-culture.multimedian.nl/
    GITWEB:	http://gollem.science.uva.nl/git/ClioPatria.git
    GIT:	git://gollem.science.uva.nl/home/git/ClioPatria.git
    GIT:	http://gollem.science.uva.nl/home/git/ClioPatria.git
    Copyright:  2007, E-Culture/MultimediaN

    ClioPatria is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    (at your option) any later version.

    ClioPatria is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with ClioPatria.  If not, see <http://www.gnu.org/licenses/>.
*/

:- module(thumbnail,
	  [ uri_thumbnail/3		% +URI, -File
	  ]).
:- use_module(library(settings)).
:- use_module(library(http/url_cache)).
:- use_module(library(debug)).
:- use_module(library(memfile)).

:- multifile
	local_file_for_uri/2.

% Windows: 'cmd.exe /C convert.exe'
:- setting(convert_program, atom, convert,
	   'ImageMagic convert used to create thumbnails').
:- setting(thumbnail_size, any, size(105,105),
	   'Term size(W,H) into which thumbnails are scaled').
:- setting(medium_size, any, size(800,800),
	   'Term size(W,H) into which medium sizes are scaled').
:- setting(cache_directory, atom, 'cache/thumbnails',
	   'Directory for caching thumbnails').
:- setting(mcache_directory, atom, 'cache/mediums',
	   'Directory for caching medium sized images').

%%	uri_thumbnail(+URI, -File)
%
%	Return thumbnail file for image at URI.
%
%	TBD: Error recovery

uri_thumbnail(URI, File, Size) :-
	thumbnail_dir(Dir0, Size),
	url_cache_file(URI, Dir0, jpeg, File),
	thread_self(Self),
	(   exists_file(File)
	->  debug(thumbnail, '[~w] CACHE: ~w', [Self, File])
	;   debug(thumbnail, '[~w] Convert for ~w', [Self, File]),
	    make_thumbnail(URI, File, Size)
	).

%%	thumbnail_dir(-AbsDir, Size)
%
%	Directory for caching thunbnails.  Create if it doesn't exist.
%
%	@AbsDir	Absolute path for location to cache thumbnails.

thumbnail_dir(AbsDir, Size) :-
	(   Size == thumbnail_size
	->  setting(cache_directory, Dir)
	;   setting(mcache_directory, Dir)
	),
	Dir \== '',
	absolute_file_name(Dir, AbsDir),
	ensure_directory(AbsDir).

%%	make_thumbnail(+URI, +File) is det.
%
%	Create a thumbnail for an image located at URI in the file
%	File.

make_thumbnail(URI, File, Size) :-
	local_file_for_uri(URI, Full), !,
	debug(thumbnail, 'Creating thumbnail from ~w', [Full]),
	scale(Full, File, Size).
make_thumbnail(URI, File, Size) :-
	url_cache(URI, Full, _Mime),
	scale(Full, File, Size).

scale(Full, File, Size) :-
	setting(Size, size(W, H)),
	setting(convert_program, Prog),
	os_relative_path(Full, OSFull),
	os_relative_path(File, OSFile),
	format(string(Cmd),
	       '"~w" -size ~wx~w "~w" -resize ~wx~w "~w"',
	       [Prog, W, H, OSFull, W, H, OSFile]),
	debug(thumbnail, Cmd, []),
	(   run(Cmd)
	->  true
	;   format(user_error, 'FAILED: ~w', [Cmd])
	).


%%	run(+Command) is det.
%
%	Run a command. On  Windows  we  use   a  pipe  to  get the error
%	messages in the Prolog console.  In   addition,  for  an totally
%	unknown reason Imagemagic =|convert.exe|= only runs given a full
%	pathname or using =|cmd.exe /C convert.exe ...|=.
%
%	Note: convert normally produces no output. If it does we assume
%	      there was an error and log to =user_error=.
%	Note: requires SWI-Prolog 5.6.28.
%
%	@tbd	Use new library(process)

run(Cmd) :-
	current_prolog_flag(windows, true), !,
	win_cmd(CmdExe),
	format(string(WinCmd), '~w /S /C "~w 2>&1"', [CmdExe, Cmd]),
	open(pipe(WinCmd), read, In),
	new_memory_file(H),
	open_memory_file(H, write, Out),
	copy_stream_data(In, Out),
	close(In), close(Out),
	memory_file_to_codes(H, Msg),
	free_memory_file(H),
	(   maplist(is_space, Msg)
	->  true
	;   format(user_error, 'Warning: "~s"', [Msg]),
	    fail
	).
run(Cmd) :-
	shell(Cmd).


%%	win_cmd(-Cmd) is det.
%
%	Get name of windows shell (cmd.exe)

win_cmd(Cmd) :-
	(   getenv(comspec, Cmd)
	->  true
	;   Cmd = 'cmd.exe'
	).


%%	os_relative_path(+Path, -RelativePath) is det.
%
%	If Path is an absolute filename, translate it into a relative
%	one to avoid too long commandlines on Windows.

os_relative_path(Path, OsRel) :-
	is_absolute_file_name(Path), !,
	relative_path(Path, Rel),
	prolog_to_os_filename(Rel, OsRel).
os_relative_path(Path, Path).


%%	relative_path(+Path, -Relative)
%
%	Transform an absolute path  into  a   relative  one  to overcome
%	limitations of the Windows commandline handling.

relative_path(Path, RelPath) :-
	working_directory(PWD, PWD),
	relative_path(Path, PWD, RelPath), !.
relative_path(Path, Path).

relative_path(Path, RelTo, RelPath) :-
	concat_atom(PL, /, Path),
	concat_atom(RL, /, RelTo),
	delete_common_prefix(PL, RL, PL1, PL2),
	to_dot_dot(PL2, DotDot, PL1),
	concat_atom(DotDot, /, RelPath).

delete_common_prefix([H|T01], [H|T02], T1, T2) :- !,
	delete_common_prefix(T01, T02, T1, T2).
delete_common_prefix(T1, T2, T1, T2).

to_dot_dot([], Tail, Tail).
to_dot_dot([_], Tail, Tail) :- !.
to_dot_dot([_|T0], ['..'|T], Tail) :-
	to_dot_dot(T0, T, Tail).



%%      ensure_directory(+Dir:atom)is det.
%
%       Create directory and -if  needed-   parents.  May  generate file
%       system errors.

ensure_directory(Dir) :-
        exists_directory(Dir), !.
ensure_directory(Dir) :-
        file_directory_name(Dir, Parent),
        Parent \== Dir,
        ensure_directory(Parent),
        make_directory(Dir).

