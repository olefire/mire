:- use_module(library(socket)).
:- use_module(library(random)).

:- dynamic exit/1.
:- dynamic hello/1.
:- dynamic path/1.

e(north) --> [north].
e(south) --> [south].
e(west) --> [west].
e(east) --> [east].
path_list(Path):-Path = [north, south, south, east, west, north].
enter --> ['> '].
hello -->[what],[is],[your],[name],['?'].
% path (north south south east west north)
exits([Exit]) --> e(Exit).
exits([Exit|Exits]) --> e(Exit), exits(Exits).
parse_exits(Exits) --> [exits], exits(Exits).

parse(Tokens) :- phrase(parse_exits(Exits), Tokens, _), retractall(exit(_)), assert(exit(Exits)).

parse(_).

:- retractall(path(_)), path_list(Path), assert(path(Path)).

filter_codes([], []).
filter_codes([H|T1], T2) :-
    char_code(C, H),
    member(C, ['(', ')', ':', ',', '>']),
    filter_codes(T1, T2).
filter_codes([H|T1], [F|T2]) :-
    code_type(F, to_lower(H)),
    filter_codes(T1, T2).

% Передвижение бота по заданному пути
process(Stream) :-
    path([Exit|Rest]),
    format(atom(Command), 'move ~w~n', [Exit]),
    write('Command: '),write(Command),
    write(Stream, Command),
    flush_output(Stream),
    (Rest == [] -> 
        retract(path(_)), path_list(Path), assertz(path(Path));
        retract(path(_)), assertz(path(Rest))).
process(_).

% Задание имени при подключении к серверу   
create_name(Stream):-
    read_line_to_codes(Stream, Codes),
    filter_codes(Codes, Filtered),
    atom_codes(Atom, Filtered),
    tokenize_atom(Atom, Tokens),
    write(Atom),
    phrase(hello,Tokens,_),assert(hello('prolog')),
    format(atom(Command), 'prolog~n',[]),
    write(Stream,Command),
    write(Command),
    flush_output(Stream).

% Функция для того что-бы бот что-то сказал в комнате
say(Stream):-
    format(atom(Command), 'say Don\'t mind me, I\'m a bot. I\'m just walking around the rooms.~n', []),
    write('Command: '),write(Command),
    write(Stream, Command),
    flush_output(Stream).

% Чтение данных с потока
read_stream(Stream,Tokens):-
    read_line_to_codes(Stream, Codes),
    filter_codes(Codes, Filtered),
    atom_codes(Atom, Filtered),
    tokenize_atom(Atom, Tokens),
    write(Atom),
    nl,
    flush_output().

% Цикл чтения потока
read_loop(Stream):-
    read_stream(Stream,Tokens),
    (Tokens == [] ->!;read_loop(Stream)).

% Что должен выполнить бот находясь в текущей комнате
command_in_room(Stream):-
    % sleep(3),
    % say(Stream),
    % read_loop(Stream),
    random(1,15,R),
    sleep(R),
    process(Stream).

% Цикл жизни бота
loop(Stream) :-
    read_loop(Stream),
    command_in_room(Stream),
    loop(Stream).

main :-
    setup_call_cleanup(
        tcp_connect(localhost:3333, Stream, []),
        (create_name(Stream), loop(Stream)),
        close(Stream)).

