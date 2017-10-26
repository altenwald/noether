-module(noether_parser_expr).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([expression/3]).

-import(noether_parser, [incr/1, incr/2, new_line/1]).

-include("noether_parser.hrl").

-record(int, {
    int :: integer(),
    line :: pos_integer()
}).

-record(float, {
    float :: float(),
    line :: pos_integer()
}).

-record(text, {
    text = <<>> :: binary(),
    line :: pos_integer()
}).

-record(char, {
    char :: byte(),
    line :: pos_integer()
}).

?ADD_LINE(int);
?ADD_LINE(float);
?ADD_LINE(text);
?ADD_LINE(char).

resolve([]) -> [];
resolve([{op, Content}]) -> Content.

add_op(Add, [{op, Content}|Parsed]) when is_list(Add) ->
    [{op, Content ++ Add}|Parsed];
add_op(Add, [{op, Content}|Parsed]) ->
    [{op, Content ++ [Add]}|Parsed];
add_op(Add, Parsed) ->
    [{op, [Add]}|Parsed].

number(<<"0", X:8, Rest/binary>>, State, []) when ?OR(X, $X, $x) ->
    hexa(Rest, incr(State, 2), []);
number(<<"0", N:8, Rest/binary>>, State, []) when ?IS_OCT(N) ->
    octa(<<N:8, Rest/binary>>, incr(State, 2), []);
number(<<A:8, Rest/binary>>, State, []) when ?IS_NUMBER(A) orelse A =:= $- ->
    number(Rest, incr(State), [add_line(#int{int = <<A:8>>}, State)]);
number(<<A:8, Rest/binary>>, State, [#int{int = N} = I]) when ?IS_NUMBER(A) ->
    number(Rest, incr(State), [I#int{int = <<N/binary,A:8>>}]);
number(<<".", Rest/binary>>, State, []) ->
    number(Rest, incr(State), [add_line(#float{float = <<"0.">>}, State)]);
number(<<".", Rest/binary>>, State, [#int{int = N, line = Line}]) ->
    number(Rest, incr(State), [#float{float = <<N/binary,".">>, line = Line}]);
number(<<A:8, Rest/binary>>, State, [#float{float = N} = F]) when ?IS_NUMBER(A) ->
    number(Rest, incr(State), [F#float{float = <<N/binary, A:8>>}]);
number(Rest, State, [#int{int = N} = I]) ->
    {Rest, State, [I#int{int = binary_to_integer(N)}]};
number(Rest, State, [#float{float = N} = F]) ->
    {Rest, State, [F#float{float = binary_to_float(N)}]}.

hexa(<<A:8,Rest/binary>>, State, []) when ?IS_HEX(A) ->
    hexa(Rest, incr(State), [add_line(#int{int = <<A:8>>}, State)]);
hexa(<<A:8,Rest/binary>>, State, [#int{int=N}=I]) when ?IS_HEX(A) ->
    hexa(Rest, incr(State), [I#int{int = <<N/binary, A:8>>}]);
hexa(Rest, State, [#int{int = N} = I]) ->
    {Rest, State, [I#int{int = binary_to_integer(N, 16)}]}.

octa(<<A:8,Rest/binary>>, State, []) when ?IS_OCT(A) ->
    octa(Rest, incr(State), [add_line(#int{int = <<A:8>>}, State)]);
octa(<<A:8,Rest/binary>>, State, [#int{int=N}=I]) when ?IS_OCT(A) ->
    octa(Rest, incr(State), [I#int{int = <<N/binary, A:8>>}]);
octa(Rest, State, [#int{int = N} = I]) ->
    {Rest, State, [I#int{int = binary_to_integer(N, 8)}]}.

string(<<"\"", Rest/binary>>, State, []) ->
    string_fixed(Rest, incr(State), add_line(#text{text = <<>>}, State));
string(<<"'", SP:8, "'", Rest/binary>>, State, []) ->
    {Rest, incr(State, 3), add_line(#char{char = SP}, State)}.

string_fixed(<<>>, State, #text{line = Line}) ->
    throw({error, {eparse, State, {unexpected_end, Line}}});
string_fixed(<<"\\\\", Rest/binary>>, State, #text{text = C} = S) ->
    string_fixed(Rest, incr(State), S#text{text = <<C/binary, "\\\\">>});
string_fixed(<<"\\\"", Rest/binary>>, State, #text{text = C} = S) ->
    string_fixed(Rest, incr(State), S#text{text = <<C/binary, "\"">>});
string_fixed(<<"\"", Rest/binary>>, State, Parsed) ->
    {Rest, incr(State), Parsed};
string_fixed(<<"\n", Rest/binary>>, State, #text{text = C} = S) ->
    string_fixed(Rest, new_line(State), S#text{text = <<C/binary, "\n">>});
string_fixed(<<A/utf8, Rest/binary>>, State, #text{text = C} = S) ->
    string_fixed(Rest, incr(State), S#text{text = <<C/binary, A/utf8>>}).

expression(<<SP:8,Rest/binary>>, State, Parsed) when ?IS_SPACE(SP) ->
    expression(Rest, incr(State), Parsed);
expression(<<SP:8,Rest/binary>>, State, Parsed) when ?IS_NEWLINE(SP) ->
    expression(Rest, new_line(State), Parsed);
expression(<<"null", SP:8, Rest/binary>>, State, Parsed)
        when not (?IS_ALPHANUM(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, incr(State, 4), add_op(undefined, Parsed));
expression(<<"true", SP:8, Rest/binary>>, State, Parsed)
        when not (?IS_ALPHANUM(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, incr(State, 4), add_op(true, Parsed));
expression(<<"false", SP:8, Rest/binary>>, State, Parsed)
        when not (?IS_ALPHANUM(SP) orelse SP =:= $_) ->
    expression(<<SP:8, Rest/binary>>, incr(State, 4), add_op(false, Parsed));
expression(<<"//", Rest/binary>>, State, Parsed) ->
    {Rest0, State0} = noether_parser:comment_line(Rest, State),
    expression(Rest0, State0, Parsed);
expression(<<"/*", Rest/binary>>, State, Parsed) ->
    {Rest0, State0} = noether_parser:comment_block(Rest, State),
    expression(Rest0, State0, Parsed);
expression(<<";", _/binary>> = Rest, State, Parsed) ->
    {Rest, State, resolve(Parsed)};
% NUMBER
expression(<<A:8, _/binary>> = Rest, State, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, State0, [Number]} = number(Rest, State, []),
    expression(Rest0, State0, add_op(Number, Parsed));
% STRING
expression(<<A:8, _/binary>> = Rest, State, Parsed) when ?OR(A, $", $') ->
    {Rest0, State0, String} = string(Rest, State, []),
    expression(Rest0, State0, add_op(String, Parsed));
%% TODO check if this syntax is in use for Java
expression(<<".", A:8, _/binary>> = Rest, State, Parsed) when ?IS_NUMBER(A) ->
    {Rest0, State0, [Number]} = number(Rest, State, []),
    expression(Rest0, State0, add_op(Number, Parsed));
expression(<<>>, State, _Parsed) ->
    throw({error, {eparse, State, unexpected_end_expression}}).
