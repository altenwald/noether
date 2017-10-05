-module(noether_parser).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([parse/1, file/1]).

-include("noether_parser.hrl").

-record(state, {
    row = 1 :: integer(),
    col = 1 :: integer(),
    package :: binary(),
    module :: binary(),
    access :: undefined | public | private | protected,
    abstract = false :: boolean()
}).

file(File) ->
    {ok, Content} = file:read_file(File),
    parse(Content).

parse(Content) when is_list(Content) ->
    parse(list_to_binary(Content));
parse(Content) ->
    {_, _, Parsed} = document(Content, #state{}, []),
    lists:reverse(Parsed).

document(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_SPACE(SP) ->
    document(Rest, incr(State), Parsed);
document(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_NEWLINE(SP) ->
    document(Rest, new_line(State), Parsed);
document(<<"package ", Rest/binary>>, State, Parsed) ->
    {Rest0, State0} = remove_spaces(Rest, State),
    {Rest1, State1, [PackageName]} = package_name(Rest0, State0),
    State2 = State1#state{package = PackageName},
    package({Rest1, State2, Parsed});
document(<<"/*", Rest/binary>>, State, Parsed) ->
    {Rest0, State0} = comment_block(Rest, State),
    document(Rest0, State0);
document(<<"//", Rest/binary>>, State, Parsed) ->
    {Rest0, State0} = comment_line(Rest, State),
    document(Rest0, State0);
document(<<>>, State, _Parsed) ->
    throw({error, {eparse, State, missing_package}}).

package(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_SPACE(SP) ->
    package(Rest, incr(State), Parsed);
package(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_NEWLINE(SP) ->
    package(Rest, new_line(State), Parsed);
package(<<"public", SP:8, Rest/binary>>, #state{access = undefined} = State,
        _Parsed) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = public}, 7), Parsed);
package(<<"public", SP:8, _Rest/binary>>, State, _Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_public}});
package(<<"protected", SP:8, Rest/binary>>, #state{access = undefined} = State,
        _Parsed) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = protected}, 10), Parsed);
package(<<"protected", SP:8, _Rest/binary>>, State, _Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_protected}});
package(<<"private", SP:8, Rest/binary>>, #state{access = undefined} = State,
        _Parsed) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = private}, 8), Parsed);
package(<<"private", SP:8, _Rest/binary>>, State, _Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_private}});
package(<<"abstract", SP:8, Rest/binary>>, #state{abstract = false} = State,
        _Parsed) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{abstract = true}, 9), Parsed);
package(<<"abstract", SP:8, _Rest/binary>>, State, _Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_abstract}});
package(<<"interface", SP:8, Rest/binary>>, State, Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 10)),
    {Rest1, State1, [InterfaceName]} = class_name(Rest0, State0),
    ModuleName = <<State#state.package/binary, ".", InterfaceName/binary>>,
    State2 = State1#state{module = ModuleName},
    {Rest3, State3, Interfaces} = interface(Rest1, State2, Parsed),
    package(Rest3, State3, Interfaces);
package(<<"class", SP:8, Rest/binary>>, State, Parsed)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 10)),
    {Rest1, State1, [InterfaceName]} = class_name(Rest0, State0, [<<>>]),
    ModuleName = <<State#state.package/binary, ".", InterfaceName/binary>>,
    State2 = State1#state{module = ModuleName},
    {Rest3, State3, Interfaces} = interface(Rest1, State2, Parsed),
    package(Rest3, reset_state(State3), Interfaces);
package(<<>>, State, Parsed) ->
    {<<>>, State, Parsed};
package(<<A:8, _/binary>>, State, _Parsed) ->
    throw({error, {eparse, State, {not_expected, <<A:8>>}}).

class_name(<<A:8, Rest/binary>>, State, [Name])
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    class_name(Rest, incr(State), [<<Name/binary, A:8>>]);
class_name(Rest, State, Parsed) ->
    {Rest, State, Parsed}.

remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_SPACE(SP) ->
    remove_spaces(Rest, incr(State));
remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_NEWLINE(SP) ->
    remove_spaces(Rest, new_line(State));
remove_spaces(<<>>, State) -> {<<>>, State};
remove_spaces(Rest, State) -> {Rest, State}.

reset_state(State) ->
    State#state{
        module = undefined,
        access = undefined,
        abstract = false
    }.

incr(State) -> incr(State, 1);
incr(#state{col = Col} = State, N) -> State#state{col = Col + N}.

new_line(#state{row = Row} = State) -> State#state{row = Row + 1}.

next_state(Name, State) -> State#state{name = Name}.

comment_line(<<>>, State, Parsed) ->
    {<<>>, State, Parsed};
comment_line(<<A:8, Rest/binary>>, State, Parsed) when ?IS_NEWLINE(A) ->
    {Rest, new_line(State), Parsed};
comment_line(<<_/utf8, Rest/binary>>, State, Parsed) ->
    comment_line(Rest, incr(State), Parsed).

comment_block(<<>>, State, _Parsed) ->
    throw({error, {eparse, State, missing_comment_end}});
comment_block(<<"*/", Rest/binary>>, State, Parsed) ->
    {Rest, incr(State, 2), Parsed};
comment_block(<<A:8, Rest/binary>>, State, Parsed) when ?IS_NEWLINE(A) ->
    comment_block(Rest, new_line(State), Parsed);
comment_block(<<_/utf8, Rest/binary>>, State, Parsed) ->
    comment_block(Rest, incr(State), Parsed).
