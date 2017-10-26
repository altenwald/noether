-module(noether_parser).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([parse/1, file/1]).

-export([incr/1, incr/2, new_line/1, comment_block/2, comment_line/2]).

-include("noether_parser.hrl").

-record(attribute, {
    name :: binary(),
    type :: binary(),
    access :: access(),
    static = false :: boolean(),
    final = false :: boolean(),
    init_value = undefined :: term(),
    line :: pos_integer()
}).

-type attribute() :: #attribute{}.

-record(method_param, {
    name :: binary(),
    type :: binary(),
    line :: pos_integer()
}).

-type method_param() :: #method_param{}.

-record(method, {
    name :: binary(),
    params = [] :: [method_param()],
    access :: access(),
    static = false :: boolean(),
    final = false :: boolean(),
    abstract = false :: boolean(),
    return :: binary(),
    code = [] :: [term()],
    line :: pos_integer()
}).

-type method() :: #method{}.

-record(class, {
    name :: binary(),
    final = false :: boolean(),
    access :: access(),
    abstract = false :: boolean(),
    extends :: undefined | binary(),
    implements = [] :: [binary()],
    attributes = [] :: [attribute()],
    methods = [] :: [method()],
    line :: pos_integer()
}).

-type class() :: #class{}.

-record(interface, {
    name :: binary(),
    methods = [] :: [method()],
    line :: pos_integer()
}).

-type interface() :: #interface{}.

-record(package, {
    name :: binary(),
    classes = [] :: [class()],
    interfaces = [] :: [interface()],
    imports = [] :: [binary()],
    line :: pos_integer()
}).

%-type package() :: #package{}.


file(File) ->
    {ok, Content} = file:read_file(File),
    parse(Content).

parse(Content) when is_list(Content) ->
    parse(list_to_binary(Content));
parse(Content) ->
    {_, _, Parsed} = document(Content, #state{}, undefined),
    lists:reverse(Parsed).

?ADD_LINE(package);
?ADD_LINE(class);
?ADD_LINE(interface);
?ADD_LINE(attribute);
?ADD_LINE(method);
?ADD_LINE(method_param).

document(<<SP:8, Rest/binary>>, State, undefined) when ?IS_SPACE(SP) ->
    document(Rest, incr(State), undefined);
document(<<SP:8, Rest/binary>>, State, undefined) when ?IS_NEWLINE(SP) ->
    document(Rest, new_line(State), undefined);
document(<<"package ", Rest/binary>>, State, undefined) ->
    {Rest0, State0} = remove_spaces(Rest, State),
    {Rest1, State1, PackageName} = package_name(Rest0, State0, <<>>),
    {<<";", Rest2/binary>>, State2} = remove_spaces(Rest1, State1),
    package(Rest2, State2, add_line(#package{name = PackageName}, State));
document(<<"/*", Rest/binary>>, State, undefined) ->
    {Rest0, State0} = comment_block(Rest, State),
    document(Rest0, State0, undefined);
document(<<"//", Rest/binary>>, State, undefined) ->
    {Rest0, State0} = comment_line(Rest, State),
    document(Rest0, State0, undefined);
document(<<>>, State, undefined) ->
    throw({error, {eparse, State, missing_package}}).

package(<<SP:8, Rest/binary>>, State, Package) when ?IS_SPACE(SP) ->
    package(Rest, incr(State), Package);
package(<<SP:8, Rest/binary>>, State, Package) when ?IS_NEWLINE(SP) ->
    package(Rest, new_line(State), Package);
package(<<"//", Rest/binary>>, State, Package) ->
    {Rest0, State0} = comment_line(Rest, State),
    package(Rest0, State0, Package);
package(<<"/*", Rest/binary>>, State, Package) ->
    {Rest0, State0} = comment_block(Rest, State),
    package(Rest0, State0, Package);
package(<<"public", SP:8, Rest/binary>>, #state{access = undefined} = State,
        Package) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = public}, 7), Package);
package(<<"public", SP:8, _Rest/binary>>, State, _Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_public}});
package(<<"protected", SP:8, Rest/binary>>, #state{access = undefined} = State,
        Package) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = protected}, 10), Package);
package(<<"protected", SP:8, _Rest/binary>>, State, _Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_protected}});
package(<<"private", SP:8, Rest/binary>>, #state{access = undefined} = State,
        Package) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{access = private}, 8), Package);
package(<<"private", SP:8, _Rest/binary>>, State, _Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_private}});
package(<<"abstract", SP:8, Rest/binary>>, #state{abstract = false} = State,
        Package) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    package(Rest, incr(State#state{abstract = true}, 9), Package);
package(<<"abstract", SP:8, _Rest/binary>>, State, _Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_abstract}});
package(<<"interface", SP:8, Rest/binary>>, State, Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 10)),
    {Rest1, State1, InterfaceName} = key_name(Rest0, State0, <<>>),
    %% TODO add the rest of the attributes for the interface
    Interface0 = add_line(#interface{name = InterfaceName}, State),
    {Rest2, State2, Interface} = interface(Rest1, reset_state(State1), Interface0),
    package(Rest2, reset_state(State2), add_interface(Package, Interface));
package(<<"class", SP:8, Rest/binary>>, State, Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 6)),
    {Rest1, State1, ClassName} = key_name(Rest0, State0, <<>>),
    Class1 = add_line(#class{name = ClassName,
                             access = State#state.access,
                             abstract = State#state.abstract,
                             final = State#state.final}, State),
    case class_meta(Rest1, State1, Class1) of
        {<<"{", Rest2/binary>>, State2, Class2} ->
            {Rest3, State3, Class3} = class(Rest2, reset_state(State2), Class2),
            package(Rest3, reset_state(State3), add_class(Package, Class3));
        {_Rest2, State2, _Class2} ->
            throw({error, {eparse, State2, unexpected_class_meta}})
    end;
package(<<"import", SP:8, Rest/binary>>, State, Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 7)),
    {<<";", Rest1/binary>>, State1, Import} = import_name(Rest0, State0, <<>>),
    package(Rest1, incr(State1), add_import(Package, Import));
package(<<>>, State, Package) ->
    {<<>>, State, Package};
package(<<A:8, _/binary>>, State, _Parsed) ->
    throw({error, {eparse, State, {not_expected, <<A:8>>}}}).

add_import(#package{imports = Imports} = Package, Import)
        when is_binary(Import) ->
    Package#package{imports = [Import|Imports]}.

add_class(#package{classes = Classes} = Package, #class{} = Class) ->
    Package#package{classes = [Class|Classes]}.

add_interface(#package{interfaces = Interfaces} = Package,
              #interface{} = Interface) ->
    Package#package{interfaces = [Interface|Interfaces]}.

package_name(<<SP:8, Rest/binary>>, State, Name) when ?IS_SPACE(SP) ->
    package_name(Rest, incr(State), Name);
package_name(<<SP:8, Rest/binary>>, State, Name) when ?IS_NEWLINE(SP) ->
    package_name(Rest, new_line(State), Name);
package_name(<<A:8, _/binary>>, State, <<>>) when ?IS_NUMBER(A) ->
    throw({error, {eparse, State, package_name_invalid}});
package_name(<<A:8, _/binary>> = Rest, State, Name)
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    case key_name(Rest, State, <<>>) of
        {<<";", _/binary>> = Rest1, State1, Name1} ->
            {Rest1, State1, <<Name/binary, Name1/binary>>};
        {<<".", Rest1/binary>>, State1, Name1} ->
            package_name(Rest1, State1, <<Name/binary, Name1/binary, ".">>);
        {_Rest1, State1, _Name1} ->
            throw({error, {eparse, State1, package_name_wrong}})
    end.

import_name(<<SP:8, Rest/binary>>, State, Name) when ?IS_SPACE(SP) ->
    import_name(Rest, incr(State), Name);
import_name(<<SP:8, Rest/binary>>, State, Name) when ?IS_NEWLINE(SP) ->
    import_name(Rest, new_line(State), Name);
import_name(<<A:8, _/binary>>, State, <<>>) when ?IS_NUMBER(A) ->
    throw({error, {eparse, State, import_name_invalid}});
import_name(<<"*.", _/binary>>, State, _Name) ->
    throw({error, {eparse, State, import_generic_wrong}});
import_name(<<"*;", Rest/binary>>, State, Name) ->
    {<<";", Rest/binary>>, incr(State, 1), <<Name/binary, "*">>};
import_name(<<A:8, _/binary>> = Rest, State, Name)
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    case key_name(Rest, State, <<>>) of
        {<<";", _/binary>> = Rest1, State1, Name1} ->
            {Rest1, State1, <<Name/binary, Name1/binary>>};
        {<<".", Rest1/binary>>, State1, Name1} ->
            import_name(Rest1, State1, <<Name/binary, Name1/binary, ".">>);
        {_Rest1, State1, _Name1} ->
            throw({error, {eparse, State1, import_name_wrong}})
    end.

key_name(<<A:8, _/binary>>, State, <<>>) when ?IS_NUMBER(A) ->
    throw({error, {eparse, State, key_name_invalid}});
key_name(<<A:8, Rest/binary>>, State, Name)
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    key_name(Rest, incr(State), <<Name/binary, A:8>>);
key_name(Rest, State, Parsed) ->
    {Rest, State, Parsed}.

interface(_Rest, State, #interface{}) ->
    %% TODO: not supported... yet
    throw({error, {eparse, State, interface_not_supported}}).

class_meta(<<SP:8, Rest/binary>>, State, Class) when ?IS_SPACE(SP) ->
    class_meta(Rest, incr(State), Class);
class_meta(<<SP:8, Rest/binary>>, State, Class) when ?IS_NEWLINE(SP) ->
    class_meta(Rest, new_line(State), Class);
class_meta(<<"extends", SP:8, Rest/binary>>, State,
           #class{extends = undefined} = Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0, Extends} = key_name(Rest, incr(State, 8), <<>>),
    class_meta(Rest0, State0, Class#class{extends = Extends});
class_meta(<<"implements", SP:8, Rest/binary>>, State,
           #class{implements = []} = Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 11)),
    {Rest1, State1, Implements} = implements(Rest0, State0, []),
    class_meta(Rest1, State1, Class#class{implements = Implements});
class_meta(<<"{", _/binary>> = Rest, State, Class) ->
    {Rest, State, Class}.

implements(Rest, State, Implements) ->
    {Rest0, State0, ImplmentName} = key_name(Rest, State, <<>>),
    case remove_spaces(Rest0, State0) of
        {<<",", Rest1/binary>>, State1} ->
            {Rest2, State2} = remove_spaces(Rest1, incr(State1)),
            implements(Rest2, State2, Implements ++ [ImplmentName]);
        {Rest1, State1} ->
            {Rest1, State1, Implements ++ [ImplmentName]}
    end.

class(<<SP:8, Rest/binary>>, State, Class) when ?IS_SPACE(SP) ->
    class(Rest, incr(State), Class);
class(<<SP:8, Rest/binary>>, State, Class) when ?IS_NEWLINE(SP) ->
    class(Rest, new_line(State), Class);
class(<<"//", Rest/binary>>, State, Class) ->
    {Rest0, State0} = comment_line(Rest, State),
    class(Rest0, State0, Class);
class(<<"/*", Rest/binary>>, State, Class) ->
    {Rest0, State0} = comment_block(Rest, State),
    class(Rest0, State0, Class);
class(<<"public", SP:8, Rest/binary>>, #state{access = undefined} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{access = public}, 7), Class);
class(<<"public", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_public}});
class(<<"protected", SP:8, Rest/binary>>, #state{access = undefined} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{access = protected}, 10), Class);
class(<<"protected", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_protected}});
class(<<"private", SP:8, Rest/binary>>, #state{access = undefined} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{access = private}, 8), Class);
class(<<"private", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_access_private}});
class(<<"abstract", SP:8, Rest/binary>>, #state{abstract = false} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{abstract = true}, 9), Class);
class(<<"abstract", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_abstract}});
class(<<"static", SP:8, Rest/binary>>, #state{static = false} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{static = true}, 7), Class);
class(<<"static", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_static}});
class(<<"final", SP:8, Rest/binary>>, #state{final = false} = State,
      Class) when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    class(Rest, incr(State#state{final = true}, 7), Class);
class(<<"final", SP:8, _Rest/binary>>, State, _Class)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    throw({error, {eparse, State, double_final}});
class(<<A:8, _/binary>> = Rest, State, Class)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, State0, Name} = key_name(Rest, State, <<>>),
    case remove_spaces(Rest0, State0) of
        {<<B:8, _/binary>> = Rest1, State1}
                when ?IS_ALPHA(B) orelse B =:= $_ ->
            %% TODO throw an error if abstract was set
            Attrib1 = add_line(#attribute{
                type = Name,
                static = State#state.static,
                access = State#state.access,
                final = State#state.final
            }, State),
            {Rest2, State2, Attrib2} = attribute(Rest1, State1, Attrib1),
            class(Rest2, reset_state(State2), add_attrib(Class, Attrib2));
        {<<"(", Rest1/binary>>, State1} ->
            {Rest2, State2, Params} = method_params(Rest1, incr(State1), []),
            Method = add_line(#method{
                return = Name,
                static = State#state.static,
                access = State#state.access,
                abstract = State#state.abstract,
                final = State#state.final,
                params = Params
            }, State),
            %% TODO add the code part
            class(Rest2, reset_state(State2), add_method(Class, Method))
    end.

add_method(#class{methods = Methods} = Class, #method{} = Method) ->
    Class#class{methods = [Method|Methods]}.

method_params(<<SP:8, Rest/binary>>, State, Params) when ?IS_SPACE(SP) ->
    method_params(Rest, incr(State), Params);
method_params(<<SP:8, Rest/binary>>, State, Params) when ?IS_NEWLINE(SP) ->
    method_params(Rest, new_line(State), Params);
method_params(<<")", Rest/binary>>, State, Params) ->
    {Rest, incr(State), Params};
method_params(Rest, State, Params) ->
    case method_param(Rest, incr(State), add_line(#method_param{}, State)) of
        {<<")", Rest0/binary>>, State0, Param} ->
            {Rest0, incr(State0), Params ++ [Param]};
        {<<",", Rest0/binary>>, State0, Param} ->
            method_params(Rest0, incr(State0), Params ++ [Param]);
        {Rest0, State0, _Param} ->
            throw({error, {eparse, State0, {unexpected_param, Rest0}}})
    end.

method_param(Rest, State, MethodParam) ->
    {Rest0, State0, Type} = key_name(Rest, State, <<>>),
    {Rest1, State1} = remove_spaces(Rest0, State0),
    {Rest2, State2, Name} = key_name(Rest1, State1, <<>>),
    case remove_spaces(Rest2, State2) of
        {<<"=", _/binary>>, State3} ->
            %% TODO: literal (string, number or constant)
            throw({error, {eparse, State3, default_values_not_supported}});
        {Rest3, State3} ->
            {Rest3, State3, MethodParam#method_param{type = Type, name = Name}}
    end.

add_attrib(#class{attributes = Attribs} = Class, #attribute{} = Attrib) ->
    Class#class{attributes = [Attrib|Attribs]}.

attribute(<<SP:8, Rest/binary>>, State, Attrib) when ?IS_SPACE(SP) ->
    attribute(Rest, incr(State), Attrib);
attribute(<<SP:8, Rest/binary>>, State, Attrib) when ?IS_NEWLINE(SP) ->
    attribute(Rest, new_line(State), Attrib);
attribute(<<"=", Rest/binary>>, State, Attrib) ->
    {<<";", Rest0/binary>>, State0, Expression} =
        noether_parser_expr:expression(Rest, incr(State), []),
    {Rest0, incr(State0), Attrib#attribute{init_value = Expression}};
attribute(<<A:8, _/binary>> = Rest, State,
          #attribute{name = undefined} = Attrib)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {NewRest, NewState, Name} = key_name(Rest, State, <<>>),
    attribute(NewRest, NewState, Attrib#attribute{name = Name});
attribute(_Rest, State, _Attrib) ->
    throw({error, {eparse, State, unexpected_attrib_meta}}).

remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_SPACE(SP) ->
    remove_spaces(Rest, incr(State));
remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_NEWLINE(SP) ->
    remove_spaces(Rest, new_line(State));
remove_spaces(<<>>, State) -> {<<>>, State};
remove_spaces(Rest, State) -> {Rest, State}.

reset_state(State) ->
    State#state{
        access = undefined,
        abstract = false,
        static = false,
        final = false
    }.

incr(State) -> incr(State, 1).
incr(#state{col = Col} = State, N) -> State#state{col = Col + N}.

new_line(#state{row = Row} = State) ->
    State#state{row = Row + 1, col = 1}.

comment_line(<<>>, State) ->
    {<<>>, State};
comment_line(<<A:8, Rest/binary>>, State) when ?IS_NEWLINE(A) ->
    {Rest, new_line(State)};
comment_line(<<_/utf8, Rest/binary>>, State) ->
    comment_line(Rest, incr(State)).

comment_block(<<>>, State) ->
    throw({error, {eparse, State, missing_comment_end}});
comment_block(<<"*/", Rest/binary>>, State) ->
    {Rest, incr(State, 2)};
comment_block(<<A:8, Rest/binary>>, State) when ?IS_NEWLINE(A) ->
    comment_block(Rest, new_line(State));
comment_block(<<_/utf8, Rest/binary>>, State) ->
    comment_block(Rest, incr(State)).
