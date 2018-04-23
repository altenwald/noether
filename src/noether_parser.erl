-module(noether_parser).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([parse/1, file/1]).

-export([incr/1,
         incr/2,
         new_line/1,
         comment_block/2,
         comment_line/2,
         key_name/2,
         type/2,
         remove_spaces/2,
         method_params/3,
         add_line/2]).

-include("noether_parser.hrl").

file(File) ->
    {ok, Content} = file:read_file(File),
    parse(Content).

parse(Content) when is_list(Content) ->
    parse(list_to_binary(Content));
parse(Content) ->
    {_, _, Parsed} = document(Content, #state{}, undefined),
    Parsed.

?ADD_LINE(array);
?ADD_LINE(assign);
?ADD_LINE(attribute);
?ADD_LINE(char);
?ADD_LINE(class);
?ADD_LINE(constant);
?ADD_LINE(if_block);
?ADD_LINE(instance);
?ADD_LINE(int);
?ADD_LINE(interface);
?ADD_LINE(float);
?ADD_LINE(method);
?ADD_LINE(method_param);
?ADD_LINE(operator);
?ADD_LINE(package);
?ADD_LINE(return);
?ADD_LINE(text);
?ADD_LINE(variable).

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
    {Rest1, State1, InterfaceName} = key_name(Rest0, State0),
    %% TODO add the rest of the attributes for the interface
    Interface0 = add_line(#interface{name = InterfaceName}, State),
    {Rest2, State2, Interface} = interface(Rest1, reset_state(State1), Interface0),
    package(Rest2, reset_state(State2), add_interface(Package, Interface));
package(<<"class", SP:8, Rest/binary>>, State, Package)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
    {Rest0, State0} = remove_spaces(Rest, incr(State, 6)),
    {Rest1, State1, ClassName} = key_name(Rest0, State0),
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
    {<<";", Rest1/binary>>, State1, Import} = import_name(Rest0, State0, []),
    package(Rest1, incr(State1), add_import(Package, Import));
package(<<>>, State, Package) ->
    {<<>>, State, Package};
package(<<A:8, _/binary>>, State, _Parsed) ->
    throw({error, {eparse, State, {not_expected, <<A:8>>}}}).

add_import(#package{imports = Imports} = Package, Import) ->
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
    case key_name(Rest, State) of
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
import_name(<<A:8, _/binary>>, State, _Name) when ?IS_NUMBER(A) ->
    throw({error, {eparse, State, import_name_invalid}});
import_name(<<"*.", _/binary>>, State, _Name) ->
    throw({error, {eparse, State, import_generic_wrong}});
import_name(<<"*", _/binary>>, State, []) ->
    throw({error, {eparse, State, import_generic_wrong}});
import_name(<<"*;", Rest/binary>>, State, Name) ->
    {<<";", Rest/binary>>, incr(State, 1), Name ++ [<<"*">>]};
import_name(<<A:8, _/binary>> = Rest, State, Name)
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    case key_name(Rest, State) of
        {<<";", _/binary>> = Rest1, State1, Name1} ->
            {Rest1, State1, Name ++ [Name1]};
        {<<".", Rest1/binary>>, State1, Name1} ->
            import_name(Rest1, State1, Name ++ [Name1]);
        {_Rest1, State1, _Name1} ->
            throw({error, {eparse, State1, import_name_wrong}})
    end.

key_name(Text, State) ->
    key_name(Text, State, <<>>).

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
    {Rest0, State0, Extends} = key_name(Rest, incr(State, 8)),
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
    {Rest0, State0, ImplmentName} = key_name(Rest, State),
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
class(<<A:8, _/binary>> = Rest, #state{type = undefined} = State, Class)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    case type(Rest, State) of
        {<<"(", Rest0/binary>>, State0, Name} ->
            {Rest1, State1, Method} = method(Rest0, incr(State0), Name),
            class(Rest1, reset_state(State1), add_method(Class, Method));
        {Rest0, State0, Type} ->
            class(Rest0, State0#state{type = Type}, Class)
    end;
class(<<A:8, _/binary>> = Rest, State, Class)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, State0, Name} = key_name(Rest, State),
    case remove_spaces(Rest0, State0) of
        {<<"(", Rest1/binary>>, State1} ->
            {Rest2, State2, Method} = method(Rest1, incr(State1), Name),
            class(Rest2, reset_state(State2), add_method(Class, Method));
        {Rest1, State1} ->
            %% TODO throw an error if abstract was set
            Attrib1 = add_line(#attribute{
                name = Name,
                type = State#state.type,
                static = State#state.static,
                access = State#state.access,
                final = State#state.final
            }, State),
            {Rest2, State2, Attrib2} = attribute(Rest1, State1, Attrib1),
            class(Rest2, reset_state(State2), add_attrib(Class, Attrib2))
    end;
class(<<"}", Rest/binary>>, State, Class) ->
    {Rest, incr(State), Class}.

method(Rest, State, Name) ->
    {Rest0, State0, Params} = method_params(Rest, State, []),
    case remove_spaces(Rest0, State0) of
        {<<"{", _/binary>> = Rest1, State1} ->
            {Rest2, State2, Code} = code_block(Rest1, State1, []),
            Method = add_line(#method{
                name = Name,
                return = State#state.type,
                static = State#state.static,
                access = State#state.access,
                abstract = State#state.abstract,
                final = State#state.final,
                params = Params,
                code = Code
            }, State),
            {Rest2, State2, Method};
        {_Rest1, State1} ->
            throw({error, {eparse, State1, missing_code}})
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
    {{Rest0, State0}, MethodParam0} = case type(Rest, State) of
        {Rest0_0, State0_0, <<"final">>} ->
            {R0, S0, T} = type(Rest0_0, State0_0),
            {{R0, S0}, MethodParam#method_param{type = T, final = true}};
        {Rest0_1, State0_1, T} ->
            {{Rest0_1, State0_1}, MethodParam#method_param{type = T}}
    end,
    {Rest1, State1, MethodParam1} = case modifiers(Rest0, State0, []) of
        {Rest1_0, State1_0, []} ->
            {Rest1_0, State1_0, MethodParam0};
        {Rest1_1, State1_1, Mod} ->
            {Rest1_1, State1_1, MethodParam0#method_param{type = [T|Mod]}}
    end,
    {Rest2, State2} = remove_spaces(Rest1, State1),
    {Rest3, State3, Name} = key_name(Rest2, State2),
    case remove_spaces(Rest3, State3) of
        {<<"=", _/binary>>, State4} ->
            %% TODO: literal (string, number or constant)
            throw({error, {eparse, State4, default_values_not_supported}});
        {Rest4, State4} ->
            MethodParam2 = MethodParam1#method_param{name = Name},
            {Rest4, State4, MethodParam2}
    end.

modifiers(<<"[]", Rest/binary>>, State, Modifiers) ->
    modifiers(Rest, State, [array|Modifiers]);
modifiers(Rest, State, Modifiers) ->
    {Rest, State, lists:reverse(Modifiers)}.

type(Text, State) ->
    type(Text, State, <<>>).

type(<<A:8, _/binary>>, State, <<>>) when ?IS_NUMBER(A) ->
    throw({error, {eparse, State, type_invalid}});
type(<<A:8, Rest/binary>>, State, Name)
        when ?IS_ALPHANUM(A) orelse A =:= $_ ->
    type(Rest, incr(State), <<Name/binary, A:8>>);
type(<<"[]", Rest/binary>>, State, Name) ->
    {Rest, incr(State, 2), {array, Name}};
type(<<"<", Rest/binary>>, State, Name) ->
    case key_name(Rest, incr(State), <<>>) of
        {<<">", Rest0/binary>>, State0, SubTypeName} ->
            {Rest0, incr(State0), {template, Name, SubTypeName}};
        {_Rest0, _State0, _Whatever} ->
            throw({error, {eparse, State, template_wrong}})
    end;
type(Rest, State, Parsed) ->
    {Rest, State, Parsed}.

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
attribute(_Rest, State, Attrib) ->
    throw({error, {eparse, State, {unexpected_attrib_meta, Attrib}}}).

remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_SPACE(SP) ->
    remove_spaces(Rest, incr(State));
remove_spaces(<<SP:8, Rest/binary>>, State) when ?IS_NEWLINE(SP) ->
    remove_spaces(Rest, new_line(State));
remove_spaces(Rest, State) -> {Rest, State}.

reset_state(State) ->
    State#state{
        access = undefined,
        abstract = false,
        static = false,
        final = false,
        type = undefined
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

code_block(Rest, State, Parsed) ->
    case remove_spaces(Rest, State) of
        {<<"{", Rest0/binary>>, State0} ->
            case code(Rest0, State0) of
                {<<"}", Rest1/binary>>, State1, Code} ->
                    {Rest1, incr(State1), Parsed ++ [Code]};
                {<<";", Rest1/binary>>, State1, Code} ->
                    code_block(Rest1, incr(State1), Parsed ++ [Code]);
                {_Rest1, State1, _Code} ->
                    throw({error, {eparse, State1, missing_separator}})
            end;
        {<<"}", Rest0/binary>>, State0} ->
            {Rest0, incr(State0), Parsed}
    end.

code(<<"break", SP:8, Rest/binary>>, State)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $; ->
    {Rest0, State0} = remove_spaces(<<SP:8, Rest/binary>>, incr(State, 5)),
    {Rest0, State0, break};
code(<<"continue", SP:8, Rest/binary>>, State)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $; ->
    {Rest0, State0} = remove_spaces(<<SP:8, Rest/binary>>, incr(State, 8)),
    {Rest0, State0, continue};
code(<<"return", SP:8, Rest/binary>>, State)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $; ->
    Rest0 = <<SP:8, Rest/binary>>,
    State0 = incr(State, 6),
    case noether_parser_expr:expression(Rest0, State0, []) of
        {Rest1, State1, []} ->
            {Rest1, State1, add_line(#return{}, State)};
        {Rest1, State1, Return} ->
            {Rest1, State1, add_line(#return{value = Return}, State)}
    end;
code(<<"true", SP:8, Rest/binary>>, State)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $; ->
    Rest0 = <<SP:8, Rest/binary>>,
    State0 = incr(State, 4),
    Parser = noether_parser_expr:add_op(true, []),
    noether_parser_expr:expression(Rest0, State0, Parser);
code(<<"false", SP:8, Rest/binary>>, State)
        when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $; ->
    Rest0 = <<SP:8, Rest/binary>>,
    State0 = incr(State, 6),
    Parser = noether_parser_expr:add_op(false, []),
    noether_parser_expr:expression(Rest0, State0, Parser);
% code(<<"if", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $( ->
%     st_if(<<SP:8, Rest/binary>>, incr(State, 2));
% code(<<"while", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $( ->
%     st_while(<<SP:8, Rest/binary>>, incr(State, 5));
% code(<<"do", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= ${ ->
%     st_do_while(<<SP:8, Rest/binary>>, incr(Pos, 2));
% code(<<"for", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $( ->
%     st_for(<<SP:8, Rest0/binary>>, incr(State, 3));
% code(<<"switch", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $( ->
%     st_switch(<<SP:8, Rest/binary>>, incr(State, 6));
% code(<<"case", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) ->
%     {<<SP:8, Rest/binary>>, incr(State, 4), case_entry};
% code(<<"default", SP:8, Rest/binary>>, State)
%         when ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $: ->
%     case remove_spaces(<<SP:8, Rest/binary>>, incr(State, 4)) of
%         {<<":", Rest0/binary>>, State0} ->
%             {Rest0, incr(State0, 1), default_entry};
%         {Rest0, State0} ->
%             throw({error, {eparse, State, switch_def_wrong}})
%     end;
code(<<"//", Rest/binary>>, State) ->
    {Rest0, State0} = comment_line(Rest, incr(State, 2)),
    code(Rest0, State0);
code(<<"/*", Rest/binary>>, State) ->
    {Rest0, State0} = comment_block(Rest, incr(State, 2)),
    code(Rest0, State0);
code(<<A:8,_/binary>> = Rest, State) when ?IS_ALPHA(A) orelse A =:= $_
                                     orelse ?IS_NUMBER(A)
                                     orelse A =:= $- orelse A =:= $(
                                     orelse A =:= $" orelse A =:= $'
                                     orelse A =:= $$ orelse A =:= $+
                                     orelse A =:= 126 orelse A =:= $! ->
    noether_parser_expr:expression(Rest, State, []);
code(<<Space:8, Rest/binary>>, State) when ?IS_SPACE(Space) ->
    code(Rest, incr(State));
code(<<NewLine:8, Rest/binary>>, State) when ?IS_NEWLINE(NewLine) ->
    code(Rest, new_line(State));
code(<<A:8, Rest/binary>>, State) when ?IS_ALPHA(A) orelse A =:= $_ ->
    noether_parser_expr:expression(<<A:8, Rest/binary>>, State, []);
code(Text, State) ->
    throw({error, {eparse, State, {unexpected, Text}}}).
