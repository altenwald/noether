-module(noether_parser_expr).
-author('manuel@altenwald.com').
-compile([warnings_as_errors]).

-export([expression/3]).

-import(noether_parser, [incr/1,
                         incr/2,
                         new_line/1,
                         remove_spaces/2,
                         add_line/2]).

-include("noether_parser.hrl").

resolve([]) -> [];
resolve([{op, Content}]) -> solve(Content).

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
% NEW ...
expression(<<"new", SP:8, Rest/binary>>, State, Parsed) when
        ?IS_SPACE(SP) orelse ?IS_NEWLINE(SP) orelse SP =:= $( ->
    {Rest0, State0} = noether_parser:remove_spaces(<<SP:8, Rest/binary>>, State),
    {Rest1, State1, ObjName} = noether_parser:key_name(Rest0, State0),
    case noether_parser:remove_spaces(Rest1, State1) of
        {<<"(", Rest2/binary>>, State2} ->
            {Rest3, State3, Args} = call_args(Rest2, State2, []),
            Instance = add_line(#instance{name = ObjName, args = Args}, State),
            expression(Rest3, State3, add_op(Instance, Parsed));
        {_Rest3, State3} ->
            throw({error, {eparse, State3, missing_construct_params}})
    end;
% COMMENTS
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
% AND, XOR
expression(<<Op:3/binary, SP:8, Rest/binary>>, State, Parsed)
        when (Op =:= <<"and">> orelse Op =:= <<"xor">>)
        andalso not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    Parsed0 = add_op({Op, precedence(Op), State}, Parsed),
    expression(Rest, incr(State, 3), Parsed0);
% OPERATOR 3 LETTERS
expression(<<Op:3/binary, Rest/binary>>, State, Parsed) when ?IS_OP3(Op) ->
    Parsed0 = add_op({Op, precedence(Op), State}, Parsed),
    expression(Rest, incr(State, 3), Parsed0);
% OR
expression(<<"or", SP:8, Rest/binary>>, State, Parsed)
        when not (?IS_ALPHA(SP) orelse ?IS_NUMBER(SP) orelse SP =:= $_) ->
    Parsed0 = add_op({<<"or">>, precedence(<<"or">>), State}, Parsed),
    expression(Rest, incr(State, 2), Parsed0);
% OPERATORS 2 LETTERS
expression(<<Op:2/binary, Rest/binary>>, State, Parsed) when ?IS_OP2(Op) ->
    Parsed0 = add_op({Op, precedence(Op), State}, Parsed),
    expression(Rest, incr(State, 2), Parsed0);
% OPERATORS 1 LETTER
expression(<<Op:1/binary, Rest/binary>>, State, Parsed) when ?IS_OP1(Op) ->
    Parsed0 = add_op({Op, precedence(Op), State}, Parsed),
    expression(Rest, incr(State), Parsed0);
% VARIABLE / FUNCTION
expression(<<A:8, _/binary>> = Rest, State, Parsed)
        when ?IS_ALPHA(A) orelse A =:= $_ ->
    {Rest0, State0, Var} = variable(Rest, State, add_line(#variable{}, State)),
    expression(Rest0, State0, add_op(Var, Parsed));
expression(<<>>, State, _Parsed) ->
    throw({error, {eparse, State, unexpected_end_expression}}).


variable(Rest, State, Variable) ->
    {Rest0, State0, Name} = noether_parser:key_name(Rest, State),
    {Rest1, State1, Idx} = var_access(Rest0, State0, []),
    {Rest1, State1, Variable#variable{name = Name, idx = Idx}}.


var_access(<<".", Rest/binary>>, State, VarAccess) ->
    {Rest0, State0, Name} = noether_parser:key_name(Rest, incr(State)),
    var_access(Rest0, State0, VarAccess ++ [{object, Name}]);
var_access(<<"[", Rest/binary>>, State, VarAccess) ->
    {<<"]", Rest0/binary>>, State0, Idx} = expression(Rest, incr(State), []),
    var_access(Rest0, incr(State0), VarAccess ++ [{array, Idx}]);
var_access(<<"(", Rest/binary>>, State, VarAccess) ->
    {<<")", Rest0/binary>>, State0, Args} = funct_args(Rest, incr(State), []),
    var_access(Rest0, incr(State0), VarAccess ++ [{call, Args}]);
var_access(Rest, State, VarAccess) ->
    {Rest, State, VarAccess}.


funct_args(Rest, State, Args) ->
    case expression(Rest, State, []) of
        {<<",", Rest0/binary>>, State0, Exp} ->
            funct_args(Rest0, incr(State0), Args ++ [Exp]);
        {Rest0, State0, Exp} ->
            {Rest0, State0, Args ++ [Exp]}
    end.


call_args(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_SPACE(SP) ->
    call_args(Rest, incr(State), Parsed);
call_args(<<SP:8, Rest/binary>>, State, Parsed) when ?IS_NEWLINE(SP) ->
    call_args(Rest, new_line(State), Parsed);
call_args(<<")",Rest/binary>>, State, Parsed) ->
    {Rest, incr(State), Parsed};
call_args(Rest, State, Parsed) when Rest =/= <<>> ->
    case expression(Rest, State, []) of
        {<<")",Rest0/binary>>, State0, []} ->
            {Rest0, incr(State0), Parsed};
        {<<")",Rest0/binary>>, State0, Arg} ->
            {Rest0, incr(State0), Parsed ++ [Arg]};
        {<<",",Rest0/binary>>, State0, Arg} ->
            call_args(Rest0, incr(State0), Parsed ++ [Arg]);
        {<<";",_/binary>>, State0, _} ->
            throw({error, {eparse, State0, {unexpected, <<";">>}}});
        {Rest, State, _} ->
            throw({error, {eparse, State, Rest}});
        {Rest0, State0, []} ->
            call_args(Rest0, State0, Parsed);
        {Rest0, State0, Arg} ->
            call_args(Rest0, State0, Parsed ++ [Arg])
    end.


%% took from https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html

precedence(<<"=">>) -> {right, 14}; %% assign
precedence(<<"+=">>) -> {right, 14};
precedence(<<"-=">>) -> {right, 14};
precedence(<<"*=">>) -> {right, 14};
precedence(<<"**=">>) -> {right, 14};
precedence(<<"/=">>) -> {right, 14};
precedence(<<".=">>) -> {right, 14};
precedence(<<"%=">>) -> {right, 14};
precedence(<<"&=">>) -> {right, 14};
precedence(<<"|=">>) -> {right, 14};
precedence(<<"^=">>) -> {right, 14};
precedence(<<"<<=">>) -> {right, 14};
precedence(<<">>=">>) -> {right, 14};
precedence(<<"?">>) -> {right, 13}; %% ternary
precedence(<<":">>) -> {right, 12}; %% ternary
precedence(<<"||">>) -> {left, 11}; %% logic
precedence(<<"or">>) -> {left, 11};
precedence(<<"xor">>) -> {left, 11};
precedence(<<"&&">>) -> {left, 10}; %% logic
precedence(<<"and">>) -> {left, 10};
precedence(<<"|">>) -> {left, 9}; %% bit by bit
precedence(<<"^">>) -> {left, 8}; %% bit by bit
precedence(<<"&">>) -> {left, 7}; %% bit by bit & references
precedence(<<"==">>) -> {left, 6};
precedence(<<"!=">>) -> {left, 6};
precedence(<<"===">>) -> {left, 6};
precedence(<<"!==">>) -> {left, 6};
precedence(<<"<>">>) -> {left, 6};
precedence(<<"<=>">>) -> {left, 6};
precedence(<<"<">>) -> {left, 5};
precedence(<<"<=">>) -> {left, 5};
precedence(<<">">>) -> {left, 5};
precedence(<<">=">>) -> {left, 5};
precedence(<<"instanceof">>) -> {left, 5};
precedence(<<"<<">>) -> {left, 4}; %% bit by bit
precedence(<<">>">>) -> {left, 4}; %% bit by bit
precedence(<<">>>">>) -> {left, 4}; %% bit by bit
precedence(<<"+">>) -> {left, 3};
precedence(<<"-">>) -> {left, 3};
precedence(<<"*">>) -> {left, 2};
precedence(<<"/">>) -> {left, 2};
precedence(<<"%">>) -> {left, 2};
precedence(<<"**">>) -> {right, 1}; %% arith
precedence(<<"++">>) -> {right, 1};
precedence(<<"--">>) -> {right, 1};
precedence(<<$~>>) -> {right, 1};
precedence(<<"!">>) -> {right, 1}; %% logic
%% cast is right, 1
precedence(_) -> false.

% process_incr_decr(Content) ->
%     process_incr_decr(Content, []).

% process_incr_decr([], Processed) ->
%     Processed;
% process_incr_decr([{<<"++">>,_,Pos},#variable{}=V|Rest], Processed) ->
%     process_incr_decr(Rest, Processed ++ [{pre_incr, V, Pos}]);
% process_incr_decr([{<<"--">>,_,Pos},#variable{}=V|Rest], Processed) ->
%     process_incr_decr(Rest, Processed ++ [{pre_decr, V, Pos}]);
% process_incr_decr([#variable{}=V,{<<"++">>,_,Pos}|Rest], Processed) ->
%     process_incr_decr(Rest, Processed ++ [{post_incr, V, Pos}]);
% process_incr_decr([#variable{}=V,{<<"--">>,_,Pos}|Rest], Processed) ->
%     process_incr_decr(Rest, Processed ++ [{post_decr, V, Pos}]);
% process_incr_decr([A|Rest], Processed) ->
%     process_incr_decr(Rest, Processed ++ [A]).

solve(Expression) ->
    Postfix = shunting_yard(parse_negative(Expression), [], []),
    [Operation] = gen_op(Postfix, []),
    Operation.

operator(O, A, B) -> #operator{sign = O, left = A, right = B}.

gen_op([], Stack) ->
    Stack;
gen_op([{<<"=">>,{_,_},Pos}|Rest], [B,{operation_not, A, Line}|Stack]) ->
    Assign = add_line(#assign{variable=A, expression=B}, Pos),
    gen_op(Rest, [{operation_not, Assign, Line}|Stack]);
gen_op([{<<"=">>,{_,_},Pos}|Rest], [B,A|Stack]) ->
    Assign = add_line(#assign{variable=A, expression=B}, Pos),
    gen_op(Rest, [Assign|Stack]);
gen_op([{<<O:1/binary,"=">>,{_,_},Pos}|Rest], [B,A|Stack])
        when ?IS_OP1_ARITH(O) ->
    Op = add_line(operator(O, A, B), Pos),
    Assign = add_line(#assign{variable=A, expression=Op}, Pos),
    gen_op(Rest, [Assign|Stack]);
gen_op([{<<126>>,{_,_},{_,R,C}}|Rest], [A|Stack]) ->
    gen_op(Rest, [{operation_bnot, A, {{line,R},{column,C}}}|Stack]);
gen_op([{<<"!">>,{_,_},{_,R,C}}|Rest], [A|Stack]) ->
    gen_op(Rest, [{operation_not, A, {{line,R},{column,C}}}|Stack]);
gen_op([#constant{name = <<"break">>}, #int{int=I}], []) ->
    [{break, I}];
gen_op([#constant{name = <<"continue">>}, #int{int=I}], []) ->
    [{continue, I}];
gen_op([{<<"->">>,{_,_},Pos}|Rest], [B,#variable{idx=Idx}=A|Stack]) ->
    Object = case B of
        #int{int = I} -> add_line({object, noether_data:to_bin(I)}, Pos);
        #float{float = F} -> add_line({object, noether_data:to_bin(F)}, Pos);
        #constant{name = Name} -> add_line({object, Name}, Pos);
        #variable{} -> add_line({object, B}, Pos);
        % #call{} -> add_line({object, B}, Pos);
        _ -> throw({eparse, Pos,
                         {<<"`\"identifier (T_STRING)\"' or "
                            "`\"variable (T_VARIABLE)\"' or "
                            "`'{'' or `'$''">>}})
    end,
    gen_op(Rest, [A#variable{idx = Idx ++ [Object]}|Stack]);
gen_op([{<<"++">>, {_,_}, Pos}|Rest], [V|Stack]) ->
    gen_op(Rest, [{post_incr, V, Pos}|Stack]);
gen_op([{<<"--">>, {_,_}, Pos}|Rest], [V|Stack]) ->
    gen_op(Rest, [{post_decr, V, Pos}|Stack]);
% TODO add the rest of casting operators
gen_op([{<<"?">>,{_,_},Pos}|Rest],
       [#operator{sign = <<":">>} = OpElse, Cond|Stack]) ->
    #operator{
        left = TrueBlock,
        right = FalseBlock
    } = OpElse,
    IfBlock = #if_block{
        conditions = Cond,
        true_block = TrueBlock,
        false_block = FalseBlock,
        line = Pos
    },
    gen_op(Rest, [IfBlock|Stack]);
gen_op([{<<"?">>,{_,_},Pos}|_Rest], _Stack) ->
    throw({eparse, Pos, <<>>});
gen_op([{Op,{_,_},Pos}|Rest], [B,A|Stack]) ->
    gen_op(Rest, [add_line(operator(Op,A,B),Pos)|Stack]);
gen_op([A|Rest], Stack) ->
    gen_op(Rest, [A|Stack]).

parse_negative(Elements) ->
    parse_negative(lists:reverse(Elements), []).

parse_negative([#int{}=I,{<<"-">>,{_,_},_},{_,{_,_},_}=Op|Rest], Stack) ->
    parse_negative([I#int{int=-I#int.int},Op|Rest], Stack);
parse_negative([#float{}=F,{<<"-">>,{_,_},_},{_,{_,_},_}=Op|Rest], Stack) ->
    parse_negative([F#float{float=-F#float.float},Op|Rest], Stack);
parse_negative([A,{<<"-">>,{_,_},_},{_,{_,_},_}=Op|Rest], Stack) ->
    parse_negative([{operation_minus, A, undefined},Op|Rest], Stack);
parse_negative([#int{}=I,{<<"-">>,{_,_},_}], Stack) ->
    [I#int{int=-I#int.int}|Stack];
parse_negative([#float{}=F,{<<"-">>,{_,_},_}], Stack) ->
    [F#float{float=-F#float.float}|Stack];
parse_negative([A,{<<"-">>,{_,_},_}], Stack) ->
    [{operation_minus, A, undefined}|Stack];
parse_negative([A|Rest], Stack) ->
    parse_negative(Rest, [A|Stack]);
parse_negative([], Stack) ->
    Stack.

shunting_yard([], [], Postfix) ->
    Postfix;
shunting_yard([], OpS, Postfix) ->
    Postfix ++ OpS;
shunting_yard([{_,{_,_},_} = Op|Rest], [], Postfix) ->
    shunting_yard(Rest, [Op], Postfix);
shunting_yard([#variable{name = Name, type = undefined},
               #variable{type = undefined} = V|Rest], Stack, Postfix) ->
    shunting_yard([V#variable{type = Name}|Rest], Stack, Postfix);
shunting_yard([{_,{left,P1},_} = Op|Rest], [{_,{_,P2},_} = Op1|OpS], Postfix)
        when P1 > P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix ++ [Op1]);
shunting_yard([{_,{_,P1},_} = Op|Rest], [{_,{_,P2},_} = Op1|OpS], Postfix)
        when P1 >= P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix ++ [Op1]);
shunting_yard([{_,{left,P1},_} = Op|Rest], [{_,{_,P2},_}|_] = OpS, Postfix)
        when P1 =< P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix);
shunting_yard([{_,{_,P1},_} = Op|Rest], [{_,{_,P2},_}|_] = OpS, Postfix)
        when P1 < P2 ->
    shunting_yard(Rest, [Op|OpS], Postfix);
shunting_yard([A|Rest], OpS, Postfix) ->
    shunting_yard(Rest, OpS, Postfix ++ [A]).
