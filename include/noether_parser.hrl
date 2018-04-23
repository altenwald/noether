%% Author: Manuel Rubio <manuel@altenwald.com>

-define(IS_SPACE(X),
    erlang:'or'(X =:= 32,
                erlang:'or'(X =:= $\t,
                            X =:= $\r)
               )
).
-define(IS_NEWLINE(X),
    X =:= $\n
).
-define(IS_NUMBER(X),
    erlang:'and'(X >= $0, X =< $9)
).
-define(IS_ALPHA(X),
    erlang:'or'(
        erlang:'and'(X >= $A, X =< $Z),
        erlang:'and'(X >= $a, X =< $z)
    )
).
-define(IS_ALPHANUM(X), erlang:'or'(?IS_NUMBER(X), ?IS_ALPHA(X))).
-define(IS_HEX(X),
    erlang:'or'(
        ?IS_NUMBER(X),
        erlang:'or'(
            erlang:'and'(X >= $A, X =< $F),
            erlang:'and'(X >= $a, X =< $f)
        )
    )
).
-define(IS_OCT(X), erlang:'and'(X >= $0, X =< $7)).

-define(OR(I,X,Y), erlang:'or'(I =:= X, I =:= Y)).

-define(ADD_LINE(Record),
    add_line(#Record{} = A, #state{row = R}) -> A#Record{line = R}
).

-define(IS_OP1_ARITH(X),
    X =:= <<"*">> orelse
    X =:= <<"/">> orelse
    X =:= <<"%">> orelse
    X =:= <<"+">> orelse
    X =:= <<"-">> orelse
    X =:= <<".">> orelse
    X =:= <<"&">> orelse
    X =:= <<"^">> orelse
    X =:= <<"|">>
).
-define(IS_OP1(X),
    X =:= <<126>> orelse
    X =:= <<"@">> orelse
    X =:= <<"!">> orelse
    X =:= <<"*">> orelse
    X =:= <<"/">> orelse
    X =:= <<"%">> orelse
    X =:= <<"+">> orelse
    X =:= <<"-">> orelse
    X =:= <<".">> orelse
    X =:= <<"<">> orelse
    X =:= <<">">> orelse
    X =:= <<"&">> orelse
    X =:= <<"^">> orelse
    X =:= <<"|">> orelse
    X =:= <<"=">>
).
-define(IS_OP2(X),
    X =:= <<"**">> orelse
    X =:= <<"++">> orelse
    X =:= <<"--">> orelse
    X =:= <<"<<">> orelse
    X =:= <<">>">> orelse
    X =:= <<"<=">> orelse
    X =:= <<">=">> orelse
    X =:= <<"==">> orelse
    X =:= <<"!=">> orelse
    X =:= <<"<>">> orelse
    X =:= <<"&&">> orelse
    X =:= <<"||">> orelse
    X =:= <<"??">> orelse
    X =:= <<"+=">> orelse
    X =:= <<"-=">> orelse
    X =:= <<"*=">> orelse
    X =:= <<"/=">> orelse
    X =:= <<".=">> orelse
    X =:= <<"%=">> orelse
    X =:= <<"&=">> orelse
    X =:= <<"|=">> orelse
    X =:= <<"^=">> orelse
    X =:= <<"->">> orelse
    X =:= <<"::">>
).
-define(IS_OP3(X),
    X =:= <<"===">> orelse
    X =:= <<"!==">> orelse
    X =:= <<"<=>">> orelse
    X =:= <<"**=">> orelse
    X =:= <<"<<=">> orelse
    X =:= <<">>=">> orelse
    X =:= <<">>>">>
).

-type access() :: undefined | public | private | protected.

-record(state, {
    row = 1 :: integer(),
    col = 1 :: integer(),
    access :: access(),
    static = false :: boolean(),
    final = false :: boolean(),
    abstract = false :: boolean(),
    type :: binary()
}).

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
    final = false :: boolean(),
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

-record(instance, {
    name :: binary(),
    args :: [method_param()],
    line :: pos_integer()
}).

-type instance() :: #instance{}.

-record(return, {
    value :: undefined | term(),
    line :: pos_integer()
}).

-type var_access() :: {object, binary()} | {array, binary()}.

-record(variable, {
    name = <<>> :: binary(),
    idx = [] :: [var_access()],
    type :: binary(),
    line :: pos_integer()
}).

-record(instance, {
    name :: binary(),
    args = [], %% TODO: add type
    line :: pos_integer()
}).

-record(assign, {
    variable,
    expression,
    line :: pos_integer()
}).


-record(operator, {
    sign,
    left,
    right,
    line :: pos_integer()
}).

-record(constant, {
    name,
    value,
    line :: pos_integer()
}).


-record(if_block, {
    conditions,
    true_block,
    false_block,
    line
}).

-record(array, {
    elements,
    line
}).
