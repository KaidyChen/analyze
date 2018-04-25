-module(analyze_get_order_no).

-export([order_no/0]).

-define(STANDARD_SYSTEM_ELEMENTS, [{1,"0"},{2,"1"},{3,"2"},  
      {4,"3"},{5,"4"},{6,"5"},{7,"6"},{8,"7"},{9,"8"},  
      {10,"9"},{11,"A"},{12,"B"},{13,"C"},{14,"D"},  
      {15,"E"},{16,"F"},{17,"G"},{18,"H"},{19,"I"},  
      {20,"J"},{21,"K"},{22,"L"},{23,"M"},{24,"N"},  
      {25,"O"},{26,"P"},{27,"Q"},{28,"R"},{29,"S"},  
      {30,"T"},{31,"U"},{32,"V"},{33,"W"},{34,"X"},  
      {35,"Y"},{36,"Z"},{37,"a"},{38,"b"},{39,"c"},
      {40,"d"},{41,"e"},{42,"f"},{43,"g"},{44,"h"},
      {45,"i"},{46,"j"},{47,"k"},{48,"l"},{49,"m"},
      {50,"n"},{51,"o"},{52,"p"},{53,"q"},{54,"r"},
      {55,"s"},{56,"t"},{57,"u"},{58,"v"},{59,"w"},
      {60,"x"},{61,"y"},{62,"z"}]).

order_no() ->
    {{Y, M, D}, {H, MM, _S}} = calendar:local_time(),
    Datetime_str = lists:flatten(io_lib:format("~2..0w~2..0w~2..0w~2..0w~2..0w", [Y rem 100, M, D, H, MM])),
    Random_str = random_str(32),
    Order_no = lists:flatten([Datetime_str | Random_str]),
    Order_no.

random_str(Count) ->
    random_str(Count, []).

random_str(Count, Random_str) when (Count > 0) ->
    random:seed(erlang:phash2([node()]),
                erlang:monotonic_time(),
          erlang:unique_integer()),
    N = rand:uniform(1000) rem 63,
    case get_alnum_of_n(N) of
        {ok, Alnum} -> random_str(Count-1, [Alnum | Random_str]);
        false       -> random_str(Count, Random_str)
    end;
random_str(0, Random_str) ->
    Random_str.

get_alnum_of_n(N) ->
    case lists:keyfind(N, 1, ?STANDARD_SYSTEM_ELEMENTS) of
        {N, Alnum} ->
            {ok, Alnum};
        false ->
            false
    end.