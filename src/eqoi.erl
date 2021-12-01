-module(eqoi).

-export([
         encode_rgb/1,
         encode_rgba/1,
         decode/1
        ]).

%% RGB or RGBA
-type pixel() :: <<_:24>> | <<_:32>>.

%% 64-element tuple
-type seen() :: {pixel() | undef}.

-record(eqoi_state,
        {
         previous :: pixel(),
         run :: integer(),
         seen :: seen()
        }).

state_initial(InitPixel) ->
    #eqoi_state{
       previous = InitPixel,
       run = 0,
       seen = seen_initial(InitPixel)
      }.

-spec pixel_initial_rgb() -> pixel().
pixel_initial_rgb() ->
    <<0, 0, 0>>.
pixel_initial_rgba() ->
    <<0, 0, 0, 255>>.

-spec full_pixel(pixel()) -> <<_:32>>.
full_pixel(P = <<_:32>>) -> P;
full_pixel(<<RGB:24>>) -> <<RGB:24, 255>>.

-spec seen_initial(pixel()) -> seen().
seen_initial(InitPixel) ->
    seen_add(InitPixel, list_to_tuple(lists:duplicate(64, undef))).

-spec seen_add(pixel(), seen()) -> seen().
seen_add(Pixel, Seen) ->
    setelement(pixel_hash(Pixel)+1, Seen, Pixel).

-spec seen_find(integer(), seen()) -> pixel() | undef.
seen_find(Index, Seen) ->
    element(Index+1, Seen).

-spec seen_update(pixel(), seen()) ->
          {match, integer()} |
          {nomatch, seen()}.
seen_update(Pixel, Seen) ->
    Hash = pixel_hash(Pixel),
    case seen_find(Hash, Seen) of
        Pixel ->
            {match, Hash};
        _ ->
            {nomatch, setelement(Hash+1, Seen, Pixel)}
    end.

-spec pixel_hash(pixel()) -> integer().
pixel_hash(<<R/integer, G/integer, B/integer, A/integer>>) ->
    pixel_hash_positive_bound(R bxor G bxor B bxor A);
pixel_hash(<<R/integer, G/integer, B/integer>>) ->
    pixel_hash_positive_bound(bnot(R bxor G bxor B)).

pixel_hash_positive_bound(N) when N >= 0 ->
    N rem 64;
pixel_hash_positive_bound(N) ->
    pixel_hash_positive_bound(N + 256).

-spec encode_rgb(binary()) -> binary().
encode_rgb(Image) ->
    encode_image(3, Image, state_initial(pixel_initial_rgb()), []).

-spec encode_rgba(binary()) -> binary().
encode_rgba(Image) ->
    encode_image(4, Image, state_initial(pixel_initial_rgba()), []).

encode_image(PixelSize, Image, State, Acc)->
    case Image of
        <<Pixel:PixelSize/binary, Rest/binary>> ->
            {NewData, NewState} = encode_pixel(Pixel, State),
            encode_image(PixelSize, Rest, NewState,
                         case NewData of
                             [] -> Acc;
                             _ -> [NewData | Acc]
                         end);
        <<>> ->
            iolist_to_binary(
              lists:reverse(
                maybe_add_run(State#eqoi_state.run, Acc)))
    end.


-spec encode_pixel(pixel(), #eqoi_state{}) -> {iodata(), #eqoi_state{}}.
encode_pixel(Pixel, State=#eqoi_state{previous=Pixel, run=Run}) ->
    case Run < 8224 of
        true ->
            %% no new byte to write; just lengthen the run
            {[], State#eqoi_state{run = 1 + Run}};
        false ->
            %% max run size; write a run byte and reset the counter
            {encode_run(Run), State#eqoi_state{run=0}}
    end;
encode_pixel(Pixel, State=#eqoi_state{run=Run, seen=Seen}) ->
    %% not a match for previous byte
    case seen_update(Pixel, Seen) of
        {match, Hash} ->
            OutBin = <<0:2, Hash:6>>,
            NewSeen = Seen;
        {nomatch, NewSeen} ->
            case component_diffs(Pixel, State#eqoi_state.previous) of
                {R, G, B, A} when R >= -2, R =< 1,
                                  G >= -2, G =< 1,
                                  B >= -2, B =< 1,
                                  A == 0 ->
                    OutBin = <<2:2, R:2, G:2, B:2>>;
                {R, G, B, A} when R >= -16, R =< 15,
                                  G >= -8, G =< 7,
                                  B >= -8, B =< 7,
                                  A == 0 ->
                    OutBin = <<6:3, R:5, G:4, B:4>>;
                {R, G, B, A} when R >= -16, R =< 15,
                                  G >= -16, G =< 15,
                                  B >= -16, B =< 15,
                                  A >= -16, A =< 15 ->
                    OutBin = <<14:4, R:5, G:5, B:5, A:5>>;
                {R, G, B, A} ->
                    <<Pr, Pg, Pb, Pa>> = full_pixel(Pixel),
                    OutBin = [<<15:4,
                                (case R of 0 -> 0; _ -> 1 end):1,
                                (case G of 0 -> 0; _ -> 1 end):1,
                                (case B of 0 -> 0; _ -> 1 end):1,
                                (case A of 0 -> 0; _ -> 1 end):1>>,
                              case R of 0 -> Pr; _ -> <<>> end,
                              case B of 0 -> Pb; _ -> <<>> end,
                              case G of 0 -> Pg; _ -> <<>> end,
                              case A of 0 -> Pa; _ -> <<>> end]
            end
    end,
    {maybe_add_run(Run, OutBin),
     State#eqoi_state{previous=Pixel, run=0, seen=NewSeen}}.

encode_run(Length) when Length =< 32 ->
    <<2:3, (Length-1):5>>;
encode_run(Length) ->
    <<3:3, (Length-33):13>>.

maybe_add_run(0, IoData) ->
    IoData;
maybe_add_run(Length, IoData) ->
    [encode_run(Length), IoData].


component_diffs(<<R, B, G, A>>, <<Pr, Pb, Pg, Pa>>) ->
    {wrap_diff(R, Pr), wrap_diff(B, Pb), wrap_diff(G, Pg), wrap_diff(A, Pa)};
component_diffs(<<R, B, G>>, <<Pr, Pb, Pg>>) ->
    {wrap_diff(R, Pr), wrap_diff(B, Pb), wrap_diff(G, Pg), 0}.

wrap_diff(X, Y) ->
    case X - Y of
        D when D > 15 ->
            X - (255 + Y);
        D when D < -16 ->
            (255 + X) - Y;
        D ->
            D
    end.

wrap_sum(X, Y) ->
    case X + Y of
        D when D > 255 ->
            D rem 256;
        D when D < 0 ->
            D + 256;
        D ->
            D
    end.

decode(Data) ->
    decode_loop(Data, state_initial(pixel_initial_rgba()), []).

decode_loop(<<>>, _, Acc) ->
    iolist_to_binary(lists:reverse(Acc));
decode_loop(Data, State, Acc) ->
    {Pixels, NewData, NewState} = decode_next_chunk(Data, State),
    decode_loop(NewData, NewState, [Pixels|Acc]).

decode_next_chunk(<<0:2, Index:6, Rest/binary>>,
                  State=#eqoi_state{seen=Seen}) ->
    %% indexed
    case seen_find(Index, Seen) of
        Pixel when is_binary(Pixel) ->
            {[Pixel], Rest, State#eqoi_state{previous=Pixel}};
        _ ->
            {error, {bad_pixel_index, Index}}
    end;
decode_next_chunk(<<2:3, Length:5, Rest/binary>>,
                  State=#eqoi_state{previous=Pixel}) ->
    %% short run
    {lists:duplicate(Length + 1, Pixel), Rest, State};
decode_next_chunk(<<3:3, Length:13, Rest/binary>>,
                  State=#eqoi_state{previous=Pixel}) ->
    %% long run
    {lists:duplicate(Length + 1, Pixel), Rest, State};
decode_next_chunk(<<2:2, Rd:2/signed, Bd:2/signed, Gd:2/signed,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, seen=Seen}) ->
    NewPixel = mod_pixel(Pixel, Rd, Bd, Gd, 0),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        seen=seen_add(NewPixel, Seen)}};
decode_next_chunk(<<6:3, Rd:5/signed, Bd:4/signed, Gd:4/signed,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, seen=Seen}) ->
    NewPixel = mod_pixel(Pixel, Rd, Bd, Gd, 0),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        seen=seen_add(NewPixel, Seen)}};
decode_next_chunk(<<14:4, Rd:5/signed, Bd:5/signed, Gd:5/signed, Ad:5/signed,
                    Rest/binary>>,
                  State=#eqoi_state{previous=Pixel, seen=Seen}) ->
    NewPixel = mod_pixel(Pixel, Rd, Bd, Gd, Ad),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        seen=seen_add(NewPixel, Seen)}};
decode_next_chunk(<<15:4, Rp:1, Bp:1, Gp:1, Ap:1, ModRest/binary>>,
                  State=#eqoi_state{previous=Pixel, seen=Seen}) ->
    {NewPixel, Rest} = mod_pixel(Pixel, {Rp, Bp, Gp, Ap}, ModRest),
    {[NewPixel], Rest, State#eqoi_state{previous=NewPixel,
                                        seen=seen_add(NewPixel, Seen)}}.

-spec mod_pixel(pixel(), integer(), integer(), integer(), integer()) ->
          pixel().
mod_pixel(<<Ro:8, Bo:8, Go:8, Ao:8>>, Rd, Bd, Gd, Ad) ->
    <<(wrap_sum(Rd, Ro)):8,
      (wrap_sum(Bd, Bo)):8,
      (wrap_sum(Gd, Go)):8,
      (wrap_sum(Ad, Ao)):8>>.

-spec mod_pixel(pixel(), {integer()}, binary()) -> {pixel(), binary()}.
mod_pixel(<<Ro:8, Bo:8, Go:8, Ao:8>>, {Rm, Bm, Gm, Am}, Data) ->
    {R, Rrest} = maybe_mod(Rm, Ro, Data),
    {B, Brest} = maybe_mod(Bm, Bo, Rrest),
    {G, Grest} = maybe_mod(Gm, Go, Brest),
    {A, Arest} = maybe_mod(Am, Ao, Grest),
    {<<R:8, B:8, G:8, A:8>>, Arest}.

-spec maybe_mod(0|1, integer(), binary()) -> {integer(), binary()}.
maybe_mod(0, Original, Data) ->
    {Original, Data};
maybe_mod(1, _, <<New:8, Rest/binary>>) ->
    {New, Rest}.
