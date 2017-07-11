%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 11:29 AM
%%%-------------------------------------------------------------------
-module(websocket_decoder).
-author("regupathy.b").
-include("websocket.hrl").

%% API
-export([scan/2]).
%%====================================================================
%% Web Socket streaming binary data parser
%%====================================================================

-spec scan(Source::binary() | [],State::term()) -> {incomplete,State::term()} | {complete,DataSet::term()} | {wrong_data,Reason::any()}.

scan(Source,[])when is_binary(Source) -> decrypt(Source);
scan(Source,{payload,Bin,Length,State,Acc}) -> payload(<<Bin/binary,Source/binary>>,Length,State,Acc);
scan(Source,{mask_payload,Bin,MaskKeys,Length,State,Acc}) -> mask_payload(<<Bin/binary,Source/binary>>,MaskKeys,Length,State,Acc);
scan(_,_) -> {wrong_data,<<"Invalid State">>}.

decrypt(<<F:1,R1:1,R2:1,R3:1,Opcode:4,Mask:1,PLength:7,Rest/binary>>) ->
  opcode(Opcode,Rest,#ws{fin=F,rsv1 = R1,rsv2 = R2,rsv3 = R3,opcode = Opcode,mask = Mask,length = PLength}).

opcode(0,Rest,#ws{length = PLength} = WSState) -> payload_length(Rest,PLength,WSState);
opcode(1,Rest,#ws{length = PLength} = WSState) -> payload_length(Rest,PLength,WSState);
opcode(_,_Rest,WSState) -> {complete,WSState}.

payload_length(<<Length:64,Bin/binary>>,127,State) -> mask(Bin,State#ws{length = Length});
payload_length(<<Length:16,Bin/binary>>,126,State) -> mask(Bin,State#ws{length = Length});
payload_length(Bin,Len,State)when Len < 126 -> mask(Bin,State).

mask(Bin,#ws{mask = 0 } = State) -> payload(Bin,State);
mask(<<M1:8,M2:8,M3:8,M4:8,RestBin/binary>>,#ws{mask=1}=State)->  mask_payload(RestBin,State#ws{mask_list = [M1,M2,M3,M4]}).

payload(Bin,#ws{length = Length} = State) -> payload(Bin,Length,State,<<>>).
payload(<<>>,0,State,Acc) -> {complete,State#ws{data = Acc}};
payload(<<H,Rest/binary>>,Length,State,Acc) -> payload(Rest,Length-1,State,<<Acc/binary,H/binary>>);
payload(Bin,Length,State,Acc) -> {incomplete,{payload,Bin,Length,State,Acc}}.

mask_payload(Bin,#ws{mask_list = L,length = Length} = State) -> mask_payload(Bin,L,Length,State,<<>>).
mask_payload(Bin,[H|[]],Len,#ws{mask_list = List} = State,Acc) -> mask_payload(Bin,[H|List],Len,State,Acc);
mask_payload(_,_,0,State,Acc) -> {complete,State#ws{data = Acc}};
mask_payload(<<>>,_,0,State,Acc) -> {complete,State#ws{data = Acc}};
mask_payload(<<H,Rest/binary>>,[MaskKey|RestMaskKey],Length,State,Acc) ->  mask_payload(Rest,RestMaskKey,Length-1,State,exor(H,MaskKey,Acc));
mask_payload(Bin,RestMaskKey,Length,State,Acc) -> {incomplete,{mask_payload,Bin,RestMaskKey,Length,State,Acc}}.

exor(Data,MaskKey,Acc) -> Bin = crypto:exor(<<Data>>,<<MaskKey>>),<<Acc/binary,Bin/binary>>.
