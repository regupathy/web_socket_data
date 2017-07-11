%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2016 2:06 PM
%%%-------------------------------------------------------------------
-module(websocket_encoder).
-author("regupathy.b").
-include("websocket.hrl").
-export([build/1]).
%%%===================================================================
%%% APIs
%%%===================================================================

-spec build(DataSet::term()) -> {ok,Data::binary()} | {wrong_data,Reason::any()}.
build(#ws{} = DataSet) -> {ok,encrypt(DataSet)};
build(_) -> {wrong_data,<<"Unknown data for encode websocket">>}.

%%%===================================================================
%%% Websocket data parser methods
%%%===================================================================

encrypt(#ws{fin = FIN,rsv1 = RSV1,rsv2 = RSV2,rsv3 = RSV3,opcode = Opcode,mask = Mask,length = Len,exLength = ExLen,data = Data}) ->
  payload(Mask,Data,<<FIN:1,RSV1:1,RSV2:1,RSV3:1,Opcode:4,Mask:1,Len:7,ExLen/binary>>).

payload(1,<<>>,Bin) -> Bin;
payload(0,<<>>,Bin) -> Bin;
payload(1,Data,Bin) -> binary_util:merge(Bin,create_mask_payload(Data,create_maskKey_list(4,[])));
payload(0,Data,Bin) -> <<Bin/binary,Data/binary>>.

create_mask_payload(Bin,MaskList)-> create_mask_payload(Bin,MaskList,MaskList,<<>>).
create_mask_payload(Bin,[],MaskList,Acc) -> create_mask_payload(Bin,MaskList,MaskList,Acc);
create_mask_payload(<<>>,_,[M1,M2,M3,M4],Acc) -> <<<<M1>>:1/binary,<<M2>>:1/binary,<<M3>>:1/binary,<<M4>>:1/binary,Acc/binary>>;
create_mask_payload(<<H:8,Rest/binary>>,[Key|T],MaskList,Acc) -> create_mask_payload(Rest,T,MaskList,exor(H,Key,Acc)).

create_maskKey_list(0,Acc) -> Acc;
create_maskKey_list(N,Acc) -> create_maskKey_list(N-1,[random:uniform(125)|Acc]).

exor(Data,MaskKey,Acc) -> Bin = crypto:exor(<<Data>>,<<MaskKey>>),<<Acc/binary,Bin/binary>>.
