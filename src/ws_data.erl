%%%-------------------------------------------------------------------
%%% @author regupathy.b
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. Apr 2016 12:07 PM
%%%-------------------------------------------------------------------
-module(ws_data).
-author("regupathy.b").
-include("websocket.hrl").
%% API
-export([new/0,add_close_tag/1,add_data/2,get_data/1,is_connection/1,add_mask/1]).

%%%===================================================================
%%% creation of websocket data methods
%%%===================================================================

new() -> #ws{fin=1,opcode=1,mask =0 ,rsv1=0,rsv2=0,rsv3=0,length=0,exLength = <<>>,data = <<"">>}.

add_mask(WS) -> WS#ws{mask = 1}.

add_close_tag(WS) -> WS#ws{opcode = 8}.

add_data(WS,Data) -> add_length(WS#ws{data = Data}).

add_length(#ws{data = Data} = WS) -> add_length(WS,bit_size(Data) div 8).
add_length(#ws{} = WS,Length)when Length =< 125 -> WS#ws{length = Length,exLength = <<>>};
add_length(#ws{} = WS,Length)when Length < 65536 -> WS#ws{length = 126,exLength = <<Length:16>>};
add_length(#ws{} = WS,Length) -> WS#ws{length = Length,exLength = <<Length:64>>}.

%%%===================================================================
%%% Extract Websocket data methods
%%%===================================================================

get_data(#ws{data = Data}) -> Data.

is_connection(#ws{opcode = 1} =_) -> active;
is_connection(#ws{opcode = 8} =_) -> close.


