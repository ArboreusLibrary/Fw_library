%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 12. Февр. 2016 21:22
%%%-------------------------------------------------------------------
-module(a_proplists).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% Module Include Start
-include("../Handler/a.hrl").

%% API
-export([
	from_record/2
]).

%% ----------------------------
%% @doc Return proplist from record by record information
-spec from_record(Data_module,Record_source) -> proplists:proplist() | {error,_Reason}
	when
		Data_module :: atom(),
		Record_source :: term().

from_record(Data_module,Record_src) when is_tuple(Record_src) ->
	[Record_name|Record] = tuple_to_list(Record_src),
	case apply(Data_module,rec_info,[Record_name]) of
		{error,Reason} -> {error,Reason};
		Record_info -> lists:zip(Record_info,Record)
	end;
from_record(_,_) -> a:error(?FUNCTION_NAME(),a014).