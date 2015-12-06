%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2015, http://arboreus.system
%%% @doc Service module for library code handling
%%%
%%% @end
%%% Created : 20. Nov 2015 18:08
%%%-------------------------------------------------------------------
-module(a_service).
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

%% API
-export([
	rebuild/2,
	find_files/5,
	reload/1
]).

%% Module includes
-include("a.hrl").

%%-----------------------------------
%% @doc

reload(Path_to_conf) ->
	case file:consult(Path_to_conf) of
		{ok,Config} ->
			Path_root = proplists:get_value(path_root,Config),
			Path_bin = proplists:get_value(path_bin,Config),
			Path_to_bin = lists:concat([Path_root,Path_bin]),
			Files = find_files(Path_to_bin,[],[],[],".beam"),
			Files;
		_ -> a:error(?FUNCTION_NAME(),a017)
	end.


%%-----------------------------------
%% @doc Rebuild application through the make file
-spec rebuild(Type,Parameters) -> ok | {error,_Reason}
	when
		Type :: application,
		Parameters :: Path_to_conf | {Module_name,Path_to_conf},
		Path_to_conf :: unicode:chardata(),
		Module_name :: atom().

rebuild(application,Path_to_conf) ->
	case file:consult(Path_to_conf) of
		{ok,Config} ->
			Path_root = proplists:get_value(path_root,Config),
			Path_src = proplists:get_value(path_src,Config),
			Path_bin = proplists:get_value(path_bin,Config),
			Rebuild_log_file = lists:concat([Path_root,"/rebuild.make.log"]),
			Error_log_file = lists:concat([Path_root,"/rebuild.error.log"]),
			Time = fun() -> binary_to_list(a_time:current(ansi)) end,
			file:write_file(
				Rebuild_log_file,
				"***\nRebuild started at: "++Time()++"\n***\n"
			),
			Files = find_files(lists:concat([Path_root,Path_src]),[],[],[],".erl"),
			lists:foreach(fun(Row) ->
				{Module_name,[Source_file,Source_path]} = Row,
				Source_path_length = fun() -> length(Path_src) + length(Path_root) end,
				Relative_path = lists:nthtail(Source_path_length(),Source_path),
				Full_bin_path = lists:concat([Path_root,Path_bin,Relative_path]),
				File_path = lists:concat([Source_path,"/",Source_file]),
				Write_ok = fun() ->
					file:write_file(
						Rebuild_log_file,
						lists:concat([
							"Module: ",atom_to_list(Module_name),
							" File: ",File_path,
							" compiled to: ",Full_bin_path,
							"\n"
						]),
						[append])
				end,
				Write_error = fun(Errors_in,Reasons_in) ->
					file:write_file(
						Error_log_file,
						lists:concat([
							"Module: ",atom_to_list(Module_name),
							" File: ",File_path,"\n"
							"Compilation ERROR\n",
							"Errors:\n",Errors_in,"\n",
							"Reasons:\n",Reasons_in,"\n",
							"***\n"
						]),
						[append])
				end,
				filelib:ensure_dir(Full_bin_path),
				case compile:file(File_path,[{outdir,Full_bin_path}]) of
					{ok,_} -> Write_ok();
					{ok,_,_} -> Write_ok();
					{ok,_,_,_} -> Write_ok();
					{error,Errors,Warnings} -> Write_error(Errors,Warnings);
					_ -> Write_error("undefined error","undefined warnings")
				end
			end,Files),
			file:write_file(
				Rebuild_log_file,
				"***\nRebuild finished at: "++Time()++"\n***\n",
				[append]
			);
		_ -> a:error(?FUNCTION_NAME(),a017)
	end.

%%-----------------------------------
%% @doc Return a list of Erlang files from the source directory
-spec find_files(Path_src,List_of_dir,Dir_list,Files,Extension) -> list()
	when
		Path_src :: unicode:chardata(),
		List_of_dir :: list(),
		Dir_list :: list(),
		Files :: list(),
		Extension :: unicode:chardata().

find_files(Path_src,[],[],[],Extension) ->
	{ok,List_of_src} = file:list_dir(Path_src),
	find_files(Path_src,List_of_src,[],[],Extension);
find_files(_,[],[],Files,_) ->
	Files;
find_files(_,[],[Dir|Dirs],Files,Extension) ->
	{ok,List_of_dir} = file:list_dir(Dir),
	find_files(Dir,List_of_dir,Dirs,Files,Extension);
find_files(Path_src,[Element|Dir_list],Directories,Files,Extension) ->
	Dir = lists:concat([Path_src,"/",Element]),
	Is_dir = filelib:is_dir(Dir),
	if
		Is_dir == true ->
			Directories_out = lists:append(Directories,[Dir]),
			find_files(Path_src,Dir_list,Directories_out,Files,Extension);
		Is_dir == false ->
			Size = length(Element),
			Extension_file = lists:nthtail(Size-4,Element),
			if
				Extension_file == Extension ->
					Module_name = list_to_atom(lists:sublist(Element,1,Size-4)),
					Files_out = lists:append(Files,[{Module_name,[Element,Path_src]}]),
					find_files(Path_src,Dir_list,Directories,Files_out,Extension);
				true ->
					find_files(Path_src,Dir_list,Directories,Files,Extension)
			end
	end.