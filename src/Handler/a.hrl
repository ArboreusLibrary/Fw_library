%% Getting function name
-define(FUNCTION_NAME(),process_info(self(),current_function)).

-type result_of_function() :: any().
-type callback_module() :: function().

-type table_name() :: atom().
-type attribute_name() :: atom().
-type file_path_string() :: unicode:latin1_chardata().

-type list_of_keys() :: list().