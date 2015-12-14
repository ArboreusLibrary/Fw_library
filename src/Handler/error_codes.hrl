%% Error codes for a:error function
-define(ERROR_CODES,[

	%% Bad arguments
	{a000,"Bad arguments"},
	{a001,"Bad arguments, must be binary()"},
	{a002,"Bad arguments, must be bitstring()"},
	{a003,"Bad arguments, must be integer()"},
	{a004,"Bad arguments, must be list()"},
	{a005,"Bad arguments, must be tuple()"},
	{a006,"Bad arguments, must be atom()"},
	{a007,"Bad arguments, must be a binary() from defined list"},
	{a008,"Bad arguments, must be a bitstring() from defined list"},
	{a009,"Bad arguments, must be an integer() from defined range"},
	{a010,"Bad arguments, must be a string() from defined list"},
	{a011,"Bad arguments, must be a tuple() from defined list"},
	{a012,"Bad arguments, must be an atom() from defined list"},
	{a013,"Bad arguments, wrong type of arguments"},
	{a014,"Bad arguments, must be string()"},
	{a015,"Bad arguments, must be typified list()"},
	{a016,"Throw exeption"},
	{a017,"Wrong path"},

	%% Error handler
	{e000,"Error handler: bad arguments"},
	{e001,"Error handler: undefined error code requested"},

	%% Module: a_sequence
	{m001_001,"No requested type of dictionary"},
	{m001_002,"Dictionary generating error"},
	{m001_003,"Bad arguments, random number generation failed"},

	%% Module: a_header
	{m002_001,"Bad arguments, Value() must be binary() | integer() | string()"},

	%% Module: a_params
	{m003_001,"Bad parameters type, must be defined in a_params module"},

	%% Module: a_list
	{m004_001,"Wrong path"},
	{m004_002,"No key-value pair in list"}

]).