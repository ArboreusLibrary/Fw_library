%%%-------------------------------------------------------------------
%%% @author Alexandr KIRILOV
%%% @copyright (C) 2016, http://arboreus.system
%%% @doc
%%%
%%% @end
%%% Created : 12. Февр. 2016 17:36
%%%-------------------------------------------------------------------
-author("Alexandr KIRILOV, http://alexandr.kirilov.me").

-define(APPLICATION_NAME,"Arboreus Library").

-define(APPLICATION_HEADER_OK,{header,["Appplication:","ok"]}).
-define(APPLICATION_HEADER_ERROR,fun(X) -> {header,["Appplication:",X]} end).

%% a_output_dsv
-define(DSV_OUTPUT_FILENAME,"application.dsv").
-define(DSV_SEPARATOR,";;").

%% a_output_xml
-define(XML_OUTPUT_FILENAME,"application.xml").