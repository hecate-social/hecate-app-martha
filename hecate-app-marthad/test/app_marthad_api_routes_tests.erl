-module(app_marthad_api_routes_tests).
-include_lib("eunit/include/eunit.hrl").

discover_routes_returns_list_test() ->
    %% Without domain apps loaded, should return empty list
    Routes = app_marthad_api_routes:discover_routes(),
    ?assert(is_list(Routes)).
