%%% @doc Loads agent role definitions from priv/roles/.
%%% Role files are markdown with YAML front matter.
-module(load_agent_role).

-export([load/1]).

-spec load(binary()) -> {ok, binary()} | {error, term()}.
load(RoleName) when is_binary(RoleName) ->
    %% Supports both flat names ("visionary") and nested ("storm/domain_expert")
    FileName = <<RoleName/binary, ".md">>,
    PrivDir = code:priv_dir(orchestrate_agents),
    PathParts = binary:split(FileName, <<"/">>, [global]),
    Path = filename:join([PrivDir, "roles" | [binary_to_list(P) || P <- PathParts]]),
    case file:read_file(Path) of
        {ok, Content} -> {ok, Content};
        {error, enoent} -> {error, {role_not_found, RoleName}};
        {error, Reason} -> {error, Reason}
    end.
