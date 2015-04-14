-module(diversity).

-export([render/3]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Render a diversity component with a given context
render(#{<<"component">> := Component, <<"settings">> := Settings0} = Params, Language, Context) ->
    LoadComponent = fun ({Name, Version}) -> load_component(Name, Version, Language) end,

    %% Retrive a list of all components in the settings tree
    io:format("Retriving components from settings...~n"),
    {ComponentList, Components0} = get_components(LoadComponent, Params),

    %% Retrive a list of all components dependencies
    io:format("Retriving dependencies of components...~n"),
    {DependencyList, Components1} = get_dependencies(LoadComponent, ComponentList, Components0),
    _AllComponentsList = DependencyList ++ ComponentList,

    %% Render the sub-components
    io:format("Rendering sub-components...~n"),
    Settings1 = map(render_fun(Components1, Language, Context), Settings0),

    %% Render the top component
    #{<<"diversity">> := #{<<"version">> := Version},
      <<"template">> := Render} = maps:get(Component, Components1),
    MustacheContext = render_context(Component, Version, Language, Settings1, Context),
    Render(MustacheContext).

get_components(LoadComponent, Params) ->
    {ComponentList0, ComponentConstraints} = fold(fun get_component/2, {[], #{}}, Params),
    ComponentList1 = lists:reverse(ComponentList0),

    %% Resolve the correct version of the components
    io:format("Resolving components versions...~n"),
    ComponentVersions = resolve_versions(ComponentConstraints),

    %% Load all components concurrently
    Components = maps:from_list(pmap(LoadComponent, maps:to_list(ComponentVersions))),

    {ComponentList1, Components}.

get_component(#{<<"component">> := Name} = Component, Acc) ->
    Constraint = maps:get(<<"version">>, Component, <<"*">>),
    add_constraint(Name, Constraint, Acc).

get_dependencies(LoadComponent, ComponentList, Components) ->
    get_dependencies(LoadComponent, ComponentList, Components, []).

get_dependencies(_LoadComponent, [], Components, ListAcc) ->
    {ListAcc, Components};
get_dependencies(LoadComponent, ComponentList, Components0, ListAcc) ->
    {_, {DependencyList0, DependencyConstraints}} = lists:foldl(
                                                        fun get_dependency/2,
                                                        {Components0, {[], #{}}},
                                                        ComponentList
                                                    ),
    DependencyList1 = lists:reverse(DependencyList0),

    %% Resolve the correct version of the dependencies
    DependencyVersions = resolve_versions(DependencyConstraints),

    %% Load all dependencies concurrently
    Dependencies = maps:from_list(pmap(LoadComponent, maps:to_list(DependencyVersions))),

    Components1 = maps:merge(Dependencies, Components0),
    get_dependencies(LoadComponent, DependencyList1, Components1, DependencyList1 ++ ListAcc).

get_dependency(Name, {Components, Acc0}) ->
    #{<<"diversity">> := Diversity} = maps:get(Name, Components),
    Dependencies = maps:get(<<"dependencies">>, Diversity, #{}),
    {Components,
     maps:fold(
         fun (Dependency, Constraint, Acc) ->
             case maps:find(Dependency, Components) of
                 %% Check if component is already resolved
                 %% If it is, check that constraint does not clash
                 {ok, #{<<"diversity">> := #{<<"version">> := DependencyVersion}}} ->
                     case diversity_semver:check_constraint(DependencyVersion, Constraint) of
                         true  ->
                             ok;
                         false ->
                             Version = maps:get(<<"version">>, Diversity),
                             lager:warning(
                               "Incompatible versions for component.~n"
                               "Component: ~p ~p~n"
                               "Dependency: ~p ~p~n"
                               "Constraint: ~p~n",
                               [Name, Version, Dependency, DependencyVersion, Constraint]
                              )
                     end,
                     Acc;
                 %% Otherwise it is a new dependency
                 error ->
                     add_constraint(Dependency, Constraint, Acc)
             end
         end,
         Acc0,
         Dependencies
      )}.

add_constraint(Name, Constraint, {ComponentList0, Components0}) ->
    case maps:find(Name, Components0) of
        {ok, Constraints} ->
            %% Add the version to the components constraints
            Components1 = maps:put(Name, [Constraint | Constraints], Components0),
            {ComponentList0, Components1};
        error ->
            Constraints = [Constraint],
            ComponentList1 = [Name | ComponentList0],
            Components1 = maps:put(Name, Constraints, Components0),
            {ComponentList1, Components1}
    end.

render_fun(Components, Language, Context) ->
    fun (#{<<"component">> := Name} = Component) ->
            Settings = maps:get(<<"settings">>, Component, #{}),

            %% Render HTML if a template exist
            case maps:find(Name, Components) of
                {ok, #{<<"template">> := Template, <<"diversity">> := #{<<"version">> := Version}}} ->
                    MustacheContext = render_context(Name, Version, Language, Settings, Context),
                    Rendered = mustache:render(Template, MustacheContext),
                    Component#{<<"componentHTML">> => iolist_to_binary(Rendered)};
                error ->
                    Component
            end
    end.

render_context(Name, Version, Language, Settings, Context) ->
    {ok, DiversityURL} = application:get_env(diversity, diversity_api_url),
    VersionBin = diversity_semver:semver_to_binary(Version),
    BaseURL = <<DiversityURL/binary, "/components/", Name/binary, "/", VersionBin/binary, "/files/">>,
    #{<<"baseURL">> => BaseURL,
      <<"context">> => Context,
      <<"lang">> => language_fun(Language),
      <<"settings">> => Settings,
      <<"settingsJSON">> => jiffy:encode(Settings)}.

language_fun(Language) ->
    fun (Text) -> re:replace(Text, <<"lang">>, Language, [global]) end.

load_component(Component, Version, Language) ->
    %% Retrive the diversity.json
    Diversity = diversity_api_client:get_diversity_json(Component, Version),
    Loaded0 = #{<<"diversity">> => Diversity#{<<"version">> => Version}},

    %% Retrive the template if it exist
    Loaded1 = case maps:find(<<"template">>, Diversity) of
                  {ok, TemplatePath} ->
                      case diversity_api_client:get_file(Component, Version, TemplatePath) of
                          {ok, Template} -> Loaded0#{<<"template">> => mustache:compile(Template)};
                          undefined -> Loaded0
                      end;
                  error ->
                      Loaded0
              end,

    %% Retrive the translations if they exist
    Loaded2 = case maps:find(Language, maps:get(<<"i18n">>, Diversity, #{})) of
                  {ok, #{<<"view">> := TranslationPath}} ->
                      case diversity_api_client:get_file(Component, Version, TranslationPath) of
                          {ok, Translation} -> Loaded1#{<<"translation">> => Translation};
                          undefined -> Loaded1
                      end;
                  _NotFound -> Loaded1

              end,

    {Component, Loaded2}.

resolve_versions(ComponentConstraints) ->
    maps:map(
        fun (Name, Constraints) ->
            case diversity_semver:resolve_version(Name, Constraints) of
                undefined ->
                    lager:warning(
                        "Could not resolve version for component ~p~nConstraints: ~p~n",
                        [Name, Constraints]
                    ),
                    '*';
                Version ->
                    Version
            end
        end,
        ComponentConstraints
    ).

map(Fun, #{<<"component">> := _, <<"settings">> := Settings} = Component) ->
    Fun(Component#{<<"settings">> => map(Fun, Settings)});
map(Fun, Map) when is_map(Map) ->
    maps:map(fun (_Property, Value) -> map(Fun, Value) end, Map);
map(Fun, List) when is_list(List) ->
    lists:map(fun (Item) -> map(Fun, Item) end, List);
map(_Fun, Term) ->
    Term.

fold(Fun, Acc0, #{<<"component">> := _} = Component) ->
    Settings = maps:get(<<"settings">>, Component, #{}),
    Acc1 = fold(Fun, Acc0, Settings),
    Fun(Component, Acc1);
fold(Fun, Acc0, Map) when is_map(Map) ->
    maps:fold(fun (_Property, Value, Acc) -> fold(Fun, Acc, Value) end, Acc0, Map);
fold(Fun, Acc0, List) when is_list(List) ->
    lists:foldl(fun (Item, Acc) -> fold(Fun, Acc, Item) end, Acc0, List);
fold(_Fun, Acc, _Term) ->
    Acc.

pmap(Fun, List) ->
    pmap(Fun, List, 5000).

pmap(Fun, List, Timeout) ->
    Parent = self(),

    %% Spawn processes
    MapFun = fun (Item) -> spawn_monitor(fun () -> Parent ! {self(), Fun(Item)} end) end,
    Processes = lists:map(MapFun, List),

    %% Await results
    ReceiveFun = fun ({Pid, Reference}) ->
                     receive
                         {Pid, Result} ->
                             Result;
                         {'DOWN', Reference, process, Pid, Reason} ->
                             error({pmap, Pid, Reason})
                     after Timeout ->
                               error({pmap, Pid, timeout})
                     end
                 end,
    lists:map(ReceiveFun, Processes).
