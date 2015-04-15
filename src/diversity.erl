-module(diversity).

-export([render/3]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Render a diversity component with a given context
render(#{<<"component">> := Name, <<"settings">> := Settings0} = Params, Language, Context) ->
    LoadComponent = fun ({ComponentName, Version}) -> load_component(ComponentName, Version, Language) end,

    %% Retrive a list of all components in the settings tree
    io:format("Retriving components from settings...~n"),
    {ComponentList, Components0} = get_components(LoadComponent, Params),

    %% Retrive a list of all components dependencies
    io:format("Retriving dependencies of components...~n"),
    {DependencyList, Components1} = get_dependencies(LoadComponent, ComponentList, Components0),
    AllComponentsList = DependencyList ++ ComponentList,

    %% Render the sub-components
    io:format("Rendering sub-components...~n"),
    Settings1 = map(render_fun(Components1, Language, Context), Settings0),
    %io:format("Settings: ~p~n", [Settings1]),

    %% Render the top component
    #{<<"diversity">> := #{<<"version">> := Version},
      <<"template">> := Render} = maps:get(Name, Components1),
    MustacheContext0 = render_context(Name, Version, Language, Settings1, Context),

    io:format("Accumulating component data...~n"),
    {L10n, Scripts, Styles, Modules} = get_component_data(AllComponentsList, Components1),
    AngularBootstrap = iolist_to_binary([
        <<"angular.module(\"tws\",">>, jiffy:encode(Modules), <<")\nangular.bootstrap(document, [\"tws\"])">>
    ]),

    MustacheContext1 = MustacheContext0#{<<"l10n">> => L10n,
                                         <<"styles">> => Styles,
                                         <<"scripts">> => Scripts,
                                         <<"modules">> => Modules,
                                         <<"angularBootstrap">> => AngularBootstrap},

    io:format("Rendering component...~n"),
    Render(MustacheContext1).

get_component_data(AllComponentsList, Components) ->
    Acc = lists:foldl(
        fun (Name, {L10n0, Scripts0, Styles0, Modules0}) ->
            #{<<"baseUrl">> := BaseURL,
              <<"diversity">> := Diversity} = Component = maps:get(Name, Components),
            L10n1 = case maps:find(<<"translation">>, Component) of
                {ok, Translation} -> [#{<<"component">> => Name, <<"messages">> => Translation} | L10n0];
                error             -> L10n0
            end,

            Scripts1 = case maps:find(<<"script">>, Diversity) of
                {ok, Script} when is_binary(Script) ->
                    [[build_url(BaseURL, Script)] | Scripts0];
                {ok, Scripts} when is_list(Scripts) ->
                    [[build_url(BaseURL, Script) || Script <- Scripts] | Scripts0];
                error ->
                    Scripts0
            end,

            Styles1 = case maps:find(<<"style">>, Diversity) of
                {ok, Style} when is_binary(Style) ->
                    [[build_url(BaseURL, Style)] | Styles0];
                {ok, Styles} when is_list(Styles) ->
                    [[build_url(BaseURL, Style) || Style <- Styles] | Styles0];
                error ->
                    Styles0
            end,

            Modules1 = case maps:find(<<"angular">>, Diversity) of
                {ok, Module} -> [Module | Modules0];
                error        -> Modules0
            end,
            {L10n1, Scripts1, Styles1, Modules1}
        end,
        {[], [], [], []},
        AllComponentsList
    ),
    {I10n0, Scripts0, Styles0, Modules0} = Acc,
    Styles1 = lists:filter(
        fun (Style) -> re:run(Style, <<"^.*\\.scss$">>, [unicode]) == nomatch end,
        lists:concat(lists:reverse(Styles0))
    ),
    %% Special care for the loading of the modules
    Modules1 = [hd(Modules0) | lists:reverse(tl(Modules0))],
    {I10n0, lists:concat(Scripts0), Styles1, Modules1}.

build_url(_BaseURL, <<"//", _Rest/binary>> = URL) -> URL;
build_url(_BaseURL, <<"http://", _Rest/binary>> = URL) -> URL;
build_url(_BaseURL, <<"https://", _Rest/binary>> = URL) -> URL;
build_url(BaseURL, Path) -> <<BaseURL/binary, "files/", Path/binary>>.

get_components(LoadComponent, Params) ->
    {ComponentList0, ComponentConstraints} = fold(fun get_component/2, {[], #{}}, Params),

    %% Remove the main component from the list
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
    fun (#{<<"component">> := Name} = Component0) ->
        Settings = maps:get(<<"settings">>, Component0, #{}),

        %% Render HTML if a template exist
        Component1 = case maps:find(Name, Components) of
            {ok, #{<<"template">> := Template, <<"diversity">> := #{<<"version">> := Version}}} ->
                MustacheContext = render_context(Name, Version, Language, Settings, Context),
                ComponentHTML = mustache:render(Template, MustacheContext),
                io:format("Rendered ~p~nHTML:~n~p~nContext: ~p~n~n", [Name, ComponentHTML, MustacheContext]),
                Component0#{<<"componentHTML">> => ComponentHTML};
            error ->
                Component0#{<<"componentHTML">> => <<>>}
        end,
        maps:remove(<<"settings">>, Component1)
    end.

render_context(Name, Version, Language, Settings, Context) ->
    {ok, DiversityURL} = application:get_env(diversity, diversity_api_url),
    VersionBin = diversity_semver:semver_to_binary(Version),
    BaseURL = <<DiversityURL/binary, "components/", Name/binary, "/", VersionBin/binary, "/files/">>,
    SettingsJSON = re:replace(jiffy:encode(Settings), <<"</script>">>, <<"<\\\\/script>">>, [global, unicode, {return, binary}]),
    #{<<"baseUrl">> => BaseURL,
      <<"context">> => Context,
      <<"lang">> => language_fun(Language),
      <<"settings">> => Settings,
      <<"settingsJSON">> => SettingsJSON}.

language_fun(Language) ->
    fun (Text) -> re:replace(Text, <<"lang">>, Language, [global, unicode, {return, binary}]) end.

load_component(Component, Version, Language) ->
    %% Build the components base URL
    {ok, DiversityURL} = application:get_env(diversity, diversity_api_url),
    VersionBin = diversity_semver:semver_to_binary(Version),
    BaseURL = <<DiversityURL/binary, "components/", Component/binary, $/, VersionBin/binary, $/>>,

    %% Retrive the diversity.json
    Diversity = diversity_api_client:get_diversity_json(Component, Version),
    Loaded0 = #{<<"diversity">> => Diversity#{<<"version">> => Version},
                <<"baseUrl">> => BaseURL},

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

map(Fun, #{<<"component">> := _} = Component) ->
    Settings = maps:get(<<"settings">>, Component, #{}),
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
