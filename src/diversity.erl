-module(diversity).

-export([render/4]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Render a diversity component with a given context
%% This is the top-level entry point for rendering a diversity template.
%%
%% To load the right versions of components the following is done:
%% - Go through the parameter map to find the components constraints are specified in it.
%% - The versions for those components are resolved.
%% - The same procedure is done for those components dependencies etc...
%% - If colliding constraints is found then it is logged and the last version is used.
%%
%% After all components are loaded we start to render the parameter object:
%% - For each component in the parameter map (bottom-first) render the mustache template into
%%   the key <<"componentHTML">> if a template exists with the correct mustache context.
%% - After the parameter map has been rendered then we render the top-component using the
%%   accumulated data (translations, scripts, styles etc.).
render(#{<<"component">> := Name, <<"settings">> := Settings0} = Parameters, Language, Context,
       DiversityURL) ->
    LoadComponent = fun ({ComponentName, Version}) ->
                            load_component(ComponentName, Version, Language, DiversityURL)
                    end,
    %% Retrive a list of all components in the parameters
    {ComponentList, Components0} = get_components(LoadComponent, Parameters, DiversityURL),

    %% Retrive a list of all components dependencies
    {DependencyList, Components1} = get_dependencies(LoadComponent, ComponentList, Components0,
                                                     DiversityURL),

    %% Concatenate the components from the parameters and the dependencies
    AllComponentsList = DependencyList ++ ComponentList,

    %% Render the sub-components
    Settings1 = map(render_fun(Components1, Language, Context, DiversityURL), Settings0),

    %% Render the top component specifically
    #{<<"diversity">> := #{<<"version">> := Version} = Diversity} = maps:get(Name, Components1),
    MustacheContext0 = render_context(Name, Version, Language, Settings1, Context, DiversityURL),

    %% Retrive the data from all components that is needed to render the top-component
    {L10n, Scripts, Styles, Modules} = get_components_data(AllComponentsList, Components1),
    AngularBootstrap = iolist_to_binary([
                                         <<"angular.module(\"tws\",">>,
                                         jiffy:encode(Modules),
                                         <<")\nangular.bootstrap(document, [\"tws\"])">>
                                        ]),

    %% Build the top-level mustache context
    MustacheContext1 = MustacheContext0#{<<"l10n">>             => L10n,
                                         <<"styles">>           => Styles,
                                         <<"scripts">>          => Scripts,
                                         <<"modules">>          => Modules,
                                         <<"angularBootstrap">> => AngularBootstrap},

    %% Do the actual rendering
    imperial:render(get_template_fun(Name, Version, Diversity, DiversityURL), MustacheContext1).

%% @doc Accumulate all the data from the components into their specific parts (like all scripts by
%% themselves and css by themselves etc).
get_components_data(AllComponentsList, Components) ->
    %% Accumulate all data
    {_Components, {I10n0, Scripts0, Styles0, Modules0}} = lists:foldl(
                                                            fun get_component_data/2,
                                                            {Components, {[], [], [], []}},
                                                            AllComponentsList
                                                           ),

    %% Filter out scss-files
    Styles1 = lists:filter(
                fun (Style) -> re:run(Style, <<"^.*\\.scss$">>, [unicode]) == nomatch end,
                lists:concat(lists:reverse(Styles0))
               ),

    %% Special care for the loading of the modules
    Modules1 = [hd(Modules0) | lists:reverse(tl(Modules0))],

    {I10n0, lists:concat(Scripts0), Styles1, Modules1}.

%% @doc Get the needed data for a specfic component
get_component_data(Name, {Components, {L10n0, Scripts0, Styles0, Modules0}}) ->
    %% Get the diversity.json and the components baseUrl
    #{<<"baseUrl">>   := BaseURL,
      <<"diversity">> := Diversity} = Component = maps:get(Name, Components),
    %% Check if the component has any translations
    L10n1 = case maps:find(<<"translation">>, Component) of
                {ok, Translation} ->
                    [#{<<"component">> => Name,
                       <<"messages">>  => Translation}
                     | L10n0];
                error ->
                    L10n0
            end,

    %% Check if the component has any javascripts
    Scripts1 = case maps:find(<<"script">>, Diversity) of
                   {ok, Script} when is_binary(Script) ->
                       [[build_url(BaseURL, Script)] | Scripts0];
                   {ok, Scripts} when is_list(Scripts) ->
                       [[build_url(BaseURL, Script) || Script <- Scripts] | Scripts0];
                   error ->
                       Scripts0
               end,

    %% Check if the component has any (s)css files
    Styles1 = case maps:find(<<"style">>, Diversity) of
                  {ok, Style} when is_binary(Style) ->
                      [[build_url(BaseURL, Style)] | Styles0];
                  {ok, Styles} when is_list(Styles) ->
                      [[build_url(BaseURL, Style) || Style <- Styles] | Styles0];
                  error ->
                      Styles0
              end,

    %% Check if the component has any angular modules
    Modules1 = case maps:find(<<"angular">>, Diversity) of
                   {ok, Module} -> [Module | Modules0];
                   error        -> Modules0
               end,

    {Components, {L10n1, Scripts1, Styles1, Modules1}}.

%% @doc Build the URL for a file IF it is "local" i.e. a file in a diversity components repository.
%% If the path given is for a remote address then leave it be as is.
build_url(_BaseURL, <<"//", _/binary>> = URL) -> URL;
build_url(_BaseURL, <<"http://", _/binary>> = URL) -> URL;
build_url(_BaseURL, <<"https://", _/binary>> = URL) -> URL;
build_url(BaseURL, Path) -> <<BaseURL/binary, (filename:join(<<"/files">>, Path))/binary>>.

%% @doc Get components which are referenced in the given parameters map and resolve their versions.
get_components(LoadComponent, Parameters, DiversityURL) ->
    %% Find all components and their constraints (bottom-first)
    {ComponentAcc, ComponentConstraints} = fold(fun add_constraint/2, {[], #{}}, Parameters),

    %% Reverse the accumulator
    ComponentList = lists:reverse(ComponentAcc),

    %% Resolve the correct version of the components
    ComponentVersions = resolve_versions(ComponentConstraints, DiversityURL),

    %% Load all components concurrently
    Components = maps:from_list(pmap(LoadComponent, maps:to_list(ComponentVersions))),

    {ComponentList, Components}.

%% @doc Accumulate all components constraints. See add_constraint/3.
add_constraint(#{<<"component">> := Name} = Component, Acc) ->
    %% Default the constraint to any version
    Constraint = maps:get(<<"version">>, Component, <<"*">>),
    add_constraint(Name, Constraint, Acc).

%% @doc Add version constraints to a component and accumulate a list of the loading order of the
%% components.
add_constraint(Name, Constraint, {ComponentList0, Components0}) ->
    case maps:find(Name, Components0) of
        %% The component has been added before (thus no need to add it to the list)
        {ok, Constraints} ->
            Components1 = maps:put(Name, [Constraint | Constraints], Components0),
            {ComponentList0, Components1};
        %% The component has never been added before so create an entry in the constraints map
        %% and add it to the load order list.
        error ->
            Constraints = [Constraint],
            ComponentList1 = [Name | ComponentList0],
            Components1 = maps:put(Name, Constraints, Components0),
            {ComponentList1, Components1}
    end.

%% @doc Retrive all dependencies for the given components (recursively)
get_dependencies(LoadComponent, ComponentList, Components, DiversityURL) ->
    get_dependencies(LoadComponent, ComponentList, Components, DiversityURL, []).

%% @doc Recursively retrive all dependencies
get_dependencies(_LoadComponent, [], Components, _DiversityURL, Acc) ->
    %% No more dependencies to get
    {Acc, Components};
get_dependencies(LoadComponent, ComponentList, Components0, DiversityURL, Acc) ->
    %% Retrive all the dependencies constraints and the order they should be loaded
    {_, {DependencyAcc, DependencyConstraints}} = lists:foldl(
                                                    fun get_dependency/2,
                                                    {Components0, {[], #{}}},
                                                    ComponentList
                                                   ),
    %% Reverse the accumulator
    DependencyList = lists:reverse(DependencyAcc),

    %% Resolve the correct version of the dependencies
    DependencyVersions = resolve_versions(DependencyConstraints, DiversityURL),

    %% Load all dependencies concurrently
    Dependencies = maps:from_list(pmap(LoadComponent, maps:to_list(DependencyVersions))),

    %% Combine results so far
    {Components1, NewAcc} = {maps:merge(Dependencies, Components0), DependencyList ++ Acc},

    get_dependencies(LoadComponent, DependencyList, Components1, DiversityURL, NewAcc).

%% @doc For a given component accumulate it's dependencies version constraints. If the dependency
%% has already been resolved then check that the constraint is satisfied.
get_dependency(Name, {Components, Acc0}) ->
    #{<<"diversity">> := Diversity} = maps:get(Name, Components),
    Dependencies = maps:get(<<"dependencies">>, Diversity, #{}),
    {Components,
     maps:fold(
       fun (Dependency, Constraint, Acc) ->
               case maps:find(Dependency, Components) of
                   %% Check if component is already resolved earlier
                   {ok, #{<<"diversity">> := #{<<"version">> := Version}}} ->
                       case diversity_semver:check_constraint(Version, Constraint) of
                           %% The constraint is satisfied
                           true  ->
                               ok;
                           %% The constraint clashes, log it and move on
                           false ->
                               Version = maps:get(<<"version">>, Diversity),
                               lager:warning(
                                 "Incompatible versions for component.~n"
                                 "Component: ~p ~p~n"
                                 "Dependency: ~p ~p~n"
                                 "Constraint: ~p",
                                 [Name, Version, Dependency, Version, Constraint]
                                )
                       end,
                       Acc;
                   %% Otherwise it is a new dependency and we handle it just like the components
                   %% in the parameters map
                   error ->
                       add_constraint(Dependency, Constraint, Acc)
               end
       end,
       Acc0,
       Dependencies
      )}.

%% @doc Create a map-function that renders all sub-components in the parameters tree
render_fun(Components, Language, Context, DiversityURL) ->
    fun (#{<<"component">> := Name} = Component0) ->
            %% Default to an empty map as settings
            Settings = maps:get(<<"settings">>, Component0, #{}),

            %% Render HTML if a template exist
            Component1 = case maps:find(Name, Components) of
                             {ok, #{<<"template">> := Template,
                                    <<"diversity">> := #{<<"version">> := Version}}} ->

                                 %% Create a mustache context
                                 MustacheContext = render_context(Name, Version, Language, Settings,
                                                                  Context, DiversityURL),

                                 %% Render the sub-component
                                 ComponentHTML = imperial:render(Template, MustacheContext),
                                 Component0#{<<"componentHTML">> => ComponentHTML};
                             _ ->
                                 Component0
                         end,
            %% Remove the settings map since it's no longer needed
            maps:remove(<<"settings">>, Component1)
    end.

%% @doc Create a base mustache context using the given parameters
render_context(Name, Version, Language, Settings, Context, DiversityURL) ->
    Tag = diversity_semver:semver_to_binary(Version),
    BaseURL = <<
                DiversityURL/binary,
                (filename:join([<<"components">>, Name, Tag, <<"files">>]))/binary
              >>,

    %% Encode the settings as a JSON-blob (with escaped script tags)
    SettingsJSON = re:replace(
                     jiffy:encode(Settings),
                     <<"</script>">>,
                     <<"<\\\\/script>">>,
                     [global, unicode, {return, binary}]
                    ),

    %% The language substitution fun
    ReOptions = [global, unicode, {return, binary}],
    Lang = fun (Text) -> re:replace(Text, <<"lang">>, Language, ReOptions) end,

    %% The mustache context
    #{<<"baseUrl">>      => BaseURL,
      <<"context">>      => Context,
      <<"lang">>         => Lang,
      <<"settings">>     => Settings,
      <<"settingsJSON">> => SettingsJSON}.

%% @doc Load a components data into memory
%% Load all the data needed for the mustache rendering into memory from the remote diversity-api.
%% This will include:
%% - diversity.json
%% - mustache template (if it exists)
%% - translations (for given language, if it exists)
load_component(Component, Version, Language, DiversityURL) ->
    %% Build the components base URL
    Tag = diversity_semver:semver_to_binary(Version),
    BaseURL = <<DiversityURL/binary, (filename:join([<<"components">>, Component, Tag]))/binary>>,

    %% Retrive the diversity.json
    Diversity = diversity_api:get_diversity_json(Component, Version, DiversityURL),
    Loaded0 = #{<<"diversity">> => Diversity#{<<"version">> => Version},
                <<"baseUrl">> => BaseURL},

    %% Retrive the template if it exist
    Loaded1 = case get_template_fun(Component, Version, Diversity, DiversityURL) of
                  undefined   -> Loaded0;
                  TemplateFun -> Loaded0#{<<"template">> => TemplateFun}
              end,

    %% Retrive the translations if they exist
    Loaded2 = case maps:find(Language, maps:get(<<"i18n">>, Diversity, #{})) of
                  {ok, #{<<"view">> := Path}} ->
                      case diversity_api:get_file(Component, Version, Path, DiversityURL) of
                          {ok, Translation} -> Loaded1#{<<"translation">> => Translation};
                          undefined -> Loaded1
                      end;
                  _NotFound -> Loaded1

              end,

    {Component, Loaded2}.

%% @doc Retrive a mustache render function if the component has a template set.
get_template_fun(Component, Version, Diversity, DiversityURL) ->
    diversity_cache:get(
      {mustache_template_fun, Component, Version},
      fun () ->
              case maps:find(<<"template">>, Diversity) of
                  {ok, TemplatePath} ->
                      case diversity_api:get_file(Component, Version, TemplatePath, DiversityURL) of
                          {ok, Template} -> imperial:compile(Template);
                          undefined      -> undefined
                      end;
                  error ->
                      undefined
              end
      end,
      application:get_env(diversity, resource_cache_time, 3600000)
     ).

%% @doc Given a map of constraints for components this function finds the best matching version for
%% each of the components.
resolve_versions(ComponentConstraints, DiversityURL) ->
    maps:map(
      fun (Name, Constraints) ->
              Versions = diversity_api:get_versions(Name, DiversityURL),
              case diversity_semver:resolve_version(Constraints, Versions) of
                  undefined ->
                      lager:warning(
                        "Could not resolve version for component.~n"
                        "Component: ~p~n"
                        "Constraints: ~p",
                        [Name, Constraints]
                       ),
                      '*';
                  Version ->
                      Version
              end
      end,
      ComponentConstraints
     ).

%% @doc Map a function over a tree (of maps and lists) where the nodes might be components.
%% This is a bottom-first traversal. A component is naively identified by having the key
%% <<"component">> in it.
map(Fun, #{<<"component">> := _} = Component) ->
    Settings = maps:get(<<"settings">>, Component, #{}),
    Fun(Component#{<<"settings">> => map(Fun, Settings)});
map(Fun, Map) when is_map(Map) ->
    maps:map(fun (_Property, Value) -> map(Fun, Value) end, Map);
map(Fun, List) when is_list(List) ->
    lists:map(fun (Item) -> map(Fun, Item) end, List);
map(_Fun, Term) ->
    Term.

%% @doc Fold a function over a tree (of maps and lists) where the nodes might be components.
%% This is a bottom-first traversal. A component is naively identified by having the key
%% <<"component">> in it.
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

%% @doc A simple parallell map
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
