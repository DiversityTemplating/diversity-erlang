-module(diversity).

-export([render/2]).

-include_lib("eunit/include/eunit.hrl").

%% @doc Render a diversity component with a given context
render(Params, Context) ->
    %% Retrive a list of all components in the settings tree
    io:format("Retriving components from settings...~n"),
    {ComponentList0, ComponentConstraints} = fold(fun get_component/2, {[], #{}}, Params),
    ComponentList1 = lists:reverse(ComponentList0),

    %% Resolve the correct version of the components
    io:format("Resolving components versions...~n"),
    ComponentVersions = maps:map(fun resolve_component/2, ComponentConstraints),

    %% Load all components
    Language = maps:get(<<"language">>, Context, <<"en">>),
    LoadComponent = fun ({Name, Version}) -> load_component(Name, Version, Language) end,
    io:format("Loading components...~n"),
    Components = maps:from_list(pmap(LoadComponent, maps:to_list(ComponentVersions))),

    %% Retrive a list of all components dependencies
    io:format("Retriving dependencies of components..."),
    {_, {DependencyList0, DependencyConstraints}} = lists:foldl(fun get_dependency/2, {Components, {[], #{}}}, ComponentList0),
    DependencyList1 = lists:reverse(DependencyList0),

    %% Resolve the correct version of the dependencies
    io:format("Resolving dependency versions...~n"),
    DependencyVersions = maps:map(fun resolve_component/2, DependencyConstraints),

    %% Load all dependencies
    io:format("Loading dependencies...~n"),
    Dependencies = maps:from_list(pmap(LoadComponent, maps:to_list(DependencyVersions))),

    AllComponentsList = DependencyList1 ++ ComponentList1,
    AllComponents = maps:merge(Dependencies, Components),
    io:format("Component order: ~p~n", [AllComponentsList]),

    %% Render the site
    io:format("Rendering components...~n"),
    #{<<"componentHTML">> := HTML} = map(render_fun(AllComponents, Context), Params),

    %% Serve
    io:format("Done...~n"),
    HTML.

render_fun(Components, Context0) ->
    fun (#{<<"component">> := Name} = Component) ->
            Settings = maps:get(<<"settings">>, Component, #{}),

            %% Render HTML if a template exist
            case maps:find(Name, Components) of
                {ok, #{<<"template">> := Template}} ->
                    Context = Context0#{<<"settings">> => Settings, <<"settingsJSON">> => jiffy:encode(Settings)},
                    %?debugFmt("~nComponent: ~p~nTemplate: ~p~nContext: ~p~n", [Name, Template, Context]),
                    Rendered = iolist_to_binary(mustache:render(Template, Context)),
                    Component#{<<"componentHTML">> => Rendered};
                error ->
                    Component
            end
    end.

resolve_component(_Name, Constraints) ->
    diversity_semver:resolve_constraints(Constraints).

get_component(#{<<"component">> := Name} = Component, Acc) ->
    Constraint = maps:get(<<"version">>, Component, <<"*">>),
    add_constraint(Name, Constraint, Acc).

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

get_dependency(Name, {Components, Acc0}) ->
    #{<<"diversity">> := Diversity} = maps:get(Name, Components),
    Dependencies = maps:get(<<"dependencies">>, Diversity, #{}),
    {Components,
     maps:fold(
       fun (Dependency, Constraint, Acc) -> add_constraint(Dependency, Constraint, Acc) end,
       Acc0,
       Dependencies
      )}.

load_component(Component, Version, Language) ->
    %% Retrive the diversity.json
    {ok, Diversity} = diversity_api_client:get_diversity_json(Component, Version),
    Loaded0 = #{<<"diversity">> => Diversity},

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
