-module(diversity).

-export([render/2]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          context,
          components,
          templates    = #{},
          translations = #{}
         }).

%% @doc Render a diversity component with a given context
render(Params, Context) ->
    %% Retrive a list of all components with the right version
    {ComponentList0, ComponentsVersions} = fold(fun get_component/2, {[], #{}}, Params),
    ComponentList1 = lists:reverse(ComponentList0),

    ?debugFmt("~nComponents: ~p~nVersions: ~p~n", [ComponentList1, ComponentsVersions]),

    Components = maps:map(fun resolve_component/2, ComponentsVersions),

    ?debugFmt("~nComponents: ~p~n", [Components]),

    %% Make sure all components are available
    State0 = #state{components = Components, context = Context},
    State1 = load_components(State0),

    %?debugFmt("~nState: ~p~n", [State]),

    #{<<"componentHTML">> := HTML} = map(render_fun(State1), Params),
    HTML.

render_fun(#state{context = Context0, templates = Templates}) ->
    fun (#{<<"component">> := Name} = Component) ->
            Settings = maps:get(<<"settings">>, Component, #{}),

            %% Render HTML if a template exist
            case maps:find(Name, Templates) of
                {ok, Template} ->
                    Context = Context0#{<<"settings">> => Settings, <<"settingsJSON">> => jiffy:encode(Settings)},
                    Rendered = unicode:characters_to_binary(mustache:render(Template, Context)),
                    %?debugFmt("~nComponent: ~p~nTemplate: ~p~nContext: ~p~nRendered: ~p~n", [Name, Template, Context1, Rendered]),
                    Component#{<<"componentHTML">> => Rendered};
                error ->
                    Component
            end
    end.

resolve_component(_Name, _Constraints) ->
    <<"*">>.

get_component(#{<<"component">> := Name} = Component, {ComponentList0, Components0}) ->
    Constraint = maps:get(<<"version">>, Component, <<"*">>),
    case maps:find(Name, Components0) of
        {ok, Constraints} ->
            %% Add the version to the components constraints
            Components1 = maps:put(Name, [Constraint | Constraints], Components0),
            {ComponentList0, Components1};
        error ->
%            Diversity = diversity_api_client:get_diversity_json(Name, Version),
%            Dependencies = maps:get(<<"dependencies">>, Diversity, #{}),
%            LoadDependency = fun (Dependency, _Version, Acc) ->
%                                     get_component(#{<<"component">> => Dependency}, Acc)
%                             end,
%            {ComponentList1, Components1} = maps:fold(LoadDependency, Acc0, Dependencies),
            Constraints = [Constraint],
            ComponentList1 = [Name | ComponentList0],
            Components1 = maps:put(Name, Constraints, Components0),
            {ComponentList1, Components1}
    end.

load_components(State) ->
    maps:fold(
      fun load_component/3,
      State,
      State#state.components
     ).

load_component(Component, #{<<"version">> := Version} = Diversity, State) ->
    #state{context = Context,
           templates = Templates0,
           translations = Translations0} = State,
    Language = maps:get(language, Context, <<"en">>),

    %% Retrive the template if it exist
    Templates1 = case maps:find(<<"template">>, Diversity) of
                   {ok, TemplatePath} ->
                         Template = diversity_api_client:get_file(Component, Version, TemplatePath),
                         Compiled = mustache:compile(Template),
                         maps:put(Component, Compiled, Templates0);
                   error ->
                         Templates0
                 end,

    %% Retrive the translations if they exist
    Translations1 = case maps:find(Language, maps:get(<<"i18n">>, Diversity, #{})) of
                        {ok, #{<<"view">> := TranslationPath}} ->
                            try diversity_api_client:get_file(Component, Version, TranslationPath) of
                                Translation -> maps:put(Component, Translation, Translations0)
                            catch
                                throw:resource_not_found -> Translations0
                            end;
                        _NotFound ->
                            Translations0
                    end,

    State#state{templates = Templates1, translations = Translations1}.

map(Fun, #{<<"component">> := _, <<"settings">> := Settings} = Component) ->
    Fun(Component, map(Fun, Settings));
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
