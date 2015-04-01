-module(diversity).

-export([render/2]).

-include_lib("eunit/include/eunit.hrl").

-record(state, {
          context,
          components   = #{},
          templates    = #{},
          translations = #{}
         }).

%% @doc Render a diversity component with a given context
render(#{<<"component">> := Component} = Params, Context) ->
    %% Get latest if not otherwise specified
    Version = maps:get(<<"version">>, Params, <<"*">>),

    %% Get the diversity.json for the main component
    _Diversity = diversity_api_client:get_diversity_json(Component, Version),

    %% Get the settings from the supplied parameters
    Settings = maps:get(<<"settings">>, Params, #{}),

    %% Get the settings schema
    Schema = diversity_api_client:get_component_settings_schema(Component, Version),

    %% Retrive all components that need to be loaded
    Fun = fun (SubComponent, Components) ->
                  Entry0 = maps:with([<<"component">>, <<"version">>], SubComponent),
                  Entry1 = maps:merge(#{<<"version">> => <<"*">>}, Entry0),
                  [Entry1 | Components]
          end,
    Components0 = lists:reverse(fold(Settings, Schema, Fun, [])),

    %% Make sure all components are available
    State = load_components(Components0, Context),

    %?debugFmt("~nState: ~p~n", [State]),

    render_component(Settings, Schema, State).

render_component(#{<<"component">> := Name} = Component0, #{<<"type">> := <<"object">>, <<"format">> := <<"diversity">>},
                 #state{context = Context0, templates = Templates} = State) ->

    ?debugFmt("~nRendering component: ~p~n", [Name]),
    %% Get resolved version from the state
    #{<<"version">> := Version} = maps:get(Name, State#state.components),

    %% Get the settings schema
    Schema = diversity_api_client:get_component_settings_schema(Name, Version),

    %% Render the children first
    Settings0 = maps:get(<<"settings">>, Component0, #{}),
    Settings1 = render_component(Settings0, Schema, State),
    Component1 = Component0#{<<"settings">> => Settings1},

    %% Render HTML if a template exist
    case maps:find(Name, Templates) of
        {ok, Template} ->
            Context1 = Context0#{<<"settings">> => Settings1, <<"settingsJSON">> => jiffy:encode(Settings1)},
            Component1#{<<"componentHTML">> => render_mustache(Template, Context1)};
        error ->
            Component1
    end;
render_component(Map, #{<<"type">> := <<"object">>, <<"properties">> := Properties}, State) when is_map(Map) ->
    ?debugFmt("~nMap: ~p~n", [Map]),
    maps:map(fun (Property, Value) -> render_component(Value, maps:get(Property, Properties, #{}), State) end, Map);
render_component(List, #{<<"type">> := <<"array">>, <<"items">> := Schema}, State) when is_list(List), is_map(Schema) ->
    ?debugFmt("~nList: ~p~n", [List]),
    lists:map(fun (Item) -> render_component(Item, Schema, State) end, List);
render_component(Term, _Schema, _State) ->
    ?debugFmt("~nLeaf: ~p~n", [Term]),
    Term.

render_mustache(Template, _Context) ->
    Template.

load_components(Components0, Context) ->
    lists:foldl(
      fun (#{<<"component">> := Component, <<"version">> := Version}, State) ->
              case maps:is_key(Component, State#state.components) of
                  true  -> State;
                  false -> load_component(Component, Version, State)
              end
      end,
      #state{context = Context},
      Components0
     ).

load_component(Component, Version, State0) ->
    #state{context = Context,
           components = Components0,
           templates = Templates0,
           translations = Translations0} = State0,
    Language = maps:get(language, Context, <<"en">>),

    %% Retrive diversity.json
    Diversity = diversity_api_client:get_diversity_json(Component, Version),

    %% Add it the components fetched so far
    Components1 = maps:put(Component, Diversity, Components0),

    %% Retrive the template if it exist
    Templates1 = case maps:find(<<"template">>, Diversity) of
                   {ok, TemplatePath} ->
                         Template = diversity_api_client:get_file(Component, Version, TemplatePath),
                         maps:put(Component, Template, Templates0);
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

    %% Load the components dependencies
    Dependencies = maps:get(<<"dependencies">>, Diversity, #{}),
    State1 = State0#state{components = Components1, templates = Templates1, translations = Translations1},
    maps:fold(fun load_dependency/3, State1, Dependencies).

load_dependency(Dependency, Constraint, State) ->
    case maps:find(Dependency, State#state.components) of
        {ok, Component} ->
            Version = maps:get(<<"version">>, Component, <<"*">>),
            not satisfied(Version, Constraint) andalso error(version_constraint),
            State;
        error ->
            load_component(Dependency, <<"*">>, State)
    end.

satisfied(_Version, _Constraint) ->
    true.

fold(#{<<"component">> := Name, <<"settings">> := Settings} = Component,
     #{<<"type">> := <<"object">>, <<"format">> := <<"diversity">>},
     Fun, Acc0) ->
    Version = maps:get(<<"version">>, Component, <<"*">>),
    Acc1 = Fun(Component, Acc0),
    Schema = diversity_api_client:get_component_settings_schema(Name, Version),
    fold(Settings, Schema, Fun, Acc1);
fold(Settings, #{<<"type">> := <<"object">>, <<"properties">> := Properties}, Fun, Acc0) when is_map(Settings) ->
    maps:fold(
      fun (Property, Value, Acc) ->
              case maps:find(Property, Properties) of
                  {ok, PropertySchema} -> fold(Value, PropertySchema, Fun, Acc);

                  _            -> Acc
              end
      end,
      Acc0,
      Settings
     );
fold(Settings, #{<<"type">> := <<"array">>, <<"items">> := Schema}, Fun, Acc0) when is_list(Settings) ->
    lists:foldl(fun (Item, Acc) -> fold(Item, Schema, Fun, Acc) end, Acc0, Settings);
fold(_Settings, _Schema, _Fun, Acc) ->
    Acc.
