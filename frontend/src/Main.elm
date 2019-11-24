import Browser
import Html exposing (Html, Attribute, h3, hr, li, p, div, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.Styled exposing (styled)
import Html.Styled.Attributes exposing (css)
import Http
import Set exposing (Set)
import Json.Decode exposing (Decoder, field, map2, map3)
import Task


main =
  Browser.element 
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


-- MODEL

--type alias Tag = String

-- TODO: make the tags in Artifact and View sets for uniqueness
-- need to figure out how this is going to play with the Json parsing
type alias Artifact =
  { url : String
  , tags: List String
  , annotation : String
  }


type alias View =
  { name : String
  , tags : List String
  }


type alias Model = 
  { tags : Set String
  , selectedTags : Set String
  , artifacts : List Artifact
  , views : List View
  , selectedView : View
  , error : String
  }


init : () -> (Model, Cmd Msg)
init _ =
 ( { tags = Set.empty
   , selectedTags = Set.empty
   , artifacts = []
   , views = []
   , selectedView = { name = "All", tags = [] }
   , error = ""
   }
  , Cmd.batch 
    [ Http.get
      { url = "http://localhost:8080/views"
      , expect = Http.expectJson ReceiveViews decoderViews
      }
    , Http.get
      { url = "http://localhost:8080/tags"
      , expect = Http.expectJson ReceiveTags decoderTags
      }
    ]
 )


-- HTTP


fetchTags : Cmd Msg
fetchTags =
  Http.get
    { url = "http://localhost:8080/tags"
    , expect = Http.expectJson ReceiveTags decoderTags
    }


fetchViews : Cmd Msg
fetchViews =
  Http.get
    { url = "http://localhost:8080/views"
    , expect = Http.expectJson ReceiveViews decoderViews
    }

fetchArtifacts : Set String -> Cmd Msg
fetchArtifacts tags =
  Http.get
    { url = "http://localhost:8080/artifacts?tags=" ++ (String.join "," (Set.toList tags))
    , expect = Http.expectJson ReceiveArtifacts decoderArtifacts
    }


-- JSON

decoderTags : Decoder (List String)
decoderTags =
  Json.Decode.list Json.Decode.string


decoderViews : Decoder (List View)
decoderViews =
  Json.Decode.list 
    ( map2 View ( field "name" Json.Decode.string ) ( field "tags" decoderTags) )

decoderArtifacts : Decoder (List Artifact)
decoderArtifacts =
  field 
    "artifacts" 
    ( Json.Decode.list
      ( map3 Artifact (field "url" Json.Decode.string) ( field "tags" decoderTags) (field "annotation" Json.Decode.string)))


-- UPDATE

type Msg 
  = AddTag String
  | RemoveTag String
  | SelectView View
  | FetchArtifacts 
  | ReceiveArtifacts (Result Http.Error (List Artifact))
  | ReceiveViews (Result Http.Error (List View))
  | ReceiveTags (Result Http.Error (List String))


addTag : String -> Model -> Model
addTag tag model =
  { model | selectedTags = Set.insert tag model.selectedTags }


removeTag : String -> Model -> Model
removeTag tag model =
  { model | selectedTags = Set.filter (\n -> n /= tag) model.selectedTags }


selectView : View -> Model -> Model
selectView tagView model =
  { model | selectedView = tagView }
  |> \m -> { m | selectedTags = Set.fromList m.selectedView.tags }

setError : String -> Model -> Model
setError error model =
  { model | error = error }


-- TODO: Reconsider sending a Cmd to fetch the artifacts on tag selection updates
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    AddTag tag ->
      (addTag tag model, send FetchArtifacts)

    RemoveTag tag ->
      (removeTag tag model, send FetchArtifacts)

    SelectView tagView ->
      (selectView tagView model, send FetchArtifacts)

    FetchArtifacts ->
      (model, fetchArtifacts model.selectedTags)

    ReceiveArtifacts result ->
      case result of
        Ok artifacts ->
          ( { model | artifacts = artifacts }, Cmd.none )

        Err _ ->
          ( setError "Error fetching artifacts" model, Cmd.none )

    ReceiveTags result ->
      case result of 
        Ok tags ->
          ( { model | tags = Set.fromList tags }, Cmd.none )

        Err _ ->
          ( setError "Error fetching tags" model, Cmd.none )

    ReceiveViews result ->
      case result of
        Ok views ->
          ( { model | views = views }, Cmd.none)

        Err _ ->
          ( setError "Error feteching views" model, Cmd.none )


-- VIEW

renderViewListItem : View -> Html Msg
renderViewListItem tagView =
  li
    []
    [ p
      [ onClick ( SelectView tagView ) ]
      [ text tagView.name ]
    ]


renderTag : String -> Html Msg
renderTag tag =
  li 
    [] 
    [ p 
      [ onClick ( AddTag tag )
      , class "tag" 
      ] 
      [ text tag ] 
    ]


renderSelectedTag : String -> Html Msg
renderSelectedTag tag =
  li 
    [] 
    [ p 
      [ onClick ( RemoveTag tag )
      , class "tag" 
      ] [ text tag ] 
    ]


renderArtifactTag : String -> Html Msg
renderArtifactTag tag =
  li [] [ p [ class "tag"] [ text tag ] ]


renderArtifact : Artifact -> Html Msg
renderArtifact artifact =
  div []
    [
      p [] [ text (artifact.url ++ " [" ++ artifact.annotation ++ "]") ],
      ul [] (List.map renderArtifactTag artifact.tags)
    ]

renderError : String -> Html Msg
renderError error =
  div []
    [
      p [] [ text error ]
    ]


view : Model -> Html Msg
view model = 
  div []
    [ div [] 
      [ renderError model.error 
      , h3 [] [ text "Views" ]
      , ul [] ( List.map renderViewListItem model.views )
      , hr [] []
      , h3 [] [ text "Tags" ]
      , ul [] ( List.map renderTag ( Set.toList model.tags ) )
      , hr [] []
      , h3 [] [ text "Selected Tags" ]
      , ul [] ( List.map renderSelectedTag ( Set.toList model.selectedTags ) )
      , hr [] []
      , h3 [] [ text "Builds" ]
      , div [] [ ul [] (List.map renderArtifact model.artifacts) ]
      ]
    ]


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


-- EXTRAS

-- Send a message
send : msg -> Cmd msg
send msg =
  Task.succeed msg
  |> Task.perform identity
