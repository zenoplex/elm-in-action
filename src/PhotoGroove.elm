module PhotoGroove exposing (main)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, src, title, type_, name, checked)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode
import Random

type alias Photo =
    { url : String
    , size: Int
    , title: String
    }

photoDecoder: Decoder Photo
photoDecoder = succeed Photo
  |> required "url" string
  |> required "size" int 
  |> optional "title" string "(untitled)"

type ThumbnailSize
  = Small
  | Medium 
  | Large

type alias Model =
    { status: Status
    , chosenSize: ThumbnailSize
    }

type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurprizeMe
  | GotRandomPhoto Photo
  | GotPhotos (Result Http.Error (List Photo))

type Status
  = Loading
  |  Loaded (List Photo) String
  |  Error String

initialModel: Model
initialModel = 
  { 
    status = Loading
  , chosenSize = Medium
  }

initialCmd: Cmd Msg
initialCmd = Http.get 
  { url = "http://elm-in-action.com/photos/list.json"
  , expect = Http.expectJson GotPhotos (list photoDecoder)
  }

urlPrefix: String
urlPrefix = 
  "http://elm-in-action.com/"

sizeToString: ThumbnailSize -> String
sizeToString size =
  case size of
    Small ->
      "small"
    Medium ->
      "medium"
    Large ->
      "large" 

sizeToClass: ThumbnailSize -> String
sizeToClass size =
  case size of
    Small ->
      "small"
    Medium ->
      "med"
    Large ->
      "large"

viewSizeChooser: ThumbnailSize -> Bool -> Html Msg
viewSizeChooser size isChosen  =
  Html.label []
    [
      Html.input [ type_ "radio", name "size", checked isChosen ,onClick (ClickedSize size)] []
    , Html.text (sizeToString size)
    ]

view: Model -> Html Msg
view model = 
  Html.div [ class "content" ] <| 
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model.chosenSize
      Loading ->
        []
      Error errorMessage ->
        [ Html.text ("Error: " ++ errorMessage)]

viewFilter: String -> Int -> Html Msg
viewFilter name magnitude =
  Html.div [ class "filter-slider" ]
    [ Html.label [] [ Html.text name ]
    , rangeSlider 
      [ Html.Attributes.max "11"
      , Html.Attributes.property "val" (Json.Encode.int magnitude)
      ] []
    , Html.label [] [ Html.text (String.fromInt magnitude)]
    ]

viewLoaded: List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize = 
  [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.h3 [] [ Html.text "Thumbnail size"]
    , Html.button [ onClick ClickedSurprizeMe ] [ Html.text "Select random image"]
    , Html.div [ id "choose-size" ] 
        <| List.map (\size -> viewSizeChooser size (chosenSize == size)) [Small, Medium, Large]
    , Html.div [ 
      id "thumbnails"
      , class (sizeToClass chosenSize) 
      ] (List.map (viewThumbnail selectedUrl) photos)
    , Html.img [ class "large", src (urlPrefix ++ "large/" ++ selectedUrl)][]
  ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  Html.img
    [ 
      src(urlPrefix ++ thumb.url)
    , title (thumb.title ++ "[" ++ String.fromInt thumb.size ++ "KB]")
    , classList [ ("selected", selectedUrl == thumb.url)]
    , onClick (ClickedPhoto thumb.url)
    ][]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case msg of
    GotPhotos (Ok photos) ->     
      case photos of
        first :: _ ->
          ({ model | status = Loaded photos first.url}, Cmd.none)      
        [] ->
          ({ model | status = Error "0 photos found"}, Cmd.none)     
    GotPhotos (Err _) ->
      ({ model | status = Error "Server error"}, Cmd.none)     
      
    GotRandomPhoto photo ->
      ({ model | status = selectUrl photo.url model.status }, Cmd.none)
    ClickedPhoto url -> 
      ({ model | status = selectUrl url model.status }, Cmd.none)
    ClickedSurprizeMe ->
      case model.status of
        Loaded (firstPhoto :: otherPhotos) _ -> 
          Random.uniform firstPhoto otherPhotos
            |> Random.generate GotRandomPhoto
            |> Tuple.pair model
        Loaded [] _ ->
          (model, Cmd.none)
        Loading ->
          (model, Cmd.none)
        Error _ ->
          (model, Cmd.none)
    ClickedSize size ->
      ({ model | chosenSize = size}, Cmd.none)

selectUrl: String -> Status -> Status
selectUrl url status =
  case status of
      Loaded photos _ ->
        Loaded photos url
      Loading ->
        status
      Error _ ->
        status

main: Program () Model Msg      
main =
  Browser.element
    { 
      init = \_ -> (initialModel, initialCmd)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

rangeSlider attributes children =
  Html.node "range-slider" attributes children