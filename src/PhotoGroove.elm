module PhotoGroove exposing (main)
import Browser
import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, src, type_, name, checked)
import Html.Events exposing (onClick)
import Random

type alias Photo =
    { url : String }

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
    , classList [ ("selected", selectedUrl == thumb.url)]
    , onClick (ClickedPhoto thumb.url)
    ][]

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
 case msg of
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
      init = \flags -> (initialModel, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \model -> Sub.none
    }