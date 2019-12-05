module PhotoGroove exposing (main)
import Browser
import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, src, type_, name, checked)
import Html.Events exposing (onClick)

type alias Photo =
    { url : String }

type ThumbnailSize
  = Small
  | Medium 
  | Large

type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize: ThumbnailSize
    }

type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurprizeMe

initialModel: Model
initialModel = 
  { photos = 
  [{ url = "1.jpeg" }
  ,{ url = "2.jpeg" }
  ,{ url = "3.jpeg" }
  ]
  , selectedUrl = "1.jpeg"
  , chosenSize = Medium
  }

photoArray: Array Photo
photoArray = Array.fromList initialModel.photos

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
  Html.div [ class "content" ]
    [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.h3 [] [ Html.text "Thumbnail size"]
    , Html.button [ onClick ClickedSurprizeMe ] [ Html.text "Choose second image"]
    , Html.div [ id "choose-size" ] (List.map (\size -> viewSizeChooser size (model.chosenSize == size)) [Small, Medium, Large])
    , Html.div [ 
      id "thumbnails"
      , class (sizeToClass model.chosenSize) 
      ] (List.map (viewThumbnail model.selectedUrl) model.photos)
    , Html.img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl)][]
    ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  Html.img
    [ 
      src(urlPrefix ++ thumb.url)
    , classList [ ("selected", selectedUrl == thumb.url)]
    , onClick (ClickedPhoto thumb.url)
    ][]

getPhotoUrl: Int -> String
getPhotoUrl index =
  case Array.get index photoArray of
    Just photo ->
      photo.url
    Nothing ->
      ""

update: Msg -> Model -> Model
update msg model =
 case msg of
    ClickedPhoto url ->
      { model | selectedUrl = url }
    ClickedSurprizeMe ->
      { model | selectedUrl = "2.jpeg" }
    ClickedSize size ->
      { model | chosenSize = size}
main =
  Browser.sandbox
    { 
      init = initialModel
    , view = view
    , update = update
    }
