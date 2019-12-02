module PhotoGroove exposing (main)
import Browser
import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, src, type_, name)
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

type alias Msg =
    { description : String
    , data : String
    }

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

viewSizeChooser: ThumbnailSize -> Html Msg
viewSizeChooser size =
  Html.label []
    [
      Html.input [ type_ "radio", name "size"] []
    , Html.text (sizeToString size)
    ]

view: Model -> Html Msg
view model = 
  Html.div [ class "content" ]
    [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.h3 [] [ Html.text "Thumbnail size"]
    , Html.div [ id "choose-size" ]
      [
        viewSizeChooser Small
      , viewSizeChooser Medium
      , viewSizeChooser Large
     ]
    , Html.button [ onClick { description = "ClickedSurprizeMe", data = ""} ] [ Html.text "Surprize Me!"]
    , Html.div [ id "thumbnails" ] (List.map (viewThumbnail model.selectedUrl) model.photos)
    , Html.img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl)][]
    ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  Html.img
    [ 
      src(urlPrefix ++ thumb.url)
    , classList [ ("selected", selectedUrl == thumb.url)]
    , onClick { description = "ClickedPhoto", data = thumb.url}
    ][]

update: Msg -> Model -> Model
update msg model =
 case msg.description of
    "ClickedPhoto" ->
      { model | selectedUrl = msg.data }
    "ClickedSurprizeMe" ->
      { model | selectedUrl = "2.jpeg" }
    _ ->
      model
main =
  Browser.sandbox
    { 
      init = initialModel
    , view = view
    , update = update
    }
