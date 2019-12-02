module PhotoGroove exposing (main)
import Browser
import Array exposing (Array)
import Html exposing (Html)
import Html.Attributes exposing (id, class, classList, src)
import Html.Events exposing (onClick)

type alias Photo =
    { url : String }

type alias Model =
    { photos : List Photo
    , selectedUrl : String
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
  }

photoArray: Array Photo
photoArray = Array.fromList initialModel.photos

urlPrefix: String
urlPrefix = 
  "http://elm-in-action.com/"

view: Model -> Html Msg
view model = 
  Html.div [ class "content" ]
    [ Html.h1 [] [ Html.text "Photo Groove" ]
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
