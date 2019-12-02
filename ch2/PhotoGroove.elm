module PhotoGroove exposing (main)
import Browser
import Html
import Html.Attributes exposing (id, class, classList, src)
import Html.Events exposing (onClick)

initialModel = 
  { photos = 
  [{ url = "1.jpeg" }
  ,{ url = "2.jpeg" }
  ,{ url = "3.jpeg" }
  ]
  , selectedUrl = "1.jpeg"
  }

urlPrefix = 
  "http://elm-in-action.com/"

view model = 
  Html.div [ class "content" ]
    [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.div [ id "thumbnails" ] (List.map (viewThumbnail model.selectedUrl) model.photos)
    , Html.img [ class "large", src (urlPrefix ++ "large/" ++ model.selectedUrl)][]
    ]

viewThumbnail selectedUrl thumb =
  Html.img
    [ 
      src(urlPrefix ++ thumb.url)
    , classList [ ("selected", selectedUrl == thumb.url)]
    , onClick { description = "ClickedPhoto", data = thumb.url}
    ][]

update msg model =
  if msg.description == "ClickedPhoto" then
    { model | selectedUrl = msg.data }
  else
    model

main =
  Browser.sandbox
    { 
      init = initialModel
    , view = view
    , update = update
    }
