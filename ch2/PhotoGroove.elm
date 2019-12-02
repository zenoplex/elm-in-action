module PhotoGroove exposing (main)
import Html
import Html.Attributes exposing (id, class, src)

initialModel = 
  [{ url = "1.jpeg" }
  ,{ url = "2.jpeg" }
  ,{ url = "3.jpeg" }
  ]

urlPrefix = 
  "http://elm-in-action.com/"

view model = 
  Html.div [ class "content" ]
    [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.div [ id "thumbnails" ] (List.map viewThumbnail model)
    ]

viewThumbnail thumb =
  Html.img [ src(urlPrefix ++ thumb.url)][]
              
              
main =
  view initialModel
