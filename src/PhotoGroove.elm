port module PhotoGroove exposing (main)
import Browser
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (on, onClick)
import Http
import Json.Decode exposing (Decoder, at, int, list, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random

port setFilters: FilterOptions -> Cmd msg

type alias FilterOptions =
  { url: String
  , filters: List 
    { 
      name: String
    , amount: Int 
    }
  }

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
    , hue: Int
    , ripple: Int
    , noise: Int
    }

type Msg
  = ClickedPhoto String
  | ClickedSize ThumbnailSize
  | ClickedSurprizeMe
  | GotRandomPhoto Photo
  | GotPhotos (Result Http.Error (List Photo))
  | SlideHue Int
  | SlideRipple Int
  | SlideNoise Int

type Status
  = Loading
  |  Loaded (List Photo) String
  |  Error String

initialModel: Model
initialModel = 
  { 
    status = Loading
  , chosenSize = Medium
  , hue = 5
  , ripple = 5
  , noise = 5
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
      Html.input 
        [ 
          Attr.type_ "radio"
        , Attr.name "size"
        , Attr.checked isChosen
        , onClick (ClickedSize size)
        ] []
    , Html.text (sizeToString size)
    ]

view: Model -> Html Msg
view model = 
  Html.div [ Attr.class "content" ] <| 
    case model.status of
      Loaded photos selectedUrl ->
        viewLoaded photos selectedUrl model
      Loading ->
        []
      Error errorMessage ->
        [ Html.text ("Error: " ++ errorMessage)]

viewFilter: (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
  Html.div [ Attr.class "filter-slider" ]
    [ Html.label [] [ Html.text name ]
    , rangeSlider 
      [ Attr.max "11"
      , Attr.property "val" (Encode.int magnitude)
      , onSlide toMsg
      ] []
    , Html.label [] [ Html.text (String.fromInt magnitude)]
    ]

viewLoaded: List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model = 
  [ Html.h1 [] [ Html.text "Photo Groove" ]
    , Html.h3 [] [ Html.text "Thumbnail size"]
    , Html.button [ onClick ClickedSurprizeMe ] [ Html.text "Select random image"]
    , Html.div [ Attr.class "filters" ]
      [
        viewFilter SlideHue "Hue" model.hue
      , viewFilter SlideRipple "Ripple" model.ripple
      , viewFilter SlideNoise "Noise" model.noise
      ]
    , Html.div [ Attr.id "choose-size" ] 
        <| List.map (\size -> viewSizeChooser size (model.chosenSize == size)) [Small, Medium, Large]
    , Html.div 
        [ 
          Attr.id "thumbnails"
        , Attr.class (sizeToClass model.chosenSize) 
        ] (List.map (viewThumbnail selectedUrl) photos)
    , Html.img [ Attr.class "large", Attr.src (urlPrefix ++ "large/" ++ selectedUrl)][]
  ]

viewThumbnail: String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
  Html.img
    [ 
      Attr.src(urlPrefix ++ thumb.url)
    , Attr.title (thumb.title ++ "[" ++ String.fromInt thumb.size ++ "KB]")
    , Attr.classList [ ("selected", selectedUrl == thumb.url)]
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
      applyFilters { model | status = selectUrl photo.url model.status }
    ClickedPhoto url -> 
      applyFilters { model | status = selectUrl url model.status }
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
    SlideHue hue ->
      ({ model | hue = hue}, Cmd.none)
    SlideRipple ripple ->
      ({ model | ripple = ripple}, Cmd.none)
    SlideNoise noise ->
      ({ model | noise = noise}, Cmd.none)

applyFilters: Model -> (Model, Cmd Msg)
applyFilters model =
  case model.status of
    Loaded photos selectedUrl ->
      let
        filters = [{ name = "Hue", amount = model.hue }
          , { name = "Ripple", amount = model.ripple }
          , { name = "Noise", amount = model.noise }
          ]
        url = urlPrefix ++ "/large" ++ selectedUrl
      in
      (model, setFilters { url = url, filters = filters })
    Loading ->
      (model, Cmd.none)
    Error msg ->
      (model, Cmd.none)

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

rangeSlider: List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
  Html.node "range-slider" attributes children

onSlide: (Int -> msg) -> Html.Attribute msg
onSlide toMsg = 
  at ["detail", "slideTo"] int
    |> Json.Decode.map toMsg
    |> on "slide"
  