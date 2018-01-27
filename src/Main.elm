module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Styles


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }


type alias Model =
    { id : Int
    , me : User
    , talks : List ( Talk, Maybe String )
    , inputText : String
    }


type alias User =
    { name : String
    , image : String
    }


type alias Talk =
    { id : Int
    , by : User
    , text : String
    , time : String
    }


isMine : User -> Talk -> Bool
isMine user talk =
    user.name == talk.by.name


type Msg
    = Input String
    | Submit
    | StartEdit Int
    | InputEditingText Int String
    | EndEdit Int
    | Delete Int


init : ( Model, Cmd Msg )
init =
    ( Model 0 user1 talks ""
    , Cmd.none
    )


user1 : User
user1 =
    User "とみざわ"
        "http://www.hochi.co.jp/photo/20170718/20170718-OHT1I50084-T.jpg"


user2 : User
user2 =
    User "伊達ちゃん"
        "https://imgcp.aacdn.jp/img-c/680/auto/tipsplus/series/246/20160608_1465380998273.jpg"


talks : List ( Talk, Maybe String )
talks =
    [ ( Talk -2
            user2
            "ピザ食いてえ"
            "2018/01/27 13:00"
      , Nothing
      )
    , ( Talk -1
            user1
            "ちょっと何言ってるかわかんないっす"
            "2018/01/27 13:30"
      , Nothing
      )
    ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input s ->
            ( { model | inputText = s }
            , Cmd.none
            )

        Submit ->
            let
                talk =
                    Talk model.id
                        model.me
                        model.inputText
                        "2018/01/27 14:00"
            in
            ( { model
                | id = model.id + 1
                , inputText = ""
                , talks = model.talks ++ [ ( talk, Nothing ) ]
              }
            , Cmd.none
            )

        StartEdit id ->
            ( { model
                | talks =
                    List.map
                        (\( talk, text ) ->
                            if talk.id == id then
                                ( talk, Just talk.text )
                            else
                                ( talk, text )
                        )
                        model.talks
              }
            , Cmd.none
            )

        InputEditingText id newText ->
            ( { model
                | talks =
                    List.map
                        (\( talk, text ) ->
                            if talk.id == id then
                                ( talk, Just newText )
                            else
                                ( talk, text )
                        )
                        model.talks
              }
            , Cmd.none
            )

        EndEdit id ->
            ( { model
                | talks =
                    List.map
                        (\( talk, text ) ->
                            if talk.id == id then
                                case text of
                                    Just s ->
                                        ( { talk | text = s }, Nothing )

                                    Nothing ->
                                        ( talk, Nothing )
                            else
                                ( talk, text )
                        )
                        model.talks
              }
            , Cmd.none
            )

        Delete id ->
            ( { model
                | talks = List.filter (\( talk, _ ) -> talk.id /= id) model.talks
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div [ Styles.mainWrap ]
        (viewForm model.me model.inputText
            :: List.map (viewTalk model.me) model.talks
        )


viewForm : User -> String -> Html Msg
viewForm user inputText =
    div [ Styles.postForm ]
        [ div [ Styles.formLeft ]
            [ img [ Styles.selfImg, src user.image ] []
            ]
        , div [ Styles.formRight ]
            [ textarea [ Styles.formArea, onInput Input, value inputText ] []
            , button [ Styles.postButton, onClick Submit ] [ text "投稿！" ]
            ]
        ]


viewTalk : User -> ( Talk, Maybe String ) -> Html Msg
viewTalk me ( talk, maybeEditingText ) =
    div [ Styles.talk ]
        [ div [ Styles.talkLeft ]
            [ img [ Styles.posterImg, src talk.by.image ] [] ]
        , div [ Styles.talkRight ]
            [ div [ Styles.posterName ] [ text talk.by.name ]
            , viewBody talk maybeEditingText
            , div [ Styles.talkFooter ]
                [ text talk.time
                , viewEditButtons me talk (maybeEditingText /= Nothing)
                ]
            ]
        ]


viewBody : Talk -> Maybe String -> Html Msg
viewBody talk maybeEditingText =
    case maybeEditingText of
        Just editingText ->
            textarea
                [ Styles.editingMessage
                , value editingText
                , onInput (InputEditingText talk.id)
                ]
                []

        Nothing ->
            div [ Styles.message ] [ text talk.text ]


viewEditButtons : User -> Talk -> Bool -> Html Msg
viewEditButtons me talk isEditing =
    if isMine me talk then
        div [ Styles.buttons ]
            [ if isEditing then
                button [ Styles.editButton, onClick (EndEdit talk.id) ] [ text "完了" ]
              else
                button [ Styles.editButton, onClick (StartEdit talk.id) ] [ text "編集" ]
            , button [ Styles.deleteButton, onClick (Delete talk.id) ] [ text "削除" ]
            ]
    else
        text ""
