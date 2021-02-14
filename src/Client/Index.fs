module Index

open Elmish
open Fable.Remoting.Client
open Shared

type Completions = { Items : string array; Selected : int option; Show : bool }

type Model =
    { Todos: Todo list
      Input: string
      Completions: Completions }

type Msg =
    | GotTodos of Todo list
    | SetInput of string
    | AddTodo
    | AddedTodo of Todo
    | UpdatedCompletions of string []
    | SelectCompletion of int
    | SelectCompletionOffset of int

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        { Todos = []
          Input = ""
          Completions = { Items = [||]; Selected = None ; Show = false } }
    let cmd = Cmd.OfAsync.perform todosApi.getTodos () GotTodos
    model, cmd

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | GotTodos todos ->
        { model with Todos = todos }, Cmd.none
    | SetInput value ->
        let cmd = Cmd.OfAsync.perform todosApi.autoComplete value UpdatedCompletions
        { model with Input = value }, cmd
    | AddTodo ->
        let todo = Todo.create model.Input
        let cmd = Cmd.OfAsync.perform todosApi.addTodo todo AddedTodo
        { model with Input = "" }, cmd
    | AddedTodo todo ->
        { model with Todos = model.Todos @ [ todo ] }, Cmd.none
    | UpdatedCompletions completions ->
        { model with Completions = { Items = completions; Selected = None; Show = true } }, Cmd.none
    | SelectCompletionOffset offset ->
        let count = model.Completions.Items.Length
        let newSelected =
            match model.Completions.Selected with
            | Some current -> current
            | None when offset > 0 -> -1
            | None -> count
            |> fun initial -> (initial + offset + count) % count
        { model with Completions = { model.Completions with Selected = Some newSelected } }, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let viewCompletions dispatch completions =
    match completions.Items with
    | [||] -> []
    | _ ->
        let viewSuggestion i sug =
            let attributes = [
                yield OnMouseDown (fun _ -> dispatch (SelectCompletion i)) :> IHTMLProp
                if Some i = completions.Selected then yield Style [ BackgroundColor "#cccccc" ] :> IHTMLProp ]
            div attributes [ str sug ]
        [ div
            [ Style [ Position PositionOptions.Absolute; ZIndex 10.; BackgroundColor "#FFFFFF"; Border "1" ]; ClassName "border block" ]
            [ yield! completions.Items |> Array.mapi viewSuggestion ] ]

let containerBox (model : Model) (dispatch : Msg -> unit) =
    Box.box' [ ] [
        Content.content [ ] [
            Content.Ol.ol [ ] [
                for todo in model.Todos do
                    li [ ] [ str todo.Description ]
            ]
        ]
        Field.div [ Field.IsGrouped ] [
            Control.p [ Control.IsExpanded ] [
                Input.text [
                  Input.Value model.Input
                  Input.Placeholder "What needs to be done?"
                  Input.OnChange (fun x -> SetInput x.Value |> dispatch)
                  Input.Props [
                      OnKeyDown (fun k ->
                        match k.key with
                        | "ArrowUp" -> dispatch (SelectCompletionOffset -1)
                        | "ArrowDown" -> dispatch (SelectCompletionOffset 1)
                        | "Enter" -> dispatch AddTodo
                      )
                  ]]
                yield! viewCompletions dispatch model.Completions
            ]
            Control.p [ ] [
                Button.a [
                    Button.Color IsPrimary
                    Button.Disabled (Todo.isValid model.Input |> not)
                    Button.OnClick (fun _ -> dispatch AddTodo)
                ] [
                    str "Add"
                ]
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [
        Hero.Color IsPrimary
        Hero.IsFullHeight
        Hero.Props [
            Style [
                Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
                BackgroundSize "cover"
            ]
        ]
    ] [
        Hero.head [ ] [
            Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        ]

        Hero.body [ ] [
            Container.container [ ] [
                Column.column [
                    Column.Width (Screen.All, Column.Is6)
                    Column.Offset (Screen.All, Column.Is3)
                ] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "nugraph" ]
                    containerBox model dispatch
                ]
            ]
        ]
    ]
