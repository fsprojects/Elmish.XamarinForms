// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous

open System
open System.Diagnostics

/// Representation of the host framework with access to the root view to update (e.g. Xamarin.Forms.Application)
type IHost =
    /// Gets a reference to the root view item (e.g. Xamarin.Forms.Application.MainPage)
    abstract member GetRootView : unit -> obj
    /// Sets a new instance of the root view item (e.g. Xamarin.Forms.Application.MainPage)
    abstract member SetRootView : obj -> unit

/// We store the current dispatch function for the running Elmish program as a 
/// static-global thunk because we want old view elements stored in the `dependsOn` global table
/// to be recyclable on resumption (when a new ProgramRunner gets created).
type internal ProgramDispatch<'msg>()  = 
    static let mutable dispatchImpl = (fun (_msg: 'msg) -> failwith "do not call dispatch during initialization" : unit)

    static let dispatch = 
        id (fun msg -> 
            dispatchImpl msg)

    static member DispatchViaThunk = dispatch 
    static member SetDispatchThunk v = dispatchImpl <- v

/// Program type captures various aspects of program behavior
type Program<'arg, 'model, 'msg> = 
    { init : 'arg -> 'model * Cmd<'msg>
      update : 'msg -> 'model -> 'model * Cmd<'msg>
      subscribe : 'model -> Cmd<'msg>
      view : 'model -> Dispatch<'msg> -> ViewElement
      canReuseView: ViewElement -> ViewElement -> bool
      syncDispatch: Dispatch<'msg> -> Dispatch<'msg>
      syncAction: (unit -> unit) -> (unit -> unit)
      debug : bool
      onError : (string * exn) -> unit }
  
 type private ViewMsg<'msg> =
     |Render of 'msg
     |Completed

/// Starts the Elmish dispatch loop for the page with the given Elmish program
type ProgramRunner<'arg, 'model, 'msg>(host: IHost, program: Program<'arg, 'model, 'msg>, arg: 'arg) = 

    do Debug.WriteLine "run: computing initial model"

    // Get the initial model
    let (initialModel,cmd) = program.init arg
    let mutable alternativeRunner : ProgramRunner<obj,obj,obj> option = None

    let mutable lastModel = initialModel
    let mutable lastViewDataOpt = None
    let dispatch = ProgramDispatch<'msg>.DispatchViaThunk
    let mutable reset = (fun () -> ())

    // If the view is dynamic, create the initial page
    let viewInfo = 
        let newRootElement = program.view initialModel dispatch
        let rootView = newRootElement.Create()
        host.SetRootView(rootView)
        newRootElement

    let updateView updatedModel =
        match lastViewDataOpt with
        | None -> 
            lastViewDataOpt <- Some viewInfo

        | Some prevPageElement ->
            let newPageElement = 
                try program.view updatedModel dispatch
                with ex -> 
                    program.onError ("Unable to evaluate view:", ex)
                    prevPageElement

            if program.canReuseView prevPageElement newPageElement then
                let rootView = host.GetRootView()
                newPageElement.UpdateIncremental (prevPageElement, rootView)
            else
                let pageObj = newPageElement.Create()
                host.SetRootView(pageObj)

            lastViewDataOpt <- Some newPageElement
    let viewInbox = MailboxProcessor.Start (fun inbox ->
         let mutable isRendering = false
         // We use local mutable instead of loop state here because we need a kind of "kill switch" for all events coming after render starts.
         //If we use message for setting isRendered, this can lead to race conditions and two render processes may start simultaneously.
         //Mutable variable helps us reset the blocking state immediately, so the next event occured would invoke render if needed.
         let rec loop (state,hasChanges)  = async{
             match! inbox.Receive() with
             | ViewMsg.Render msg when (not isRendering)->
                 isRendering<-true
                 /// Here we put an inbox into the "waiting" state and wait for render finishes
                 Debug.WriteLine "View invoked"
                 async{
                     program.syncAction (fun()->updateView msg) ()
                     isRendering<-false
                     inbox.Post (Completed)
                 } |> Async.Start
                 return! loop (msg,false)
             | ViewMsg.Completed -> 
                 /// When we finished render and have "dirty" changes ,we should invoke Render again to be sure to receive actual view
                 if(hasChanges) then
                     inbox.Post (Render state)
                 return! loop (state,hasChanges)
                 
             | ViewMsg.Render msg when isRendering->
                  /// Here we just accumulate changes, no actual rendering happens
                 return! loop (msg,true)
         }
         
         loop (initialModel,false)      
          )   
        // Start Elmish dispatch loop  
    let processMsg msg = 
        try
            let (updatedModel,newCommands) = program.update msg lastModel
            lastModel <- updatedModel
            try 
                 viewInbox.Post (ViewMsg.Render updatedModel)
            with ex ->
                program.onError ("Unable to update view:", ex)
            for sub in newCommands do
                try 
                    sub dispatch
                with ex ->
                    program.onError ("Error executing commands:", ex)
        with ex ->
            program.onError ("Unable to process a message:", ex)                   
    do 
        // Set up the global dispatch function
        ProgramDispatch<'msg>.SetDispatchThunk (processMsg |> program.syncDispatch)

        reset <- (fun () -> updateView lastModel  ) |> program.syncAction

        Debug.WriteLine "updating the initial view"

        updateView initialModel 

        Debug.WriteLine "dispatching initial commands"
        for sub in (program.subscribe initialModel @ cmd) do
            try 
                sub dispatch
            with ex ->
                program.onError ("Error executing commands:", ex)

    member __.Argument = arg
    
    member __.CurrentModel = lastModel 

    member __.Dispatch(msg) = dispatch msg

    member runner.ChangeProgram(newProgram: Program<obj,obj, obj>) : unit =
        let action = program.syncAction (fun () -> 
            // TODO: transmogrify the model
            try
                alternativeRunner <- Some (ProgramRunner<obj,obj, obj>(host, newProgram, runner.Argument))
            with ex ->
                program.onError ("Error changing the program:", ex)
        )
        action()

    member __.ResetView() : unit  =
        let action = program.syncAction (fun () -> 
            match alternativeRunner with 
            | Some r -> r.ResetView()
            | None -> reset()
        )
        action()

    /// Set the current model, e.g. on resume
    member __.SetCurrentModel(model, cmd: Cmd<_>) =
        let action = program.syncAction (fun () -> 
            match alternativeRunner with 
            | Some _ -> 
                // TODO: transmogrify the resurrected model
                printfn "SetCurrentModel: ignoring (can't the model after ChangeProgram has been called)"
            | None -> 
                Debug.WriteLine "updating the view after setting the model"
                lastModel <- model
                updateView model
                for sub in program.subscribe model @ cmd do
                    sub dispatch
        )
        action()

/// Program module - functions to manipulate program instances
[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Program =
    let internal onError (text: string, ex: exn) = 
        Console.WriteLine (sprintf "%s: %A" text ex)

    /// Typical program, new commands are produced by `init` and `update` along with the new state.
    let mkProgram (init : 'arg -> 'model * Cmd<'msg>) (update : 'msg -> 'model -> 'model * Cmd<'msg>) (view : 'model -> Dispatch<'msg> -> ViewElement) =
        { init = init
          update = update
          view = view
          canReuseView = fun _ _ -> false
          syncDispatch = id
          syncAction = id
          subscribe = fun _ -> Cmd.none
          debug = false
          onError = onError }

    /// Simple program that produces only new state with `init` and `update`.
    let mkSimple (init : 'arg -> 'model) (update : 'msg -> 'model -> 'model) (view : 'model -> Dispatch<'msg> -> ViewElement) = 
        mkProgram (fun arg -> init arg, Cmd.none) (fun msg model -> update msg model, Cmd.none) view

    /// Typical program, new commands are produced discriminated unions returned by `init` and `update` along with the new state.
    let mkProgramWithCmdMsg (init: 'arg -> 'model * 'cmdMsg list) (update: 'msg -> 'model -> 'model * 'cmdMsg list) (view: 'model -> Dispatch<'msg> -> ViewElement) (mapToCmd: 'cmdMsg -> Cmd<'msg>) =
        let convert = fun (model, cmdMsgs) -> model, (cmdMsgs |> List.map mapToCmd |> Cmd.batch)
        mkProgram (fun arg -> init arg |> convert) (fun msg model -> update msg model |> convert) view

    /// Subscribe to external source of events.
    /// The subscription is called once - with the initial (or resumed) model, but can dispatch new messages at any time.
    let withSubscription (subscribe : 'model -> Cmd<'msg>) (program: Program<'arg, 'model, 'msg>) =
        let sub model =
            Cmd.batch [ program.subscribe model
                        subscribe model ]
        { program with subscribe = sub }

    /// Trace all the updates to the console
    let withConsoleTrace (program: Program<'arg, 'model, 'msg>) =
        let traceInit arg =
            try 
                let initModel,cmd = program.init arg
                Console.WriteLine (sprintf "Initial model: %0A" initModel)
                initModel,cmd
            with e -> 
                Console.WriteLine (sprintf "Error in init function: %0A" e)
                reraise ()

        let traceUpdate msg model =
            Console.WriteLine (sprintf "Message: %0A" msg)
            try 
                let newModel,cmd = program.update msg model
                Console.WriteLine (sprintf "Updated model: %0A" newModel)
                newModel,cmd
            with e -> 
                Console.WriteLine (sprintf "Error in model function: %0A" e)
                reraise ()

        let traceView model dispatch =
            Console.WriteLine (sprintf "View, model = %0A" model)
            try 
                let info = program.view model dispatch
                Console.WriteLine (sprintf "View result: %0A" info)
                info
            with e -> 
                Console.WriteLine (sprintf "Error in view function: %0A" e)
                reraise ()
                
        { program with
            init = traceInit 
            update = traceUpdate
            view = traceView }

    /// Trace all the messages as they update the model
    let withTrace trace (program: Program<'arg, 'model, 'msg>) =
        { program
            with update = fun msg model -> trace msg model; program.update msg model}

    /// Handle dispatch loop exceptions
    let withErrorHandler onError (program: Program<'arg, 'model, 'msg>) =
        { program
            with onError = onError }

    /// Set debugging to true
    let withDebug program = 
        { program with debug = true }

    /// Set the canReuseView function
    let withCanReuseView canReuseView program = 
        { program with canReuseView = canReuseView }

    /// Set the syncDispatch function
    let withSyncDispatch syncDispatch program = 
        { program with syncDispatch = syncDispatch }

    /// Set the syncAction function
    let withSyncAction syncAction program = 
        { program with syncAction = syncAction }
        
    /// Run the app with Fabulous
    let runWithFabulous (host : IHost) (arg: 'arg) (program: Program<'arg, 'model, 'msg>) = 
        ProgramRunner(host, program, arg)

    /// Run the app with Fabulous
    let runFabulous (host : IHost) (program: Program<unit, 'model, 'msg>) = 
        runWithFabulous host () program