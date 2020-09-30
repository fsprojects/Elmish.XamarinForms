// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.CodeGen.Generator

open Fabulous.CodeGen
open Fabulous.CodeGen.Binder.Models
open Fabulous.CodeGen.Text
open Fabulous.CodeGen.Generator.Models
open System.IO

module CodeGenerator =
    let getAttributeKeyName uniqueName =
        sprintf "%sAttribKey" uniqueName
    
    let getAttributeKey customAttributeKey uniqueName =
        match customAttributeKey with
        | None -> sprintf "ViewAttributes.%s" (getAttributeKeyName uniqueName)
        | Some attributeKey -> attributeKey
    
    let generateNamespace (namespaceOfGeneratedCode: string) (additionalNamespaces: string array) (w: StringWriter) = 
        w.printfn "// Copyright 2018-2020 Fabulous contributors. See LICENSE.md for license."
        w.printfn "namespace %s" namespaceOfGeneratedCode
        w.printfn ""
        w.printfn "#nowarn \"59\" // cast always holds"
        w.printfn "#nowarn \"66\" // cast always holds"
        w.printfn "#nowarn \"67\" // cast always holds"
        w.printfn "#nowarn \"760\""
        w.printfn ""
        w.printfn "open Fabulous"
        
        for additionalNamespace in additionalNamespaces do
            w.printfn "open %s" additionalNamespace
        
        w.printfn ""
        w

    let generateAttributes (members: AttributeData array) (w: StringWriter) =
        w.printfn "module ViewAttributes ="
        for m in members do
            let typeName =
                match m.Name with
                | "Created" -> "(obj -> unit)"
                | _ -> "_"
                
            w.printfn "    let %s : AttributeKey<%s> = AttributeKey<%s>(\"%s\")" (getAttributeKeyName m.UniqueName) typeName typeName m.UniqueName
        w.printfn ""
        w

    let generateBuildFunction (data: BuildData) (w: StringWriter) =
        let memberNewLine = "\n                              " + String.replicate data.Name.Length " " + " "
        let members =
            data.Members
            |> Array.map (fun m -> sprintf ",%s?%s: %s" memberNewLine m.Name m.InputType)
            |> Array.fold (+) ""

        let immediateMembers =
            data.Members
            |> Array.filter (fun m -> not m.IsInherited)

        w.printfn "    /// Builds the attributes for a %s in the view" data.Name
        w.printfn "    static member inline Build%s(attribCount: int%s) = " data.Name members

        if immediateMembers.Length > 0 then
            w.printfn ""
            for m in immediateMembers do
                if m.Name = "extraAttributes" then
                    w.printfn "        let attribCount = match %s with Some extraAttributes -> attribCount + extraAttributes.Length | None -> attribCount" m.Name
                else
                    w.printfn "        let attribCount = match %s with Some _ -> attribCount + 1 | None -> attribCount" m.Name
            
            w.printfn "        let extraCount = match extraAttributes with Some extraAttributes -> extraAttributes.Length | None -> 0"
            w.printfn ""

        
        match data.BaseName with 
        | None ->
            w.printfn "        let attribBuilder = AttributesBuilder(attribCount, extraCount)"
        | Some nameOfBaseCreator ->
            let baseMemberNewLine = "\n                                              " + String.replicate nameOfBaseCreator.Length " " + " "
            let baseMembers =
                data.Members
                |> Array.filter (fun m -> m.IsInherited)
                |> Array.mapi (fun index m -> sprintf ", %s?%s=%s" (if index > 0 && index % 5 = 0 then baseMemberNewLine else "") m.Name m.Name)
                |> Array.fold (+) ""
            w.printfn "        let attribBuilder = ViewBuilders.Build%s(attribCount%s)" nameOfBaseCreator baseMembers

        for m in immediateMembers do
            let attributeKey = getAttributeKey m.CustomAttributeKey m.UniqueName                
            w.printfn "        match %s with None -> () | Some v -> attribBuilder.Add(%s, %s(v)) " m.Name attributeKey m.ConvertInputToModel 

        w.printfn "        attribBuilder"
        w.printfn ""
        w

    let generateCreateFunction (data: CreateData option) (w: StringWriter) =
        match data with
        | None -> w
        | Some data ->
            w.printfn "    static member Create%s () : %s =" data.Name data.FullName
            
            if data.TypeToInstantiate = data.FullName then
                w.printfn "        %s()" data.TypeToInstantiate
            else
                w.printfn "        upcast (%s())" data.TypeToInstantiate
            
            w.printfn ""
            w
        
    let generateUpdateFunction (data: UpdateData) (w: StringWriter) =
        let generateProperties (properties: UpdateProperty array) =
            for p in properties do
                let hasApply = not (System.String.IsNullOrWhiteSpace(p.ConvertModelToValue)) || not (System.String.IsNullOrWhiteSpace(p.UpdateCode))
                let attributeKey = getAttributeKey p.CustomAttributeKey p.UniqueName

                // Check if the property is a collection
                match p.CollectionDataElementType with 
                | Some collectionDataElementType when not hasApply ->
                    w.printfn "        Collections.updateChildren prev%sOpt curr%sOpt target.%s" p.UniqueName p.UniqueName p.Name
                    w.printfn "            (fun x -> x.Create() :?> %s)" collectionDataElementType
                    w.printfn "            Collections.updateChild"
                    w.printfn "            (match registry.TryGetValue(%s.KeyValue) with true, func -> func | false, _ -> (fun _ _ _ -> ()))" attributeKey
                    
                | Some _ when hasApply ->
                    w.printfn "        %s prev%sOpt curr%sOpt target" p.UpdateCode p.UniqueName p.UniqueName
                    w.printfn "            (match registry.TryGetValue(%s.KeyValue) with true, func -> func | false, _ -> (fun _ _ _ -> ()))" attributeKey

                | _ ->
                    // If the type is ViewElement, then it's a type from the model
                    // Issue recursive calls to "Create" and "UpdateIncremental"
                    if p.ModelType = "ViewElement" && not hasApply then
                        w.printfn "        match prev%sOpt, curr%sOpt with" p.UniqueName p.UniqueName
                        w.printfn "        // For structured objects, dependsOn on reference equality"
                        w.printfn "        | ValueSome prevValue, ValueSome newValue when identical prevValue newValue -> ()"
                        w.printfn "        | ValueSome prevValue, ValueSome newValue when canReuseView prevValue newValue ->"
                        w.printfn "            newValue.UpdateIncremental(prevValue, target.%s)" p.Name
                        w.printfn "        | _, ValueSome newValue ->"
                        w.printfn "            target.%s <- (newValue.Create() :?> %s)" p.Name p.OriginalType
                        w.printfn "        | ValueSome _, ValueNone ->"
                        w.printfn "            target.%s <- null"  p.Name
                        w.printfn "        | ValueNone, ValueNone -> ()"

                    // Explicit update code
                    elif not (System.String.IsNullOrWhiteSpace(p.UpdateCode)) then
                        w.printfn "        %s prev%sOpt curr%sOpt target" p.UpdateCode p.UniqueName p.UniqueName

                    else
                        w.printfn "        match prev%sOpt, curr%sOpt with" p.UniqueName p.UniqueName
                        w.printfn "        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()"
                        w.printfn "        | _, ValueSome currValue -> target.%s <- %s currValue" p.Name p.ConvertModelToValue
                        if p.DefaultValue = "" then
                            w.printfn "        | ValueSome _, ValueNone -> target.ClearValue %s.%sProperty" data.FullName p.Name
                        else
                            w.printfn "        | ValueSome _, ValueNone -> target.%s <- %s" p.Name p.DefaultValue
                        w.printfn "        | ValueNone, ValueNone -> ()"
        
        w.printfn "    static member Update%s (registry: System.Collections.Generic.Dictionary<int, ViewElement voption -> ViewElement -> obj -> unit>, prevOpt: ViewElement voption, curr: ViewElement, target: %s) = " data.Name data.FullName
        
        // Attached properties updaters
        if data.PropertiesWithAttachedProperties.Length > 0 then
            for p in data.PropertiesWithAttachedProperties do
                let targetChildType = p.CollectionDataElementType |> Option.defaultValue "obj"
                w.printfn "        let update%sAttachedProperties overrideFunc (prevChildOpt: ViewElement voption) (newChild: ViewElement) (targetChild: %s) =" p.UniqueName targetChildType
                for ap in p.AttachedProperties do
                    let hasApply = not (System.String.IsNullOrWhiteSpace(ap.ConvertModelToValue)) || not (System.String.IsNullOrWhiteSpace(ap.UpdateCode))
                    let attributeKey = getAttributeKey ap.CustomAttributeKey ap.UniqueName
                    
                    w.printfn "            let prev%sOpt = match prevChildOpt with ValueNone -> ValueNone | ValueSome prevChild -> prevChild.TryGetAttributeKeyed<%s>(%s)" ap.UniqueName ap.ModelType attributeKey
                    w.printfn "            let curr%sOpt = newChild.TryGetAttributeKeyed<%s>(%s)" ap.UniqueName ap.ModelType attributeKey
                    
                    if ap.ModelType = "ViewElement" && not hasApply then
                        w.printfn "            match prev%sOpt, curr%sOpt with" ap.UniqueName ap.UniqueName
                        w.printfn "            // For structured objects, dependsOn on reference equality"
                        w.printfn "            | ValueSome prevValue, ValueSome newValue when identical prevValue newValue -> ()"
                        w.printfn "            | ValueSome prevValue, ValueSome newValue when canReuseView prevValue newValue ->"
                        w.printfn "                newValue.UpdateIncremental(prevValue, (%s.Get%s(targetChild)))" data.FullName ap.Name
                        w.printfn "            | _, ValueSome newValue ->"
                        w.printfn "                %s.Set%s(targetChild, (newValue.Create() :?> %s))" data.FullName ap.Name ap.OriginalType
                        w.printfn "            | ValueSome _, ValueNone ->"
                        w.printfn "                %s.Set%s(targetChild, null)" data.FullName ap.Name
                        w.printfn "            | ValueNone, ValueNone -> ()"
                        
                    elif not (System.String.IsNullOrWhiteSpace(ap.UpdateCode)) then
                        w.printfn "            %s prev%sOpt curr%sOpt targetChild" ap.UniqueName ap.UniqueName ap.UpdateCode
                        
                    else
                        w.printfn "            match prev%sOpt, curr%sOpt with" ap.UniqueName ap.UniqueName
                        w.printfn "            | ValueSome prevChildValue, ValueSome currChildValue when prevChildValue = currChildValue -> ()"
                        w.printfn "            | _, ValueSome currChildValue -> targetChild.SetValue(%s.%sProperty, %s currChildValue)" data.FullName ap.Name ap.ConvertModelToValue
                        w.printfn "            | ValueSome _, ValueNone -> targetChild.ClearValue(%s.%sProperty)" data.FullName ap.Name
                        w.printfn "            | _ -> ()"
                        
                    w.printfn "            overrideFunc prevChildOpt newChild targetChild"
                    
            w.printfn ""
            for p in data.PropertiesWithAttachedProperties do
                let attributeKey = getAttributeKey p.CustomAttributeKey p.UniqueName
                w.printfn "        registry.[%s.KeyValue] <- (fun o p c t -> (update%sAttachedProperties o p c (unbox t)))(match registry.TryGetValue(%s.KeyValue) with true, func -> func | false, _ -> (fun _ _ _ -> ()))" attributeKey p.UniqueName attributeKey
            w.printfn ""
                        
        if (data.ImmediateMembers.Length = 0) then
            if (data.BaseName.IsNone) then
                w.printfn "        ()"
            else
                w.printfn "        ViewBuilders.Update%s (registry, prevOpt, curr, target)" data.BaseName.Value
        else
            if data.ImmediateMembers.Length > 0 then
                for m in data.ImmediateMembers do
                    w.printfn "        let mutable prev%sOpt = ValueNone" m.UniqueName
                    w.printfn "        let mutable curr%sOpt = ValueNone" m.UniqueName
                w.printfn "        for kvp in curr.AttributesKeyed do"
                for m in data.ImmediateMembers do
                    let attributeKey = getAttributeKey m.CustomAttributeKey m.UniqueName
                    w.printfn "            if kvp.Key = %s.KeyValue then " attributeKey
                    w.printfn "                curr%sOpt <- ValueSome (kvp.Value :?> %s)" m.UniqueName m.ModelType
                w.printfn "        match prevOpt with"
                w.printfn "        | ValueNone -> ()"
                w.printfn "        | ValueSome prev ->"
                w.printfn "            for kvp in prev.AttributesKeyed do"
                for m in data.ImmediateMembers do
                    let attributeKey = getAttributeKey m.CustomAttributeKey m.UniqueName
                    w.printfn "                if kvp.Key = %s.KeyValue then " attributeKey
                    w.printfn "                    prev%sOpt <- ValueSome (kvp.Value :?> %s)" m.UniqueName m.ModelType
            
            // Unsubscribe previous event handlers
            if data.Events.Length > 0 then
                w.printfn "        // Unsubscribe previous event handlers"
                for e in data.Events do
                    let relatedProperties =
                        e.RelatedProperties
                        |> Array.map (fun p -> sprintf "(identical prev%sOpt curr%sOpt)" p p)
                        |> Array.fold (fun a b -> a + " && " + b) ""

                    w.printfn "        let shouldUpdate%s = not ((identical prev%sOpt curr%sOpt)%s)" e.UniqueName e.UniqueName e.UniqueName relatedProperties
                    w.printfn "        if shouldUpdate%s then" e.UniqueName
                    w.printfn "            match prev%sOpt with" e.UniqueName
                    w.printfn "            | ValueSome prevValue -> target.%s.RemoveHandler(prevValue)" e.Name
                    w.printfn "            | ValueNone -> ()"

            // Update priority properties
            if data.PriorityProperties.Length > 0 then
                w.printfn "        // Update priority properties"
                generateProperties data.PriorityProperties

            // Update inherited members
            if data.BaseName.IsSome then
                w.printfn "        // Update inherited members"
                w.printfn "        ViewBuilders.Update%s (registry, prevOpt, curr, target)" data.BaseName.Value

            // Update properties
            if data.Properties.Length > 0 then
                w.printfn "        // Update properties"
                generateProperties data.Properties
            
            // Subscribe event handlers
            if data.Events.Length > 0 then
                w.printfn "        // Subscribe new event handlers"
                for e in data.Events do
                    w.printfn "        if shouldUpdate%s then" e.UniqueName
                    w.printfn "            match curr%sOpt with" e.UniqueName
                    w.printfn "            | ValueSome currValue -> target.%s.AddHandler(currValue)" e.Name
                    w.printfn "            | ValueNone -> ()"
                
        w.printfn ""
        w

    let generateConstruct (data: ConstructData option) (w: StringWriter) =
        match data with
        | None -> ()
        | Some data ->
            let memberNewLine = "\n                                  " + String.replicate data.Name.Length " " + " "
            let space = "\n                               "
            let membersForConstructor =
                data.Members
                |> Array.mapi (fun i m ->
                    let commaSpace = if i = 0 then "" else "," + memberNewLine
                    match m.Name with
                    | "created" -> sprintf "%s?%s: (%s -> unit)" commaSpace m.Name data.FullName
                    | "ref" ->     sprintf "%s?%s: ViewRef<%s>" commaSpace m.Name data.FullName
                    | _ ->         sprintf "%s?%s: %s" commaSpace m.Name m.InputType)
                |> Array.fold (+) ""
            let membersForBuild =
                data.Members
                |> Array.map (fun m ->
                    match m.Name with
                    | "created" -> sprintf ",%s?%s=(match %s with None -> None | Some createdFunc -> Some (fun (target: obj) ->  createdFunc (unbox<%s> target)))" space m.Name m.Name data.FullName
                    | "ref" ->     sprintf ",%s?%s=(match %s with None -> None | Some (ref: ViewRef<%s>) -> Some ref.Unbox)" space m.Name m.Name data.FullName
                    | _ ->         sprintf ",%s?%s=%s" space m.Name m.Name)
                |> Array.fold (+) ""

            w.printfn "    static member inline Construct%s(%s) = " data.Name membersForConstructor
            w.printfn ""
            w.printfn "        let attribBuilder = ViewBuilders.Build%s(0%s)" data.Name membersForBuild
            w.printfn ""
            w.printfn "        ViewElement.Create<%s>(ViewBuilders.Create%s, (fun registry prevOpt curr target -> ViewBuilders.Update%s(registry, prevOpt, curr, target)), attribBuilder, extraAttributes)" data.FullName data.Name data.Name
            w.printfn ""


    let generateBuilders (data: BuilderData array) (w: StringWriter) =
        w.printfn "type ViewBuilders() ="
        for typ in data do
            w
            |> generateBuildFunction typ.Build
            |> generateCreateFunction typ.Create
            |> generateUpdateFunction typ.Update
            |> generateConstruct typ.Construct
        w

    let generateViewers (data: ViewerData array) (w: StringWriter) =
        for typ in data do
            let genericConstraint =
                match typ.GenericConstraint with
                | None -> ""
                | Some constr -> sprintf "<%s>" constr
            
            w.printfn "/// Viewer that allows to read the properties of a ViewElement representing a %s" typ.Name
            w.printfn "type %s%s(element: ViewElement) =" typ.ViewerName genericConstraint

            match typ.InheritedViewerName with
            | None -> ()
            | Some inheritedViewerName ->
                let inheritedGenericConstraint =
                    match typ.InheritedGenericConstraint with
                    | None -> ""
                    | Some constr -> sprintf "<%s>" constr
                
                w.printfn "    inherit %s%s(element)" inheritedViewerName inheritedGenericConstraint

            w.printfn "    do if not ((typeof<%s>).IsAssignableFrom(element.TargetType)) then failwithf \"A ViewElement assignable to type '%s' is expected, but '%%s' was provided.\" element.TargetType.FullName" typ.FullName typ.FullName
            for m in typ.Members do
                match m.Name with
                | "Created" | "Ref" -> ()
                | _ ->
                    let attributeKey = getAttributeKey m.CustomAttributeKey m.UniqueName
                    w.printfn "    /// Get the value of the %s member" m.Name
                    w.printfn "    member this.%s = element.GetAttributeKeyed(%s)" m.Name attributeKey
            w.printfn ""
        w

    let generateConstructors (data: ConstructorData array) (w: StringWriter) =
        w.printfn "[<AbstractClass; Sealed>]"
        w.printfn "type View private () ="

        for d in data do
            let memberNewLine = "\n                         " + String.replicate d.Name.Length " " + " "
            let space = "\n                               "
            let membersForConstructor =
                d.Members
                |> Array.mapi (fun i m ->
                    let commaSpace = if i = 0 then "" else "," + memberNewLine
                    match m.Name with
                    | "created" -> sprintf "%s?%s: (%s -> unit)" commaSpace m.Name d.FullName
                    | "ref" ->     sprintf "%s?%s: ViewRef<%s>" commaSpace m.Name d.FullName
                    | _ ->         sprintf "%s?%s: %s" commaSpace m.Name m.InputType)
                |> Array.fold (+) ""
            let membersForConstruct =
                d.Members
                |> Array.mapi (fun i m ->
                    let commaSpace = if i = 0 then "" else "," + space
                    sprintf "%s?%s=%s" commaSpace m.Name m.Name)
                |> Array.fold (+) ""

            w.printfn "    /// Describes a %s in the view" d.Name
            w.printfn "    static member inline %s(%s) =" d.Name membersForConstructor
            w.printfn ""
            w.printfn "        ViewBuilders.Construct%s(%s)" d.Name membersForConstruct
            w.printfn ""
        w.printfn ""
        w
    
    let generateViewExtensions (data: ViewExtensionsData array) (w: StringWriter) : StringWriter =
        let newLine = "\n                                       "

        w.printfn "[<AutoOpen>]"
        w.printfn "module ViewElementExtensions = "
        w.printfn ""
        w.printfn "    type ViewElement with"

        for m in data do
            match m.UniqueName with
            | "Created" | "Ref" -> ()
            | _ ->
                let attributeKey = getAttributeKey m.CustomAttributeKey m.UniqueName
                w.printfn ""
                w.printfn "        /// Adjusts the %s property in the visual element" m.UniqueName
                w.printfn "        member x.%s(value: %s) = x.WithAttribute(%s, %s(value))" m.UniqueName m.InputType attributeKey m.ConvertInputToModel

        let members =
            data
            |> Array.filter (fun m -> m.UniqueName <> "Created" && m.UniqueName <> "Ref")
            |> Array.mapi (fun index m -> sprintf "%s%s?%s: %s" (if index > 0 then ", " else "") (if index > 0 && index % 5 = 0 then newLine else "") m.LowerUniqueName m.InputType)
            |> Array.fold (+) ""

        w.printfn ""
        w.printfn "        member inline viewElement.With(%s) =" members
        for m in data do
            match m.UniqueName with
            | "Created" | "Ref" -> ()
            | _ -> w.printfn "            let viewElement = match %s with None -> viewElement | Some opt -> viewElement.%s(opt)" m.LowerUniqueName m.UniqueName
        w.printfn "            viewElement"
        w.printfn ""

        for m in data do
            match m.UniqueName with
            | "Created" | "Ref" -> ()
            | _ ->
                w.printfn "    /// Adjusts the %s property in the visual element" m.UniqueName
                w.printfn "    let %s (value: %s) (x: ViewElement) = x.%s(value)" m.LowerUniqueName m.InputType m.UniqueName
        w
        
    let generate data =
        let toString (w: StringWriter) = w.ToString()
        use writer = new StringWriter()
        
        writer
        |> generateNamespace data.Namespace data.AdditionalNamespaces
        |> generateAttributes data.Attributes
        |> generateBuilders data.Builders
        |> generateViewers data.Viewers
        |> generateConstructors data.Constructors
        |> generateViewExtensions data.ViewExtensions
        |> toString

    let generateCode
        (prepareData: BoundModel -> GeneratorData)
        (generate: GeneratorData -> string)
        (bindings: BoundModel) : WorkflowResult<string> =
        
        bindings
        |> prepareData
        |> generate
        |> WorkflowResult.ok
