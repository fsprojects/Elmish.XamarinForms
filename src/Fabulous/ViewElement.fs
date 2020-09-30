// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous

#nowarn "67" // cast always holds

open System
open System.Collections.Generic
open System.Diagnostics

[<AutoOpen>]
module internal AttributeKeys = 
    let attribKeys = Dictionary<string,int>()
    let attribNames = Dictionary<int,string>()

[<Struct>]
type AttributeKey<'T> internal (keyv: int) = 

    static let getAttribKeyValue (attribName: string) : int = 
        match attribKeys.TryGetValue(attribName) with 
        | true, keyv -> keyv
        | false, _ -> 
            let keyv = attribKeys.Count + 1
            attribKeys.[attribName] <- keyv
            attribNames.[keyv] <- attribName
            keyv

    new (keyName: string) = AttributeKey<'T>(getAttribKeyValue keyName)

    member __.KeyValue = keyv

    member __.Name = AttributeKey<'T>.GetName(keyv)

    static member GetName(keyv: int) = 
        match attribNames.TryGetValue(keyv) with 
        | true, keyv -> keyv
        | false, _ -> failwithf "unregistered attribute key %d" keyv


/// A description of a visual element
type AttributesBuilder (attribCount: int, extraCount: int) = 
    
    let mutable count = extraCount
    let mutable attribs = Array.zeroCreate<KeyValuePair<int, obj>>(attribCount)

    new (attribCount) = AttributesBuilder(attribCount, 0)

    /// Get the attributes of the visual element
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member __.Attributes = 
        if isNull attribs then [| |] 
        else attribs |> Array.map (fun kvp -> KeyValuePair(AttributeKey<int>.GetName kvp.Key, kvp.Value))

    /// Get the attributes of the visual element
    member __.Close() : _[] = 
        let res = attribs 
        attribs <- null
        res

    /// Produce a new visual element with an adjusted attribute
    member __.Add(key: AttributeKey<'T>, value: 'T) = 
        if isNull attribs then failwithf "The attribute builder has already been closed"
        if count >= attribs.Length then failwithf "The attribute builder was not large enough for the added attributes, it was given size %d. Did you get the attribute count right?" attribs.Length
        attribs.[count] <- KeyValuePair(key.KeyValue, box value)
        count <- count + 1


type ViewRef() = 
    let handle = System.WeakReference<obj>(null)
    
    let valueChanged = Event<obj>()
    
    member __.ValueChanged = valueChanged.Publish

    member __.Set(target: obj) : unit = 
        handle.SetTarget(target)
        valueChanged.Trigger(target) 

    member __.Unset() : unit = 
        handle.SetTarget(null)
        valueChanged.Trigger(null) 

    member __.TryValue = 
        match handle.TryGetTarget() with 
        | true, null -> None
        | true, res -> Some res 
        | _ -> None

type ViewRef<'T when 'T : not struct>() = 
    let handle = ViewRef()
    
    let valueChanged = Event<'T>()
    
    do handle.ValueChanged.Add(fun value ->
        valueChanged.Trigger(unbox value)
    )
    
    member __.ValueChanged = valueChanged.Publish

    member __.Set(target: 'T) : unit =
        handle.Set(box target)
        
    member __.Value : 'T = 
        match handle.TryValue with 
        | Some res -> unbox res
        | None -> failwith "view reference target has been collected or was not set"

    member __.Unbox = handle

    member __.TryValue : 'T option = 
        match handle.TryValue with 
        | Some res -> Some (unbox res)
        | _ -> None

/// A description of a visual element
type ViewElement internal (targetType: Type, create: (unit -> obj), update: (ViewElement voption -> ViewElement -> obj -> unit), attribs: KeyValuePair<int,obj>[]) = 
    
    let extraAttribsCounter = ref 0

    new (targetType: Type, create: (unit -> obj), update: (ViewElement voption -> ViewElement -> obj -> unit), attribsBuilder: AttributesBuilder) =
        ViewElement(targetType, create, update, attribsBuilder.Close())
    
    static member Create
            (create: (unit -> 'T),
             update: (Dictionary<int, ViewElement voption -> ViewElement -> obj -> unit> -> ViewElement voption -> ViewElement -> 'T -> unit),
             attribsBuilder: AttributesBuilder) =
        
        ViewElement(
            typeof<'T>, (create >> box),
            (fun prev curr target -> update (Dictionary()) prev curr (unbox target)),
            attribsBuilder.Close()
        )

    static member Create
            (create: (unit -> 'T),
             update: (Dictionary<int, ViewElement voption -> ViewElement -> obj -> unit> -> ViewElement voption -> ViewElement -> 'T -> unit),
             attribsBuilder: AttributesBuilder,
             extraAttributes: (ViewElement -> ViewElement) list option) =
        let viewElement =
            ViewElement(
                typeof<'T>, (create >> box),
                (fun prev curr target -> update (Dictionary()) prev curr (unbox target)),
                attribsBuilder.Close()
            )
        
        match extraAttributes with
        | Some extraAttributes when extraAttributes.Length > 0 ->
            viewElement.WithExtraAttributes(extraAttributes)
        | _ ->
            viewElement

    static member val CreatedAttribKey : AttributeKey<obj -> unit> = AttributeKey<_>("Created")
    static member val RefAttribKey : AttributeKey<ViewRef> = AttributeKey<_>("Ref")
    static member val KeyAttribKey : AttributeKey<string> = AttributeKey<_>("Key")

    /// Get the type created by the visual element
    member x.TargetType = targetType

    /// Get the attributes of the visual element
    [<DebuggerBrowsable(DebuggerBrowsableState.Never)>]
    member x.AttributesKeyed = attribs

    /// Get the attributes of the visual element
    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Attributes = attribs |> Array.map (fun kvp -> KeyValuePair(AttributeKey<int>.GetName kvp.Key, kvp.Value))
    

    /// Get an attribute of the visual element
    member x.TryGetAttributeKeyed<'T>(key: AttributeKey<'T>) = 
        match attribs |> Array.tryFind (fun kvp -> kvp.Key = key.KeyValue) with 
        | Some kvp -> ValueSome(unbox<'T>(kvp.Value)) 
        | None -> ValueNone

    /// Get an attribute of the visual element
    member x.TryGetAttribute<'T>(name: string) = 
        x.TryGetAttributeKeyed<'T>(AttributeKey<'T> name)
 
    /// Get an attribute of the visual element
    member x.GetAttributeKeyed<'T>(key: AttributeKey<'T>) = 
        match attribs |> Array.tryFind (fun kvp -> kvp.Key = key.KeyValue) with 
        | Some kvp -> unbox<'T>(kvp.Value)
        | None -> failwithf "Property '%s' does not exist on %s" key.Name x.TargetType.Name
        
    /// Try get the key attribute value
    member x.TryGetKey() = x.TryGetAttributeKeyed(ViewElement.KeyAttribKey)

    /// Apply initial settings to a freshly created visual element
    member x.Update (target: obj) = update ValueNone x target

    /// Differentially update a visual element given the previous settings
    member x.UpdateIncremental(prev: ViewElement, target: obj) = update (ValueSome prev) x target

    /// Differentially update the inherited attributes of a visual element given the previous settings
    member x.UpdateInherited(prevOpt: ViewElement voption, curr: ViewElement, target: obj) = update prevOpt curr target

    /// Create the UI element from the view description
    member x.Create() : obj =
        Debug.WriteLine (sprintf "Create %O" x.TargetType)
        let target = create()
        x.Update(target)
        match x.TryGetAttributeKeyed(ViewElement.CreatedAttribKey) with
        | ValueSome f -> f target
        | ValueNone -> ()
        match x.TryGetAttributeKeyed(ViewElement.RefAttribKey) with
        | ValueSome f -> f.Set (box target)
        | ValueNone -> ()
        target

    /// Produce a new visual element with an adjusted attribute
    member x.WithAttribute(key: AttributeKey<'T>, value: 'T) =
        let duplicateViewElement newAttribsLength attribIndex =
            let attribs2 = Array.zeroCreate newAttribsLength
            Array.blit attribs 0 attribs2 0 attribs.Length
            attribs2.[attribIndex] <- KeyValuePair(key.KeyValue, box value)
            ViewElement(targetType, create, update, attribs2)
        
        let n = attribs.Length
        
        let existingAttrIndexOpt = attribs |> Array.tryFindIndex (fun attr -> attr.Key = key.KeyValue)
        match existingAttrIndexOpt with
        | Some i ->
            duplicateViewElement n i // duplicate and replace existing attribute
        | None ->
            if attribs.[!extraAttribsCounter] = Unchecked.defaultof<KeyValuePair<_,_>> then
              attribs.[!extraAttribsCounter] <- KeyValuePair(key.KeyValue, box value)
              incr extraAttribsCounter
              x
            else          
              duplicateViewElement (n + 1) n // duplicate and add new attribute

    member x.WithExtraAttributes(extraAttributes: (ViewElement -> ViewElement) list) =
        if extraAttributes.Length > 0
        then extraAttributes |> Seq.map (fun apply -> apply x) |> Seq.last
        else x
    
    override x.ToString() = sprintf "%s(...)@%d" x.TargetType.Name (x.GetHashCode())



        