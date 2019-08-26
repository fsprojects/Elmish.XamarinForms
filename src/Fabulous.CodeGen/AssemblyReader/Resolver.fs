namespace Fabulous.CodeGen.AssemblyReader

open Fabulous.CodeGen.Helpers
open Mono.Cecil

module Resolver =
    /// Extracts all types from the given assemblies
    let getAllTypesFromAssemblies (assemblies: AssemblyDefinition array) =
        [ for assembly in assemblies do
            for ``module`` in assembly.Modules do
                for ``type`` in ``module``.Types do
                    yield ``type`` ]

    let rec private getAllTypesDerivingFromBaseTypeInner (allTypes: Mono.Cecil.TypeDefinition list) baseTypeName =
        match allTypes |> List.tryFind (fun tdef -> tdef.FullName = baseTypeName) with
        | None -> []
        | Some baseType ->
            let typesToCheck = filterTypesDerivingFromBaseType allTypes baseTypeName
            let foundTypes = findAllDerivingTypes allTypes typesToCheck []
            foundTypes @ [ baseType ]
        
    and private filterTypesDerivingFromBaseType (allTypes: Mono.Cecil.TypeDefinition list) baseTypeName =
        allTypes
        |> List.filter (fun tdef ->
            match tdef.BaseType with
            | null -> false
            | :? GenericInstanceType as git -> git.ElementType.FullName = baseTypeName
            | baseType -> baseType.FullName = baseTypeName
        )
        
    and private findAllDerivingTypes (allTypes: Mono.Cecil.TypeDefinition list) (typesToCheck: Mono.Cecil.TypeDefinition list) (matchingTypes: Mono.Cecil.TypeDefinition list) =
        match typesToCheck with
        | [] -> matchingTypes
        | tdef::remainingTypesToCheck ->
            let derivingTypes = getAllTypesDerivingFromBaseTypeInner allTypes tdef.FullName
            let newMatchingTypes = List.concat [ derivingTypes; matchingTypes ]
            findAllDerivingTypes allTypes remainingTypesToCheck newMatchingTypes
    
    /// Finds all types that derive from a given base type (on all depth levels)
    let getAllTypesDerivingFromBaseType (isTypeResolvable: string -> bool) (allTypes: TypeDefinition list) baseTypeName =
        getAllTypesDerivingFromBaseTypeInner allTypes baseTypeName
        |> List.filter (fun tdef -> tdef.FullName |> isTypeResolvable)
        |> List.rev
        |> List.toArray
        
    /// Finds all attached properties for a given type
    let getAllAttachedPropertiesForType propertyBaseType (``type``: TypeDefinition) =
        if not ``type``.HasFields then
            [||]
        else
            ``type``.Fields
            |> Seq.filter (fun fdef -> fdef.IsStatic && fdef.FieldType.FullName = propertyBaseType && fdef.Name.EndsWith("Property"))
            |> Seq.filter (fun fdef -> ``type``.Properties |> Seq.exists (fun pdef -> pdef.Name = (fdef.Name |> Text.removeText "Property")) |> not)
            |> Seq.toArray
    
    /// Finds all not settable list properties for a given type
    let getAllListPropertiesWithNoSetterForType (``type``: TypeDefinition) =
        if not ``type``.HasProperties then
            [||]
        else
            ``type``.Properties
            |> Seq.filter (fun pdef -> pdef.GetMethod <> null && pdef.GetMethod.IsPublic && pdef.SetMethod = null)
            |> Seq.filter (fun pdef -> pdef.PropertyType.GetElementType().FullName = "System.Collections.Generic.IList`1")
            |> Seq.toArray

    /// Finds all settable properties for a given type
    let getAllPropertiesWithBindingFieldForType propertyBaseType (``type``: TypeDefinition) =
        if not ``type``.HasProperties then
            [||]
        else
            ``type``.Fields
            |> Seq.filter (fun fdef -> fdef.IsStatic && fdef.FieldType.FullName = propertyBaseType && fdef.Name.EndsWith("Property"))
            |> Seq.filter (fun fdef -> ``type``.Properties |> Seq.exists (fun pdef -> pdef.Name = (fdef.Name |> Text.removeText "Property")))
            |> Seq.toArray
        
    /// Finds all events for a given type
    let getAllEventsForType (``type``: TypeDefinition) =
        if not ``type``.HasEvents then
            [||]
        else
            ``type``.Events
            |> Seq.filter (fun edef -> edef.AddMethod.IsPublic && edef.RemoveMethod.IsPublic)
            |> Seq.toArray
            
    let rec private getHierarchyForTypeInner stopAtType (``type``: TypeDefinition) (state: string list) =
        match ``type``.BaseType with
        | null -> state
        | baseType ->
            let baseTypeDefinition = baseType.Resolve()
            let newState = baseTypeDefinition.FullName :: state
            
            if baseTypeDefinition.FullName = stopAtType then
                newState
            else
                getHierarchyForTypeInner stopAtType baseTypeDefinition newState
            
    /// Resolves all the inheritance hierarchy for a given type (Button -> View -> Element -> VisualElement)
    let getHierarchyForType stopAtType (``type``: TypeDefinition) =
        if ``type``.FullName = stopAtType then
            [||]
        else
            getHierarchyForTypeInner stopAtType ``type`` []
            |> List.rev
            |> List.toArray
            
    let getElementTypeForType (``type``: TypeDefinition) =
        if ``type``.HasGenericParameters then
            Some ``type``.GenericParameters.[0].FullName
        else
            None