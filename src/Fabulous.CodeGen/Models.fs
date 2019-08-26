// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.CodeGen

module Models =
    type Logger =
        { traceInformation: string -> unit
          traceWarning: string -> unit
          traceError: string -> unit }
        
    type Configuration =
        { /// The base type full name from which all UI controls inherit from (e.g. Xamarin.Forms.Element)
          baseTypeName: string
          
          /// The type full name of the object used for properties declared with a bindable/dependency property (e.g. Xamarin.Forms.BindableProperty)
          propertyBaseType: string
          
          /// The default type to which attached properties can be applied to (e.g. Xamarin.Forms.Element)
          baseTargetTypeForAttachedProperties: string }
    
    type Member() =
        /// Indicates the source property/event name as found by the Assembly Reader to include (and override if needed)
        /// If none is provided, the generator will consider it's a non-existent property and other fields will be mandatory
        member val Source : string option = None with get, set
        
        /// The name to use (e.g. ItemsSource => Items)
        member val Name : string option = None with get, set
        
        /// The unique identifier of the member to use inside the generated code (e.g. Text => ButtonText)
        /// If a Name is provided, the ShortName will be automatically determined from it
        member val UniqueName : string option = None with get, set
        
        /// The type which the user should provide for the member
        member val InputType : string option = None with get, set
        
        /// The type as which the member's value will be stored in the ViewElement
        /// If the InputType is an F# List, the ModelType will automatically set to an array
        member val ModelType : string option = None with get, set
        
        /// The function that converts the input value to the model type
        /// If the InputType is an F# List, the convert function will automatically be Array.ofList
        member val ConvertInputToModel : string option = None with get, set

    type IConstructorMember =
        /// The name of the member in the constructor, should be in a lower camel case (e.g. ItemsSource => items | View.ListView(items=...))
        /// If a Name is provided, the ShortName will be automatically determined from it
        abstract ShortName : string option

    type IProperty =
        /// The default value of the property when none is given by the user
        abstract DefaultValue : string option
        
        /// The type of the elements if the property is a collection
        abstract ElementType : string option
        
        /// The function that converts the model value to the actual type expected by the property
        abstract ConvertModelToValue : string option
        
        /// The function that will handle the update process for this property
        /// Completely replace the code generated for this property in Update*ControlType*
        abstract UpdateCode : string option

    type Property() =
        inherit Member()
        
        member val ShortName = None with get, set
        member val DefaultValue = None with get, set
        member val ElementType = None with get, set
        member val ConvertModelToValue = None with get, set
        member val UpdateCode = None with get, set
        
        interface IConstructorMember with
            member x.ShortName = x.ShortName
        interface IProperty with
            member x.DefaultValue = x.DefaultValue
            member x.ElementType = x.ElementType
            member x.ConvertModelToValue = x.ConvertModelToValue
            member x.UpdateCode = x.UpdateCode

    type Event() =
        inherit Member()
        
        member val ShortName = None with get, set
        
        /// The properties' name which are linked to this event (e.g. TextChanged => Text)
        member val RelatedProperties : string[] option = None with get, set
        
        interface IConstructorMember with
            member x.ShortName = x.ShortName
        
    type AttachedProperty() =
        inherit Member()
        
        /// The type to which this attached property will be applied to
        member val TargetType : string option = None with get, set
        member val DefaultValue = None with get, set
        member val ElementType = None with get, set
        member val ConvertModelToValue = None with get, set
        member val UpdateCode = None with get, set
        
        interface IProperty with
            member x.DefaultValue = x.DefaultValue
            member x.ElementType = x.ElementType
            member x.ConvertModelToValue = x.ConvertModelToValue
            member x.UpdateCode = x.UpdateCode
    
    type Type() =
        /// The full name of the type to include (e.g. Xamarin.Forms.Button)
        member val Type : string = "" with get, set
        
        /// The full name of the type to instantiate (e.g. Fabulous.XamarinForms.CustomButton)
        member val CustomType : string option = None with get, set

        /// Indicates if this type can be instantiated by itself and if the generator should provide a public constructor for it
        member val CanBeInstantiated : bool = true with get, set

        /// The name of the type as used inside Fabulous (e.g. MyWonderfulButton => View.MyWonderfulButton(...))
        member val Name : string option = None with get, set
        
        /// The events to include/create/override for this type
        member val Events : Event[] option = None with get, set
        
        /// The properties to include/create/override for this type
        member val Properties : Property[] option = None with get, set
        
        /// The attached properties to include/create/override for this type
        member val AttachedProperties : AttachedProperty[] option = None with get, set
        
        /// An ordered list of all direct members (events and properties, not attached properties) to determine the order of the constructor (e.g. ["Text", "TextChanged", "Font"] => View.Entry(text=..., textChanged=..., font=...)
        /// Values must be Name
        /// Can order inherited members
        member val ConstructorMemberOrdering : string[] option = None with get, set
    
    type OverwriteData() =
        /// Assemblies to read (can be relative paths to dlls)
        member val Assemblies : string[] = [||] with get, set
        
        /// The namespace under which all the generated code will be put
        member val OutputNamespace : string = "" with get, set
        
        /// The types to include in the generated code
        member val Types : Type[] = [||] with get, set