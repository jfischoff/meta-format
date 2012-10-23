# Think of it as JSON+

JSON is a great human readable, succinct, simple serialization format. I love JSON. I just want more. I want to represent graphs and I want pngs, mp3s, basically I also want specialized binary formats. 

## Introducing Data.Archive.Meta.Value 

Let's start by looking at the declaration for Value. It is basically an embellished (stolen) Data.Aeson.Value 

```haskell
    data Value = Object              !Object 
               | Array               !Array  
               | String              !Text   
               | Number              !Number 
               | Bool                !Bool   
               | Null                 
               | InternalReference   !Text   
               | ExternalReference   { 
                    _referenceType :: !Text,
                    _ref           :: !Text 
                }
```
                 
In addition to the typically JSON constructors there are a few additions. Mainly the InternalReference, and ExternalReference. The InternalReference takes a Text which serves as a path into the parent Value. The ExternalReference takes a type (think file type) and a path.

The Value datatype includes instances of ToJSON and FromJSON that are pretty straightforward. 

The Value type used by the Context type

```haskell
    data Context = Context {
            _value :: Value,
            _files :: [File]
        }
```    
        
Which include a Value "manifest" and a list of external files where File is

```haskell
    data File = File {
            _filePath  :: Text,
            _fileBytes :: ByteString 
        }
```
        
When serializing the Context it can be convenient to make tarball of the manifest and the files. Support is provided for this through Context instance of the ToTar class. 

To make a tarball of Context like so

```haskell
    toTar x
```

Also included is ToContext class. Provide this and get the tarball serialization for "free".







