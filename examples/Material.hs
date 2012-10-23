module Material where
--Example

--Consider a game where you have a Material type. 
--The Material type has shaders and the images that are used 
--by the shaders, and any uniforms that the shaders need.
    
data Material = Material {
        _vertexShader   :: Text,
        _fragmentShader :: Text,
        _images         :: [(Text, Image)]
        _uniforms       :: [Uniform]
}

    