module Base64Processing

    open Fable.Core
    [<Global>]
    let console: JS.Console = jsNative

    let addPadding (text: string) =
        let paddingLength = text.Length + (4 - text.Length % 4) % 4

        text.PadRight(paddingLength, '=')

    let replaceString (oldChar: char) (newChar: char) (text: string) = text.Replace(oldChar, newChar)

    let convertFromBase64Url(base64Content: string) =
        base64Content
        |> replaceString '-' '+'
        |> replaceString '_' '/'
        |> addPadding

    let base64UrlDecode (base64Content: string) =
        base64Content
        |> convertFromBase64Url
        |> System.Convert.FromBase64String

    let base64UrlDecodeToString (base64Content: string) =
        base64Content
        |> base64UrlDecode
        |> System.Text.Encoding.UTF8.GetString
    
    let base64UrlDecodeToUint8Array (base64Content: string) =
        base64Content
        |> base64UrlDecode
        |> unbox<JS.Uint8Array>
