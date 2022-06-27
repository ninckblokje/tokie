module WebCrypto
    
    open Fable.Core

    open Base64Processing
    open Token.TokenOperations
    open Token.TokenTypes
    
    [<Global>]
    let console: JS.Console = jsNative

    type CryptoKey = obj

    type X5C = string

    type JWK =
        { kty: string
          ``use``: string
          key_ops: string
          alg: string
          kid: string
          x5u: string
          x5c: X5C []
          x5t: string
          ``x5t#S256``: string }

    type WebCryptoBridge =
        abstract convertRS256JWK: jwk: JWK -> JS.Promise<obj>
        abstract verifyRS256Signature: cryptoKey: CryptoKey -> signature: JS.ArrayBuffer -> data: string -> JS.Promise<obj>

    [<ImportAll("./WebCryptoBridge.js")>]
    let webCryptoBridge: WebCryptoBridge = jsNative

    let validateRS256Signature(token: Token) (jwk: JWK) : JS.Promise<Token> =
        
        let getData (token) =
            sprintf "%s.%s" token.Base64.Header.Value token.Base64.Payload.Value
        
        let getSignature(token: Token) =
            (token.Base64.Signature.Value |> base64UrlDecode |> unbox<JS.Uint8Array>).buffer

        let handleError(err) =
            console.log err
            copyToken false (Some "Signature verification failed") token
        
        let processResult(result: bool) =
            match result with
            | true -> token
            | false -> copyToken false (Some "Signature rejected") token
        
        let verify(cryptoKey: CryptoKey) =
            webCryptoBridge.verifyRS256Signature cryptoKey (getSignature token) (getData token)

        promise {
            return! webCryptoBridge.convertRS256JWK jwk
                            |> Promise.map unbox<CryptoKey>
                            |> Promise.map verify
                            |> Promise.map unbox<bool>
                            |> Promise.map processResult
                            |> Promise.catch handleError
        }
