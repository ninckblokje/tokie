module Token

  module TokenTypes =

    type SplitToken = string []

    type JOSEHeader =
        { typ: string
          cty: string
          alg: string
          iss: string
          sub: string
          aud: string }

    type TokenBase64 =
        { Header: string option
          Payload: string option
          Signature: string option }

    type Token =
        { Header: JOSEHeader option
          Payload: string option
          Base64: TokenBase64
          Valid: bool
          Reason: string option }

    type ValidatedSplitToken =
        { TokenParts: SplitToken
          Length: int
          Valid: bool
          Reason: string option }
  
  module TokenOperations = 

    open Base64Processing
    open JsonProcessing
    open TokenTypes

    open Fable.Core
    [<Global>]
    let console: JS.Console = jsNative
    
    let buildToken (validatedSplitToken: ValidatedSplitToken) =
      let headerBase64 = 
          match validatedSplitToken.Valid with
          | true -> validatedSplitToken.TokenParts.[0] |> Some
          | false -> None
      
      let header =
          match validatedSplitToken.Valid && headerBase64.IsSome with
          | true -> headerBase64.Value |> base64UrlDecodeToString |> deserializeJson |> Some
          | false -> None
      
      let payloadBase64 =
          match validatedSplitToken.Valid with
          | true -> validatedSplitToken.TokenParts.[1] |> Some
          | false -> None
      
      let payload =
          match validatedSplitToken.Valid && payloadBase64.IsSome with
          | true -> payloadBase64.Value |> base64UrlDecodeToString |> Some
          | false -> None
      
      let signatureBase64 =
          match validatedSplitToken.Valid && validatedSplitToken.Length = 3 with
          | true -> validatedSplitToken.TokenParts.[2] |> Some
          | false -> None

      { Header = header
        Payload = payload
        Base64 = { Header = headerBase64
                   Payload = payloadBase64
                   Signature = signatureBase64 }
        Valid = validatedSplitToken.Valid
        Reason = validatedSplitToken.Reason }
    
    let copyToken (valid: bool) (reason: string option) (token: Token) =
      { Header = token.Header
        Payload = token.Payload
        Base64 = token.Base64
        Valid = valid
        Reason = reason }

    let splitToken (rawToken: string) = rawToken.Split('.')
