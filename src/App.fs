module App

open Browser.Dom
open Fable.Core
open Fable.Import

open JsonProcessing
open Token.TokenOperations
open Token.TokenTypes
open WebCrypto

[<Global>]
let console: JS.Console = jsNative

let jwkTextarea =
    document.querySelector (".jwk-textarea") :?> Browser.Types.HTMLTextAreaElement

let outputTextarea =
    document.querySelector (".output-textarea") :?> Browser.Types.HTMLTextAreaElement

let tokenTextarea =
    document.querySelector (".token-textarea") :?> Browser.Types.HTMLTextAreaElement

let validTextarea =
    document.querySelector (".valid-textarea") :?> Browser.Types.HTMLTextAreaElement

let resetButton =
    document.querySelector (".reset-button") :?> Browser.Types.HTMLButtonElement

let validateButton =
    document.querySelector (".validate-button") :?> Browser.Types.HTMLButtonElement

let parseJWK (rawJWK: string) =
    deserializeJson rawJWK

let validateSplitToken (splitToken: SplitToken) =
    let valid = splitToken.Length > 1

    { TokenParts = splitToken
      Length = splitToken.Length
      Valid = valid
      Reason =
        if valid then
            None
        else
            Some "Token should at least have 2 parts" }

let validateValidToken(token: Token) =
    let validAlg =
        match token.Header.Value.alg with
        | "none" -> true
        | "RS256" -> true
        | _ -> false
    let validTyp =
        match token.Header.Value.typ with
        | "JWT" -> true
        | _ -> false

    let reason =
        if validAlg
        then
            if validTyp
            then
                None
            else
                Some(sprintf "typ %s not supported" token.Header.Value.typ)
        else
            Some(sprintf "alg %s not supported" token.Header.Value.alg)
    
    copyToken (validAlg && validTyp) reason token

let validateToken(token: Token) =
    if token.Valid && token.Header.IsSome
    then
        validateValidToken token
    else
        token

let validateValidTokenSignature(rawJWK: string) (token: Token) : JS.Promise<Token> =
    try
        let jwk = parseJWK rawJWK

        match token.Header.Value.alg with
        | "none" -> promise { return token }
        | "RS256" -> validateRS256Signature token jwk
        | _ -> promise { return copyToken false (Some "Signature is not valid") token }
    with
    | exn ->
        console.error exn
        console.error exn.StackTrace
        promise { return copyToken false (Some "Error validating signature or parsing key") token }

let validateTokenSignature (rawJWK: string) (token: Token) =
    if token.Valid && token.Header.IsSome && rawJWK.Trim() <> ""
    then
        validateValidTokenSignature rawJWK token
    else
        promise { return token }
    
let parseToken (rawToken: string) : Token =
    let output =
        try
            rawToken
            |> splitToken
            |> validateSplitToken
            |> buildToken
        with
        | exn ->
            console.error exn
            console.error exn.StackTrace
            { Header = None
              Payload = None
              Base64 = { Header = None
                         Payload = None
                         Signature = None }
              Valid = false
              Reason = Some exn.Message }

    output

let printToken (token: Token) =
    let stringHeader = if token.Header.IsSome then serializeJson token.Header.Value else ""
    let stringPayload = if token.Payload.IsSome then token.Payload.Value else ""

    sprintf "%s.%s" stringHeader stringPayload

let updateTextareas token =
    outputTextarea.value <- printToken token
    validTextarea.value <- if token.Valid then "" else token.Reason.Value

resetButton.onclick <-
    fun _ ->
        tokenTextarea.value <- ""
        jwkTextarea.value <- ""
        outputTextarea.value <- ""
        validTextarea.value <- ""

validateButton.onclick <-
    fun _ ->
        let rawJWK = jwkTextarea.value
        tokenTextarea.value
        |> parseToken
        |> validateToken
        |> validateTokenSignature rawJWK
        |> Promise.map updateTextareas
