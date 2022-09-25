(*
  Copyright (c) 2022, ninckblokje
  All rights reserved.
  
  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:
  
  * Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.
  
  * Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.
  
  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
  DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
  FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
  DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
  CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
  OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

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
