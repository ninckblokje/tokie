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
