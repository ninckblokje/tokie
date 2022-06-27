export function convertRS256JWK(jwk) {
    return crypto.subtle.importKey(
        "jwk",
        jwk,
        {
            name: "RSASSA-PKCS1-v1_5",
            hash: {name: "SHA-256"},
        },
        false,
        ["verify"]
    );
}

export function verifyRS256Signature(key, signature, data) {
    return crypto.subtle.verify(
            {
                name: "RSASSA-PKCS1-v1_5",
                hash: { name: "SHA-256" }
            },
            key,
            signature,
            new TextEncoder().encode(data)
        )
}
