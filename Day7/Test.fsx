#r "bin/Debug/Library.dll"

open Advent.Library

#load "Program.fsx"

open Program
open System

test findTLS [| "abba[mnop]qrst" //supports TLS (abba outside square brackets).
                "abcd[bddb]xyyx" //does not support TLS (bddb is within square brackets, even though xyyx is outside square brackets).
                "aaaa[qwer]tyui" //does not support TLS (aaaa is invalid; the interior characters must be different).
                "ioxxoj[asdfgh]zxcvbn" //supports TLS (oxxo is outside square brackets, even though it's within a larger string).
                "ioxxoj[abba]zxcvbn" |] //Doesn't support TLS
|> is [| TLS; NotTLS; NotTLS; TLS; NotTLS |]

test findSSL [| 
    "aba[bab]xyz"   supports SSL (aba outside square brackets with corresponding bab within square brackets).
    "xyx[xyx]xyx"   does not support SSL (xyx, but no corresponding yxy).
    "aaa[kek]eke"   supports SSL (eke in supernet with corresponding kek in hypernet; the aaa sequence is not related, because the interior character must be different).
    "zazbz[bzb]cdb" supports SSL (zaz has no corresponding aza, but zbz has a corresponding bzb, even though zaz and zbz overlap).
 |]
|> is [| SSL; NotSSL; SSL;SSL |]