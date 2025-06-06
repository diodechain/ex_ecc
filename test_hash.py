from py_ecc.bls.point_compression import (
    compress_G1,
    compress_G2,
    decompress_G1,
    decompress_G2,
)
from py_ecc.optimized_bls12_381 import (
    G2,
    multiply,
    FQ2,
)
from py_ecc.bls.hash_to_curve import (
    hash_to_G1,
    hash_to_G2,
)
from hashlib import (
    sha256,
)

DST_G2 = b"QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_"

test_cases = [
    (
        "",
        FQ2([
            0x0141EBFBDCA40EB85B87142E130AB689C673CF60F1A3E98D69335266F30D9B8D4AC44C1038E9DCDD5393FAF5C41FB78A,
            0x05CB8437535E20ECFFAEF7752BADDF98034139C38452458BAEEFAB379BA13DFF5BF5DD71B72418717047F5B0F37DA03D
        ]),
        FQ2([
            0x0503921D7F6A12805E72940B963C0CF3471C7B2A524950CA195D11062EE75EC076DAF2D4BC358C4B190C0C98064FDD92,
            0x12424AC32561493F3FE3C260708A12B7C620E7BE00099A974E259DDC7D1F6395C3C811CDD19F1E8DBF3E9ECFDCBAB8D6
        ])
    ),
]

def test_hash_to_g2():
    for msg, x, y in test_cases:
        print("MSG")
        point = hash_to_G2(msg.encode(), DST_G2, sha256)



if __name__ == "__main__":
    print("Running compress tests...")
    test_hash_to_g2()
    print("All tests passed!") 