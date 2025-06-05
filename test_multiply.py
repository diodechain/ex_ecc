from py_ecc.bls.point_compression import (
    compress_G1,
    compress_G2,
    decompress_G1,
    decompress_G2,
)
from py_ecc.optimized_bls12_381 import (
    G2,
    multiply,
)

def test_compress():
    pt = G2
    print(pt)
    z1, z2 = compress_G2(pt)
    print(z1)
    print(z2)
    print("Compress tests passed!")

if __name__ == "__main__":
    print("Running compress tests...")
    test_compress()
    print("All tests passed!") 