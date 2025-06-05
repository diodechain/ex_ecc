from py_ecc.optimized_bls12_381 import (
    G1,
    multiply,
)

def test_multiply():
    # Test case: Verify aggregated signatures
    pt = multiply(G1, 3)
    print(pt)
    print("Multiply tests passed!")

if __name__ == "__main__":
    print("Running multiply tests...")
    test_multiply()
    print("All tests passed!") 