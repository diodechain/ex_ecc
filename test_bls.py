from py_ecc.bls import G2Basic

def test_key_validation():
    # Test case 1: Valid public key
    sk = 42
    pk = G2Basic.SkToPk(sk)
    print("Python pubkey for sk=42 (hex):", pk.hex())
    print("Python pubkey for sk=42 (list):", list(pk))
    assert G2Basic.KeyValidate(pk) == True
    
    # Test case 2: Invalid public key (wrong length)
    invalid_pk = b'\x11' * 48
    assert G2Basic.KeyValidate(invalid_pk) == False
    
    print("Key validation tests passed!")

def test_sign_and_verify():
    test_cases = [1, 5, 124, 735, 127409812145, 90768492698215092512159]
    
    for privkey in test_cases:
        msg = str(privkey).encode()
        pub = G2Basic.SkToPk(privkey)
        sig = G2Basic.Sign(privkey, msg)
        assert G2Basic.Verify(pub, msg, sig)
    
    print("Sign and verify tests passed!")

def test_aggregation():
    # Test case: Aggregate signatures with unique messages
    sks = [2, 3]
    msgs = [b"msg1", b"msg2"]
    pubs = [G2Basic.SkToPk(sk) for sk in sks]
    sigs = [G2Basic.Sign(sk, msg) for sk, msg in zip(sks, msgs)]
    aggregate_sig = G2Basic.Aggregate(sigs)
    assert G2Basic.AggregateVerify(pubs, msgs, aggregate_sig)
    print("Aggregation tests passed!")

def test_aggregate_verify():
    # Test case: Verify aggregated signatures
    sks = list(range(1, 6))
    messages = [str(i).encode() for i in range(1, 6)]
    
    pks = [G2Basic.SkToPk(sk) for sk in sks]
    signatures = [G2Basic.Sign(sk, msg) for sk, msg in zip(sks, messages)]
    
    aggregate_signature = G2Basic.Aggregate(signatures)
    assert G2Basic.AggregateVerify(pks, messages, aggregate_signature)
    
    print("Aggregate verify tests passed!")

if __name__ == "__main__":
    print("Running BLS tests...")
    test_key_validation()
    test_sign_and_verify()
    test_aggregation()
    test_aggregate_verify()
    print("All tests passed!") 