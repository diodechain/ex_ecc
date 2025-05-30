defmodule ExEcc.Secp256k1Test do
  use ExUnit.Case
  alias ExEcc.Secp256k1

  # Test private key from Python tests
  @priv Base.decode16!("792eca682b890b31356247f2b04662bff448b6bb19ea1c8ab48da222c894ef9b", case: :lower)

  # Expected public key from Python tests
  @pub {
    20033694065814990006010338153307081985267967222430278129327181081381512401190,
    72089573118161052907088366229362685603474623289048716349537937839432544970413
  }

  describe "privtopub/1" do
    test "converts private key to public key" do
      assert Secp256k1.privtopub(@priv) == @pub
    end
  end

  describe "ecdsa_raw_sign/2 and ecdsa_raw_recover/2" do
    test "signs and recovers message" do
      message = :binary.copy(<<0x35>>, 32)
      {v, r, s} = Secp256k1.ecdsa_raw_sign(message, @priv)
      assert Secp256k1.ecdsa_raw_recover(message, {v, r, s}) == @pub
    end

    test "handles issue_4_bug case" do
      unsigned_message = "6a74f15f29c3227c5d1d2e27894da58d417a484ef53bc7aa57ee323b42ded656"
      v = 28
      r = String.to_integer("5897c2c7c7412b0a555fb6f053ddb6047c59666bbebc6f5573134e074992d841", 16)
      s = String.to_integer("1c71d1c62b74caff8695a186e2a24dd701070ba9946748318135e3ac0950b1d4", 16)

      # Just verify the function doesn't crash
      Secp256k1.ecdsa_raw_recover(unsigned_message, {v, r, s})
    end
  end
end
