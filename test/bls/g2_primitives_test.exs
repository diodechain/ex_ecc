defmodule ExEcc.BLS.G2PrimitivesTest do
  use ExUnit.Case
  alias ExEcc.BLS.G2Primitives
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve

  test "decompress G2 with no modular square root found" do
    invalid_signature = <<0xA0>> <> :binary.copy(<<0x11>>, 95)
    assert_raise RuntimeError, ~r/Failed to find a modular squareroot/, fn ->
      G2Primitives.signature_to_g2(invalid_signature)
    end
  end

  test "G2 signature encode decode" do
    g2_point = Curve.multiply(Curve.g2(), 42)
    signature = G2Primitives.g2_to_signature(g2_point)
    assert Curve.normalize(G2Primitives.signature_to_g2(signature)) == Curve.normalize(g2_point)
  end

  test "G1 pubkey encode decode" do
    g1_point = Curve.multiply(Curve.g1(), 42)
    pubkey = G2Primitives.g1_to_pubkey(g1_point)
    assert Curve.normalize(G2Primitives.pubkey_to_g1(pubkey)) == Curve.normalize(g1_point)
  end
end
