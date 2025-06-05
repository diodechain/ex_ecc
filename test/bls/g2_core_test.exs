defmodule ExEcc.BLS.G2CoreTest do
  use ExUnit.Case
  alias ExEcc.BLS.Ciphersuites.G2Basic
  alias ExEcc.BLS.G2Primitives
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve

  describe "key validation" do
    test "validates public keys correctly" do
      test_cases = [
        {G2Basic.sk_to_pk(42), true},
        {String.duplicate(<<17>>, 48), false}
      ]

      for {pubkey, success} <- test_cases do
        assert G2Basic.key_validate(pubkey) == success
      end
    end
  end

  describe "sign and verify" do
    test "signs and verifies messages correctly" do
      test_cases = [
        1,
        5,
        124,
        735,
        127_409_812_145,
        90_768_492_698_215_092_512_159
      ]

      for privkey <- test_cases do
        msg = to_string(privkey)
        pub = G2Basic.sk_to_pk(privkey)
        sig = G2Basic._core_sign(privkey, msg, G2Basic.dst())
        assert G2Basic._core_verify(pub, msg, sig, G2Basic.dst())
      end
    end
  end

  describe "aggregation" do
    test "aggregates signatures correctly" do
      test_cases = [
        {
          [Curve.multiply(Curve.g2(), 2), Curve.multiply(Curve.g2(), 3)],
          Curve.multiply(Curve.g2(), 2 + 3)
        },
        {
          [Curve.multiply(Curve.g2(), 42), Curve.multiply(Curve.g2(), 69)],
          Curve.multiply(Curve.g2(), 42 + 69)
        }
      ]

      for {signature_points, result_point} <- test_cases do
        signatures = Enum.map(signature_points, &G2Primitives.g2_to_signature/1)
        result_signature = G2Primitives.g2_to_signature(result_point)
        assert G2Basic.aggregate(signatures) == result_signature
      end
    end
  end

  describe "core aggregate verify" do
    test "verifies aggregated signatures correctly" do
      test_cases = [
        {1..5 |> Enum.to_list(), 1..5 |> Enum.to_list()}
      ]

      for {sks, messages} <- test_cases do
        pks = Enum.map(sks, &G2Basic.sk_to_pk/1)
        messages = Enum.map(messages, &to_string/1)

        signatures =
          Enum.zip(sks, messages)
          |> Enum.map(fn {sk, msg} -> G2Basic._core_sign(sk, msg, G2Basic.dst()) end)

        aggregate_signature = G2Basic.aggregate(signatures)
        assert G2Basic._core_aggregate_verify(pks, messages, aggregate_signature, G2Basic.dst())
      end
    end
  end
end
