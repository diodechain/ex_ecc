defmodule ExEcc.BLS.Ciphersuites.G2BasicTest do
  use ExUnit.Case
  alias ExEcc.BLS.Ciphersuites.G2Basic
  alias ExEcc.BLS.Ciphersuites.Base.ValidationError
  alias ExEcc.BLS.G2Primitives
  alias ExEcc.OptimizedBLS12381
  alias ExEcc.OptimizedBLS12381.OptimizedCurve

  # Constants from Python test
  @z1_pubkey G2Primitives.g1_to_pubkey(OptimizedCurve.z1())
  @z2_signature G2Primitives.g2_to_signature(OptimizedCurve.z2())
  @sample_message "helloworld"

  describe "aggregate_verify/3" do
    test "verifies valid aggregated signatures" do
      sks = Enum.to_list(1..10)
      messages = Enum.to_list(1..10)
      pks = Enum.map(sks, &G2Basic.sk_to_pk/1)
      messages = Enum.map(messages, &to_string/1)
      signatures = Enum.zip(sks, messages) |> Enum.map(fn {sk, msg} -> G2Basic.sign(sk, msg) end)
      aggregate_signature = G2Basic.aggregate(signatures)
      assert G2Basic.aggregate_verify(pks, messages, aggregate_signature)
    end

    test "fails with duplicate messages" do
      sks = Enum.to_list(1..3)
      messages = ["42", "69", "42"]
      pks = Enum.map(sks, &G2Basic.sk_to_pk/1)
      signatures = Enum.zip(sks, messages) |> Enum.map(fn {sk, msg} -> G2Basic.sign(sk, msg) end)
      aggregate_signature = G2Basic.aggregate(signatures)
      refute G2Basic.aggregate_verify(pks, messages, aggregate_signature)
    end
  end

  describe "sk_to_pk/1" do
    test "converts valid private key to public key" do
      assert {:ok, _} = G2Basic.sk_to_pk(1)
    end

    test "fails with invalid private key" do
      assert {:error, %ValidationError{}} = G2Basic.sk_to_pk(0)
      assert {:error, %ValidationError{}} = G2Basic.sk_to_pk("hello")
    end
  end

  describe "sign/2" do
    test "signs message with valid private key" do
      assert {:ok, _} = G2Basic.sign(1, @sample_message)
    end

    test "fails with invalid inputs" do
      assert {:error, %ValidationError{}} = G2Basic.sign(0, @sample_message)
      assert {:error, %ValidationError{}} = G2Basic.sign("hello", @sample_message)
      assert {:error, %ValidationError{}} = G2Basic.sign(1, 123)
    end
  end

  describe "aggregate/1" do
    test "aggregates valid signatures" do
      sig1 = G2Basic.sign(1, @sample_message)
      sig2 = G2Basic.sign(2, @sample_message)
      assert {:ok, _} = G2Basic.aggregate([sig1])
      assert {:ok, _} = G2Basic.aggregate([sig1, sig2])
      assert {:ok, _} = G2Basic.aggregate([@z2_signature])
    end

    test "fails with invalid inputs" do
      assert {:error, %ValidationError{}} = G2Basic.aggregate(["hello"])
      assert {:error, %ValidationError{}} = G2Basic.aggregate([])
    end
  end

  describe "verify/3" do
    test "verifies valid signature" do
      pk = G2Basic.sk_to_pk(1)
      signature = G2Basic.sign(1, @sample_message)
      assert G2Basic.verify(pk, @sample_message, signature)
    end

    test "fails with mismatched key/signature" do
      pk = G2Basic.sk_to_pk(2)
      signature = G2Basic.sign(1, @sample_message)
      refute G2Basic.verify(pk, @sample_message, signature)
    end

    test "fails with zero signature" do
      pk = G2Basic.sk_to_pk(1)
      refute G2Basic.verify(pk, @sample_message, @z2_signature)
    end

    test "fails with zero public key" do
      signature = G2Basic.sign(1, @sample_message)
      refute G2Basic.verify(@z1_pubkey, @sample_message, signature)
      refute G2Basic.verify(@z1_pubkey, @sample_message, @z2_signature)
    end

    test "fails with invalid public key" do
      invalid_pk =
        <<64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>

      invalid_sig =
        <<64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
          0, 0, 0, 0, 0, 0, 0, 0, 0>>

      refute G2Basic.verify(invalid_pk, @sample_message, invalid_sig)
    end
  end
end
