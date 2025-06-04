defmodule ExEcc.BLS.Ciphersuites do
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.OptimizedBLS12381.OptimizedPairing, as: Pairing
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.BLS.HashToCurve
  alias ExEcc.BLS.G2Primitives
  alias ExEcc.BLS.Hash
  import While

  defmodule Base do
    @dst ""
    def dst(), do: @dst

    def xmd_hash_function(), do: ExEcc.BLS.Hash.sha256_function()

    #
    # Input validation helpers
    #
    def _is_valid_privkey(privkey) do
      is_integer(privkey) and privkey > 0 and privkey < Curve.curve_order()
    end

    def _is_valid_pubkey(pubkey) do
      is_binary(pubkey) and byte_size(pubkey) == 48
    end

    def _is_valid_message(message) do
      is_binary(message)
    end

    def _is_valid_signature(signature) do
      # SV: minimal-pubkey-size
      is_binary(signature) and byte_size(signature) == 96
    end

    @doc """
    The SkToPk algorithm takes a secret key SK and outputs the
    corresponding public key PK.

    Raise `ValidationError` when there is input validation error.
    """
    def sk_to_pk(cls, privkey) do
      if not cls._is_valid_privkey(privkey) do
        raise "Invalid private key"
      end

      G2Primitives.g1_to_pubkey(Curve.multiply(Curve.g1(), privkey))
    end

    def key_gen(cls, ikm, key_info \\ <<>>) do
      salt = "BLS-SIG-KEYGEN-SALT-"
      sk = 0

      reduce_while(sk, fn sk ->
        if sk == 0 do
          salt = cls.xmd_hash_function().fun.(salt)
          prk = Hash.hkdf_extract(salt, ikm <> <<0>>)
          # noqa: E741
          l = ceil(1.5 * ceil(:math.log2(Curve.curve_order())) / 8)
          okm = Hash.hkdf_expand(prk, key_info <> Hash.i2osp(l, 2), l)
          {:cont, rem(Hash.os2ip(okm), Curve.curve_order())}
        else
          {:halt, sk}
        end
      end)
    end

    def key_validate(pk) do
      pubkey_point =
        try do
          G2Primitives.pubkey_to_g1(pk)
        rescue
          _ -> false
        end

      cond do
        pubkey_point == false -> false
        Curve.is_inf(pubkey_point) -> false
        not G2Primitives.subgroup_check(pubkey_point) -> false
        true -> true
      end
    end

    @doc """
    The CoreSign algorithm computes a signature from SK, a secret key,
    and message, an octet string.

    Raise `ValidationError` when there is input validation error.
    """
    def _core_sign(cls, sk, message, dst) do
      cond do
        not cls._is_valid_privkey(sk) -> raise "Invalid secret key"
        not cls._is_valid_message(message) -> raise "Invalid message"
        true -> :ok
      end

      # Procedure
      message_point = HashToCurve.hash_to_g2(message, dst, cls.xmd_hash_function)
      signature_point = Curve.multiply(message_point, sk)
      G2Primitives.g2_to_signature(signature_point)
    end

    def _core_verify(cls, pk, message, signature, dst) do
      try do
        # Inputs validation
        cond do
          not cls._is_valid_pubkey(pk) -> raise "Invalid public key"
          not cls._is_valid_message(message) -> raise "Invalid message"
          not cls._is_valid_signature(signature) -> raise "Invalid signature"
          true -> :ok
        end

        # Procedure
        if not key_validate(pk) do
          raise "Invalid public key"
        end

        signature_point = G2Primitives.signature_to_g2(signature)

        if not G2Primitives.subgroup_check(signature_point) do
          false
        else
          final_exponentiation =
            Pairing.final_exponentiate(
              Pairing.pairing(
                signature_point,
                Curve.g1(),
                final_exponentiate: false
              ) *
                Pairing.pairing(
                  HashToCurve.hash_to_g2(message, dst, cls.xmd_hash_function),
                  Curve.neg(G2Primitives.pubkey_to_g1(pk)),
                  final_exponentiate: false
                )
            )

          final_exponentiation == FQ12.one()
        end
      rescue
        _ -> false
      end
    end

    @doc """
    The Aggregate algorithm aggregates multiple signatures into one.

    Raise `ValidationError` when there is input validation error.
    """
    def aggregate(cls, signatures) do
      # Preconditions
      if length(signatures) < 1 do
        raise "Insufficient number of signatures. (n < 1)"
      end

      # Inputs validation
      for signature <- signatures do
        if not cls._is_valid_signature(signature) do
          raise "Invalid signature"
        end
      end

      # Procedure
      # Seed with the point at infinity
      Enum.reduce(signatures, Curve.z2(), fn signature, aggregate ->
        signature_point = G2Primitives.signature_to_g2(signature)
        Curve.add(aggregate, signature_point)
      end)
      |> G2Primitives.g2_to_signature()
    end

    def _core_aggregate_verify(cls, pks, messages, signature, dst) do
      try do
        # Inputs validation
        for pk <- pks do
          if not cls._is_valid_pubkey(pk) do
            raise "Invalid public key"
          end
        end

        for message <- messages do
          if not cls._is_valid_message(message) do
            raise "Invalid message"
          end
        end

        if length(pks) != length(messages) do
          raise "Inconsistent number of PKs and messages"
        end

        if not cls._is_valid_signature(signature) do
          raise "Invalid signature"
        end

        # Preconditions
        if length(pks) < 1 do
          raise "Insufficient number of PKs. (n < 1)"
        end

        # Procedure
        signature_point = G2Primitives.signature_to_g2(signature)

        if not G2Primitives.subgroup_check(signature_point) do
          false
        else
          aggregate =
            Enum.reduce(Enum.zip(pks, messages), FQ12.one(), fn {pk, message}, aggregate ->
              if not key_validate(pk) do
                raise "Invalid public key"
              end

              pubkey_point = G2Primitives.pubkey_to_g1(pk)
              message_point = HashToCurve.hash_to_g2(message, dst, cls.xmd_hash_function)

              aggregate *
                Pairing.pairing(
                  message_point,
                  pubkey_point,
                  final_exponentiate: false
                )
            end)

          aggregate =
            aggregate *
              Pairing.pairing(signature_point, Curve.neg(Curve.g1()), final_exponentiate: false)

          Pairing.final_exponentiate(aggregate) == FQ12.one()
        end
      rescue
        _ -> false
      end
    end

    def sign(cls, sk, message) do
      _core_sign(cls, sk, message, cls.dst())
    end

    def verify(cls, pk, message, signature) do
      _core_verify(cls, pk, message, signature, cls.dst())
    end
  end

  defmodule G2Basic do
    alias ExEcc.BLS.Ciphersuites.Base
    @dst "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_NUL_"
    def dst(), do: @dst

    def aggregate_verify(cls, pks, messages, signature) do
      Base._core_aggregate_verify(cls, pks, messages, signature, cls.dst())
    end

    def aggregate(signatures), do: Base.aggregate(__MODULE__, signatures)
    def key_gen(ikm, key_info \\ <<>>), do: Base.key_gen(__MODULE__, ikm, key_info)
    def sign(sk, message), do: Base.sign(__MODULE__, sk, message)
    def sk_to_pk(privkey), do: Base.sk_to_pk(__MODULE__, privkey)
    def verify(pk, message, signature), do: Base.verify(__MODULE__, pk, message, signature)
    defdelegate _is_valid_message(message), to: Base
    defdelegate _is_valid_privkey(privkey), to: Base
    defdelegate _is_valid_pubkey(pubkey), to: Base
    defdelegate _is_valid_signature(signature), to: Base
    defdelegate xmd_hash_function(), to: Base
  end

  defmodule G2MessageAugmentation do
    alias ExEcc.BLS.Ciphersuites.Base
    @dst "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_AUG_"
    def dst(), do: @dst

    def sign(cls, sk, message) do
      Base._core_sign(cls, sk, message, cls.dst())
    end

    def verify(cls, pk, message, signature) do
      Base._core_verify(cls, pk, message, signature, cls.dst())
    end

    def aggregate_verify(cls, pks, messages, signature) do
      if length(messages) != length(Enum.uniq(messages)) do
        false
      else
        Base._core_aggregate_verify(cls, pks, messages, signature, cls.dst())
      end
    end
  end

  defmodule G2ProofOfPossession do
    alias ExEcc.BLS.Ciphersuites.Base
    alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
    alias ExEcc.BLS.G2Primitives
    @dst "BLS_SIG_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_"
    def dst(), do: @dst
    @pop_tag "BLS_POP_BLS12381G2_XMD:SHA-256_SSWU_RO_POP_"
    def pop_tag(), do: @pop_tag

    @doc """
    Note: PopVerify is a precondition for -Verify APIs
    However, it's difficult to verify it with the API interface in runtime.
    To ensure KeyValidate has been checked, we check it in the input validation.
    See https://github.com/cfrg/draft-irtf-cfrg-bls-signature/issues/27 for
    the discussion.
    """
    def is_valid_pubkey(pubkey) do
      Base._is_valid_pubkey(pubkey) and Base.key_validate(pubkey)
    end

    def aggregate_verify(cls, pks, messages, signature) do
      Base._core_aggregate_verify(cls, pks, messages, signature, cls.dst())
    end

    def pop_prove(cls, sk) do
      pubkey = Base.sk_to_pk(cls, sk)
      Base._core_sign(cls, sk, pubkey, cls.pop_tag())
    end

    def pop_verify(cls, pk, proof) do
      Base._core_verify(cls, pk, pk, proof, cls.pop_tag())
    end

    @doc """
    Aggregate the public keys.

    Raise `ValidationError` when there is input validation error.
    """
    def aggregate_pk(pks) do
      if length(pks) < 1 do
        raise "Insufficient number of PKs. (n < 1)"
      end

      Enum.reduce(pks, Curve.z1(), fn pk, aggregate ->
        pubkey_point = G2Primitives.pubkey_to_g1(pk)
        Curve.add(aggregate, pubkey_point)
      end)
      |> G2Primitives.g1_to_pubkey()
    end

    def fast_aggregate_verify(pks, message, signature) do
      try do
        # Inputs validation
        for pk <- pks do
          if not Base._is_valid_pubkey(pk) do
            raise "Invalid public key"
          end
        end

        if not Base._is_valid_message(message) do
          raise "Invalid message"
        end

        if not Base._is_valid_signature(signature) do
          raise "Invalid signature"
        end

        # Preconditions
        if length(pks) < 1 do
          raise "Insufficient number of PKs. (n < 1)"
        end

        # Procedure
        aggregate_pubkey = aggregate_pk(pks)
        Base.verify(__MODULE__, aggregate_pubkey, message, signature)
      rescue
        _ -> false
      end
    end

    defdelegate _is_valid_pubkey(pubkey), to: Base
    defdelegate _is_valid_message(message), to: Base
    defdelegate _is_valid_signature(signature), to: Base
  end
end
