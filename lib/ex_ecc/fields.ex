defmodule ExEcc.Fields do
  alias ExEcc.FieldMath
  alias ExEcc.Fields.{FQ, FQP, FQ2, FQ12}
  alias ExEcc.Fields.{OptimizedFQ, OptimizedFQP, OptimizedFQ2, OptimizedFQ12}
  alias ExEcc.Fields.FieldProperties

  #
  # bn128 curve fields
  #
  defmodule BN128FQ do
    use FieldMath, parent: FQ
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
  end

  defmodule BN128FQP do
    use FieldMath, parent: FQP
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
  end

  defmodule BN128FQ2 do
    use FieldMath, parent: [FQ2, BN128FQP]
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
    def fq2_modulus_coeffs, do: FieldProperties.field_properties()["bn128"].fq2_modulus_coeffs
  end

  defmodule BN128FQ12 do
    use FieldMath, parent: [FQ12, BN128FQP]
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus

    def fq12_modulus_coeffs,
      do: FieldProperties.field_properties()["bn128"].fq12_modulus_coeffs
  end

  #
  # bls12_381 curve fields
  #
  defmodule BLS12381FQ do
    use FieldMath, parent: FQ
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus
  end

  defmodule BLS12381FQP do
    use FieldMath, parent: FQP
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus
  end

  defmodule BLS12381FQ2 do
    use FieldMath, parent: [FQ2, BLS12381FQP]
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus

    def fq2_modulus_coeffs,
      do: FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
  end

  defmodule BLS12381FQ12 do
    use FieldMath, parent: [FQ12, BLS12381FQP]
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus

    def fq12_modulus_coeffs,
      do: FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs
  end

  #
  # optimized_bn128 curve fields
  #
  defmodule OptimizedBN128FQ do
    use FieldMath, parent: OptimizedFQ
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
  end

  defmodule OptimizedBN128FQP do
    use FieldMath, parent: OptimizedFQP
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
  end

  defmodule OptimizedBN128FQ2 do
    use FieldMath, parent: [OptimizedFQ2, OptimizedBN128FQP]
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus
    def fq2_modulus_coeffs, do: FieldProperties.field_properties()["bn128"].fq2_modulus_coeffs
  end

  defmodule OptimizedBN128FQ12 do
    use FieldMath, parent: [OptimizedFQ12, OptimizedBN128FQP]
    def field_modulus, do: FieldProperties.field_properties()["bn128"].field_modulus

    def fq12_modulus_coeffs,
      do: FieldProperties.field_properties()["bn128"].fq12_modulus_coeffs
  end

  #
  # optimized_bls12_381 curve fields
  #
  defmodule OptimizedBLS12381FQ do
    use FieldMath, parent: OptimizedFQ
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus
  end

  defmodule OptimizedBLS12381FQP do
    use FieldMath, parent: OptimizedFQP
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus
  end

  defmodule OptimizedBLS12381FQ2 do
    use FieldMath, parent: [OptimizedFQ2, OptimizedBLS12381FQP]
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus

    def fq2_modulus_coeffs,
      do: FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
  end

  defmodule OptimizedBLS12381FQ12 do
    use FieldMath, parent: [OptimizedFQ12, OptimizedBLS12381FQP]
    def field_modulus, do: FieldProperties.field_properties()["bls12_381"].field_modulus

    def fq12_modulus_coeffs,
      do: FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs
  end
end
