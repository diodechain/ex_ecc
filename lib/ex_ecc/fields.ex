defmodule ExEcc.Fields do
  alias ExEcc.Fields.FieldElements, as: FQ
  alias ExEcc.Fields.FieldElements.FQP
  alias ExEcc.Fields.OptimizedFieldElements, as: OptimizedFQ
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: OptimizedFQP
  alias ExEcc.Fields.FieldProperties

  # BLS12-381 curve fields
  defmodule BLS12381 do
    @field_modulus FieldProperties.field_properties()["bls12_381"].field_modulus
    @fq2_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
    @fq12_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs

    def fq(n) do
      FQ.new_fq(n, @field_modulus)
    end

    def fqp(coeffs, modulus_coeffs \\ @fq2_modulus_coeffs) do
      FQP.new_fqp(coeffs, modulus_coeffs, @field_modulus)
    end

    def fq2(coeffs) do
      FQP.new_fqp(coeffs, @fq2_modulus_coeffs, @field_modulus)
    end

    def fq12(coeffs) do
      FQP.new_fqp(coeffs, @fq12_modulus_coeffs, @field_modulus)
    end
  end

  # Optimized BLS12-381 curve fields
  defmodule OptimizedBLS12381 do
    @field_modulus FieldProperties.field_properties()["bls12_381"].field_modulus
    @fq2_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq2_modulus_coeffs
    @fq12_modulus_coeffs FieldProperties.field_properties()["bls12_381"].fq12_modulus_coeffs

    def fq(n) do
      OptimizedFQ.new_fq(n, @field_modulus)
    end

    def fqp(coeffs, modulus_coeffs \\ @fq2_modulus_coeffs) do
      OptimizedFQP.new_fqp(coeffs, modulus_coeffs, @field_modulus)
    end

    def fq2(coeffs) do
      OptimizedFQP.new_fqp(coeffs, @fq2_modulus_coeffs, @field_modulus)
    end

    def fq12(coeffs) do
      OptimizedFQP.new_fqp(coeffs, @fq12_modulus_coeffs, @field_modulus)
    end
  end
end
