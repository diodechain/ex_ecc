defmodule ExEcc.Fields.OptimizedBN128FQ2 do
  alias ExEcc.Fields.OptimizedFieldElements.FQP

  @field_modulus 21_888_242_871_839_275_222_246_405_745_257_275_088_548_364_400_416_034_343_698_204_186_575_808_495_617
  # For BN128, the irreducible polynomial is x^2 + 1
  @modulus_coeffs [1, 0]

  defstruct coeffs: [], field_modulus: @field_modulus

  def new(coeffs) when is_list(coeffs) do
    coeffs_mod =
      Enum.map(coeffs, fn c ->
        rem(rem(c, @field_modulus) + @field_modulus, @field_modulus)
      end)

    FQP.new_fqp(coeffs_mod, @modulus_coeffs, @field_modulus)
  end

  def new(coeffs, field_modulus), do: FQP.new_fqp(coeffs, @modulus_coeffs, field_modulus)

  def one do
    new([1, 0])
  end

  def zero do
    new([0, 0])
  end

  def neg(fq2), do: FQP.neg(fq2)
  def equal?(fq2a, fq2b), do: FQP.equal?(fq2a, fq2b)
  def add(fq2a, fq2b), do: FQP.add(fq2a, fq2b)
  def subtract(fq2a, fq2b), do: FQP.subtract(fq2a, fq2b)
  def multiply(fq2a, fq2b), do: FQP.multiply(fq2a, fq2b)
  def divide(fq2a, fq2b), do: FQP.divide(fq2a, fq2b)
  def pow(fq2, exponent), do: FQP.pow(fq2, exponent)
  def negate(fq2), do: FQP.neg(fq2)
  def eq(fq2a, fq2b), do: FQP.equal?(fq2a, fq2b)
  def mul(a, b), do: multiply(a, b)
end
