defmodule ExEcc.Fields.OptimizedBN128FQ2 do
  alias ExEcc.Fields.OptimizedFieldElements.FQP

  @field_modulus 21888242871839275222246405745257275088548364400416034343698204186575808495617
  @modulus_coeffs [1, 0]  # For BN128, the irreducible polynomial is x^2 + 1

  defstruct coeffs: [], field_modulus: @field_modulus

  def new(coeffs) when is_list(coeffs) do
    coeffs_mod = Enum.map(coeffs, fn c ->
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
