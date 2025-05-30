defmodule ExEcc.Fields.Bls12381FQ2 do
  alias ExEcc.Fields.OptimizedFieldElements.FQP, as: FQP

  @modulus_coeffs [1, 0]  # x^2 + 1 = 0

  def new_fq2(coeffs, field_modulus), do: FQP.new_fqp(coeffs, @modulus_coeffs, field_modulus)
  def add(a, b), do: FQP.add(a, b)
  def sub(a, b), do: FQP.subtract(a, b)
  def mul(a, b), do: FQP.mul(a, b)
  def divide(a, b), do: FQP.divide(a, b)
  def pow(a, n), do: FQP.pow(a, n)
  def neg(a), do: FQP.neg(a)
  def equal?(a, b), do: FQP.equal?(a, b)
  def is_zero(a), do: FQP.is_zero(a)
  def is_one(a), do: FQP.is_one(a)
  def from_coords(x, y), do: FQP.new_fqp([x, y], @modulus_coeffs, nil) # field_modulus must be provided
  def to_coords(a), do: a.coeffs
  def one(field_modulus), do: FQP.one(field_modulus, 2, @modulus_coeffs)
  def zero(field_modulus), do: FQP.zero(field_modulus, 2, @modulus_coeffs)
  def new(val), do: FQP.new_fqp(val, @modulus_coeffs, nil) # field_modulus must be provided
  def multiply(a, b), do: FQP.mul(a, b)
  def negate(a), do: FQP.neg(a)

  # Add missing functions
  def inv(a), do: FQP.inv(a)
  def conjugate(a), do: FQP.conjugate(a)
  def frobenius(a, power), do: FQP.frobenius(a, power)
  def sgn0(a), do: FQP.sgn0(a)
end
