defmodule ExEcc.Fields.Bls12381FQ do
  alias ExEcc.Fields.OptimizedFieldElements, as: FQ

  @field_modulus 0x1A0111EA397FE69A4B1BA7B6434BACD764774B84F38512BF6730D2A0F6B0F6241EABFFFEB153FFFFB9FEFFFFFFFFAAAB

  def new_fq(val, field_modulus), do: FQ.new_fq(val, field_modulus)
  def add(a, b), do: FQ.add(a, b)
  def sub(a, b), do: FQ.sub(a, b)
  def mul(a, b), do: FQ.mul(a, b)
  def divide(a, b), do: FQ.divide(a, b)
  def pow(a, n), do: FQ.pow(a, n)
  def neg(a), do: FQ.neg(a)
  def equal?(a, b), do: FQ.equal?(a, b)
  def is_zero(a), do: FQ.is_zero(a)
  def is_one(a), do: FQ.is_one(a)
  def from_int(n), do: FQ.new_fq(n, @field_modulus)
  def to_int(a), do: a.n
  def one(field_modulus), do: FQ.one(field_modulus)
  def zero(field_modulus), do: FQ.zero(field_modulus)
  def new(val), do: FQ.new_fq(val, @field_modulus)
  def multiply(a, b), do: FQ.mul(a, b)
  def divide(a, b), do: FQ.divide(a, b)
  def negate(a), do: FQ.neg(a)

  # Add missing functions
  def inv(a), do: FQ.inv(a)
  def sgn0(a), do: FQ.sgn0(a)
  def field_modulus, do: @field_modulus
end
