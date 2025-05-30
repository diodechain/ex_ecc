defmodule ExECC.BLS12381 do
  @moduledoc """
  BLS12-381 curve implementation.
  This module provides aliases and exports for the BLS12-381 curve operations.
  """

  alias ExEcc.Bls12381.Curve, as: Curve
  alias ExEcc.Bls12381.Pairing, as: Pairing
  alias ExEcc.Fields.Bls12381FQ, as: FQ
  alias ExEcc.Fields.Bls12381FQ2, as: FQ2
  alias ExEcc.Fields.Bls12381FQ12, as: FQ12

  # Re-export commonly used functions
  defdelegate add(p1, p2), to: Curve
  defdelegate double(p), to: Curve
  defdelegate multiply(p, n), to: Curve
  defdelegate neg(p), to: Curve
  defdelegate eq(p1, p2), to: Curve
  defdelegate is_on_curve(p, b), to: Curve
  defdelegate g1(), to: Curve
  defdelegate g2(), to: Curve
  defdelegate curve_order(), to: Curve

  # Pairing operations
  defdelegate pairing(q, p), to: Pairing
  defdelegate final_exponentiate(f), to: Pairing

  # Field operations
  defdelegate new(val, field_modulus), to: FQ
  defdelegate new_fq2(coeffs, field_modulus), to: FQ2
  defdelegate new_fq12(coeffs, field_modulus), to: FQ12

  # Function aliases to match test expectations
  def normalize(p), do: Curve.normalize(p)
  def is_inf(p), do: Curve.is_inf(p)
  def is_on_curve2(p), do: Curve.is_on_curve(p)
  def from_coords(x, y), do: Curve.from_coords(x, y)
  def to_coords(p), do: Curve.to_coords(p)

  # G2 function aliases
  def add2(p1, p2), do: Curve.add(p1, p2)
  def double2(p), do: Curve.double(p)
  def multiply2(p, n), do: Curve.multiply(p, n)
  def neg2(p), do: Curve.neg(p)
  def normalize2(p), do: Curve.normalize(p)
  def is_inf2(p), do: Curve.is_inf(p)
  def eq2(p1, p2), do: Curve.eq(p1, p2)
  def from_coords2(x, y), do: Curve.from_coords(x, y)
  def to_coords2(p), do: Curve.to_coords(p)

  # Field function aliases
  def fq_add(a, b), do: FQ.add(a, b)
  def fq_sub(a, b), do: FQ.sub(a, b)
  def fq_mul(a, b), do: FQ.mul(a, b)
  def fq_div(a, b), do: FQ.div(a, b)
  def fq_neg(a), do: FQ.neg(a)
  def fq_inv(a), do: FQ.inv(a)
  def fq_pow(a, n), do: FQ.pow(a, n)
  def fq_eq(a, b), do: FQ.eq(a, b)
  def fq_is_zero(a), do: FQ.is_zero(a)
  def fq_is_one(a), do: FQ.is_one(a)
  def fq_from_int(n), do: FQ.from_int(n)
  def fq_to_int(a), do: FQ.to_int(a)

  # FQ2 function aliases
  def fq2_add(a, b), do: FQ2.add(a, b)
  def fq2_sub(a, b), do: FQ2.sub(a, b)
  def fq2_mul(a, b), do: FQ2.mul(a, b)
  def fq2_div(a, b), do: FQ2.div(a, b)
  def fq2_neg(a), do: FQ2.neg(a)
  def fq2_inv(a), do: FQ2.inv(a)
  def fq2_pow(a, n), do: FQ2.pow(a, n)
  def fq2_eq(a, b), do: FQ2.eq(a, b)
  def fq2_is_zero(a), do: FQ2.is_zero(a)
  def fq2_is_one(a), do: FQ2.is_one(a)
  def fq2_from_coords(x, y), do: FQ2.from_coords(x, y)
  def fq2_to_coords(a), do: FQ2.to_coords(a)

  # FQ6 function aliases
  def fq6_add(a, b), do: Curve.add(a, b)
  def fq6_sub(a, b), do: Curve.sub(a, b)
  def fq6_mul(a, b), do: Curve.mul(a, b)
  def fq6_div(a, b), do: Curve.div(a, b)
  def fq6_neg(a), do: Curve.neg(a)
  def fq6_inv(a), do: Curve.inv(a)
  def fq6_pow(a, n), do: Curve.pow(a, n)
  def fq6_eq(a, b), do: Curve.eq(a, b)
  def fq6_is_zero(a), do: Curve.is_zero(a)
  def fq6_is_one(a), do: Curve.is_one(a)
  def fq6_from_coords(x, y, z), do: Curve.from_coords(x, y, z)
  def fq6_to_coords(a), do: Curve.to_coords(a)

  # FQ12 function aliases
  def fq12_add(a, b), do: FQ12.add(a, b)
  def fq12_sub(a, b), do: FQ12.sub(a, b)
  def fq12_mul(a, b), do: FQ12.mul(a, b)
  def fq12_div(a, b), do: FQ12.div(a, b)
  def fq12_neg(a), do: FQ12.neg(a)
  def fq12_inv(a), do: FQ12.inv(a)
  def fq12_pow(a, n), do: FQ12.pow(a, n)
  def fq12_eq(a, b), do: FQ12.eq(a, b)
  def fq12_is_zero(a), do: FQ12.is_zero(a)
  def fq12_is_one(a), do: FQ12.is_one(a)
  def fq12_from_coords(x, y), do: FQ12.from_coords(x, y)
  def fq12_to_coords(a), do: FQ12.to_coords(a)

  # ... existing code ...
end
