defmodule ExEcc.OptimizedBLS12381.ClearCofactor do
  alias ExEcc.OptimizedBLS12381.Constants
  alias ExEcc.OptimizedBLS12381.OptimizedCurve

  def multiply_clear_cofactor_g1(p) do
    OptimizedCurve.multiply(p, Constants.h_eff_g1())
  end

  # Cofactor Clearing Method by Multiplication
  # There is an optimization based on this Section 4.1 of https://eprint.iacr.org/2017/419
  # However there is a patent `US patent 7110538` so I'm not sure if it can be used.
  def multiply_clear_cofactor_g2(p) do
    OptimizedCurve.multiply(p, Constants.h_eff_g2())
  end
end
