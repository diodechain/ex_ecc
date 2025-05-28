defmodule ExEcc.OptimizedBls12381.ClearCofactor do
  alias ExEcc.OptimizedBls12381.Constants, as: Const
  alias ExEcc.OptimizedBls12381.Curve, as: OptimizedCurve
  # Assuming Optimized_Field and Optimized_Point3D types will be handled by existing aliases
  # or defined in a common typing module if necessary.

  def multiply_clear_cofactor_g1(p) do
    OptimizedCurve.multiply(p, Const.h_eff_g1())
    # Python has H_EFF_G1, I am assuming it is defined in Constants or should be.
    # If not, this will need to be added to constants.ex
  end

  # Cofactor Clearing Method by Multiplication
  # There is an optimization based on this Section 4.1 of https://eprint.iacr.org/2017/419
  # However there is a patent `US patent 7110538` so I'm not sure if it can be used.
  def multiply_clear_cofactor_g2(p) do
    OptimizedCurve.multiply(p, Const.h_eff_g2())
  end
end
