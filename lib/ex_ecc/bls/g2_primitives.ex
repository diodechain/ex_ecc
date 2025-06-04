defmodule ExEcc.BLS.G2Primitives do
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.BLS.PointCompression, as: PointCompression
  alias ExEcc.BLS.Hash

  def subgroup_check(P) do
    Curve.is_inf(Curve.multiply(P, Curve.curve_order()))
  end

  def g2_to_signature(pt) do
    {z1, z2} = PointCompression.compress_g2(pt)
    Hash.i2osp(z1, 48) <> Hash.i2osp(z2, 48)
  end

  def signature_to_g2(signature) do
    p = {Hash.os2ip(binary_part(signature, 0, 48)), Hash.os2ip(binary_part(signature, 48, 48))}
    PointCompression.decompress_g2(p)
  end

  def g1_to_pubkey(pt) do
    z = PointCompression.compress_g1(pt)
    Hash.i2osp(z, 48)
  end

  def pubkey_to_g1(pubkey) do
    z = Hash.os2ip(pubkey)
    PointCompression.decompress_g1(z)
  end
end
