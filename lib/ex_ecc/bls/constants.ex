defmodule ExEcc.BLS.Constants do
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.FieldMath

  @q ExEcc.OptimizedBLS12381.OptimizedCurve.field_modulus()
  def q, do: @q

  @g2_cofactor 305_502_333_931_268_344_200_999_753_193_121_504_214_466_019_254_188_142_667_664_032_982_267_604_182_971_884_026_507_427_359_259_977_847_832_272_839_041_616_661_285_803_823_378_372_096_355_777_062_779_109
  def g2_cofactor, do: @g2_cofactor

  @fq2_order @q ** 2 - 1
  def fq2_order, do: @fq2_order

  @eighth_roots_of_unity Enum.map(0..7, fn k ->
                           FieldMath.pow(FQ2.new({1, 1}), div(@fq2_order * k, 8))
                         end)
                         |> List.to_tuple()
  def eighth_roots_of_unity, do: @eighth_roots_of_unity

  @pow_2_381 2 ** 381
  def pow_2_381, do: @pow_2_381

  @pow_2_382 2 ** 382
  def pow_2_382, do: @pow_2_382

  @pow_2_383 2 ** 383
  def pow_2_383, do: @pow_2_383

  @pow_2_384 2 ** 384
  def pow_2_384, do: @pow_2_384

  @hash_to_field_l 64
  def hash_to_field_l, do: @hash_to_field_l
end
