defmodule ExEcc.Fields.FieldProperties do
  def field_properties do
    %{
      "bn128" => %{
        field_modulus:
          21_888_242_871_839_275_222_246_405_745_257_275_088_696_311_157_297_823_662_689_037_894_645_226_208_583,
        fq2_modulus_coeffs: {1, 0},
        fq12_modulus_coeffs: {82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0}
      },
      "bls12_381" => %{
        field_modulus:
          4_002_409_555_221_667_393_417_789_825_735_904_156_556_882_819_939_007_885_332_058_136_124_031_650_490_837_864_442_687_629_129_015_664_037_894_272_559_787,
        fq2_modulus_coeffs: {1, 0},
        fq12_modulus_coeffs: {2, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0}
      }
    }
  end

  def field_properties(curve_name) when curve_name in ["bn128", "bls12_381"] do
    field_properties()[curve_name]
  end

  def field_property(curve_name, property)
      when curve_name in ["bn128", "bls12_381"] and
             property in [:field_modulus, :fq2_modulus_coeffs, :fq12_modulus_coeffs] do
    Map.fetch!(field_properties(curve_name), property)
  end
end
