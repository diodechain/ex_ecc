defmodule ExEcc.Fields.FieldProperties do
  # In Elixir, we would typically represent this data as a map or a struct.
  # TypedDict from Python translates somewhat to structs with enforced keys in Elixir,
  # or simply maps with known atom keys.

  # We'll use the ExEcc.Typing types where appropriate once they are fully defined.
  # For now, using basic Elixir types.
  # @type fq2_modulus_coeffs_type :: ExEcc.Typing.fq2_modulus_coeffs_type()
  # @type fq12_modulus_coeffs_type :: ExEcc.Typing.fq12_modulus_coeffs_type()
  @type fq2_modulus_coeffs_type :: {integer, integer}
  @type fq12_modulus_coeffs_type ::
          {integer, integer, integer, integer, integer, integer, integer, integer, integer,
           integer, integer, integer}

  @typedoc """
  Represents properties of a curve's field.
  - `field_modulus`: The modulus of the prime field FQ.
  - `fq2_modulus_coeffs`: Coefficients for the FQ2 extension field, e.g., u^2 - beta = 0. Typically (beta, 0).
  - `fq12_modulus_coeffs`: Coefficients for the FQ12 extension field. e.g. v^6 - xi = 0 where xi is in FQ2.
  """
  @type curve_field_properties :: %{
          required(:field_modulus) => integer,
          required(:fq2_modulus_coeffs) => fq2_modulus_coeffs_type(),
          required(:fq12_modulus_coeffs) => fq12_modulus_coeffs_type()
        }

  @typedoc """
  A map from curve names (as strings) to their field properties.
  """
  @type field_properties_map :: %{String.t() => curve_field_properties()}

  @doc """
  Actual field property data for supported curves.
  """
  @spec field_properties() :: field_properties_map()
  def field_properties do
    %{
      "bn128" => %{
        field_modulus:
          21_888_242_871_839_275_222_246_405_745_257_275_088_696_311_157_297_823_662_689_037_894_645_226_208_583,
        fq2_modulus_coeffs: {1, 0},
        # Implied + [1]
        fq12_modulus_coeffs: {82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0}
      },
      "bls12_381" => %{
        field_modulus:
          4_002_409_555_221_667_393_417_789_825_735_904_156_556_882_819_939_007_885_332_058_136_124_031_650_490_837_864_442_687_629_129_015_664_037_894_272_559_787,
        fq2_modulus_coeffs: {1, 0},
        # Implied + [1]
        fq12_modulus_coeffs: {2, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0}
      }
    }
  end
end
