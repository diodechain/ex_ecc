defmodule ExEcc.Fields.FieldProperties do
  # In Elixir, we would typically represent this data as a map or a struct.
  # TypedDict from Python translates somewhat to structs with enforced keys in Elixir,
  # or simply maps with known atom keys.

  # We'll use the ExEcc.Typing types where appropriate once they are fully defined.
  # For now, using basic Elixir types.
  # @type fq2_modulus_coeffs_type :: ExEcc.Typing.fq2_modulus_coeffs_type()
  # @type fq12_modulus_coeffs_type :: ExEcc.Typing.fq12_modulus_coeffs_type()
  @type fq2_modulus_coeffs_type :: {integer, integer}
  @type fq12_modulus_coeffs_type :: {integer, integer, integer, integer, integer, integer, integer, integer, integer, integer, integer, integer}

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
        field_modulus: 21888242871839275222246405745257275088696311157297823662689037894645226208583,
        fq2_modulus_coeffs: {1, 0},
        fq12_modulus_coeffs: {82, 0, 0, 0, 0, 0, -18, 0, 0, 0, 0, 0} # Implied + [1]
      },
      "bls12_381" => %{
        field_modulus: 4002409555221667393417789825735904156556882819939007885332058136124031650490837864442687629129015664037894272559787,
        fq2_modulus_coeffs: {1, 0},
        fq12_modulus_coeffs: {2, 0, 0, 0, 0, 0, -2, 0, 0, 0, 0, 0} # Implied + [1]
      }
    }
  end
end
