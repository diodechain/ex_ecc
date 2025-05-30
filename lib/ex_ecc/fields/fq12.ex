defmodule ExEcc.Fields.FQ12 do
  alias ExEcc.Fields.FieldElements.FQP

  @moduledoc """
  The 12th-degree extension field.
  """

  defstruct [:coeffs, :field_modulus]

  def parent(), do: FQP
  def degree(), do: 12
  def fq12_modulus_coeffs(), do: "FQ12_modulus_coeffs_type"

  def new(fq12, coeffs) do
    parent().new(fq12, coeffs, fq12_modulus_coeffs())
  end
end
