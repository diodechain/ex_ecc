defmodule ExEcc.Typing do
  # NOTE: Elixir uses @type for type specifications. This is a rough translation.
  # Further refinement will be needed to map these Python types to Elixir equivalents.

  #
  # These types are wrt Normal Integers
  #
  @type plain_point_2d :: {integer, integer}
  @type plain_point_3d :: {integer, integer, integer}

  #
  # Types for the normal curves and fields
  #
  # Elixir does not have a direct equivalent of TypeVar in the same way Python does.
  # We might define protocols or use more general types like `any` or `term` initially.
  # For now, let's represent Field as a general term.
  @type field :: any

  # :infinity or nil for point at infinity
  @type point_2d :: {:ok, {field, field}} | :infinity | nil
  # :infinity or nil for point at infinity
  @type point_3d :: {:ok, {field, field, field}} | :infinity | nil
  # GeneralPoint would be a union of point_2d and point_3d
  @type general_point :: point_2d | point_3d

  #
  # Types For optimized curves and fields
  #
  @type optimized_field :: any

  @type optimized_point_2d :: {optimized_field, optimized_field}
  @type optimized_point_3d :: {optimized_field, optimized_field, optimized_field}
  # Optimized_GeneralPoint would be a union of optimized_point_2d and optimized_point_3d
  @type optimized_general_point :: optimized_point_2d | optimized_point_3d

  #
  # Miscellaneous types
  #
  @type fq2_modulus_coeffs_type :: {integer, integer}
  @type fq12_modulus_coeffs_type ::
          {integer, integer, integer, integer, integer, integer, integer, integer, integer,
           integer, integer, integer}
end
