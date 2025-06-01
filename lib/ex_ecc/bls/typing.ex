defmodule ExEcc.BLS.Typing do
  # Import global ExEcc types if they exist and are relevant
  # alias ExEcc.Typing, as: GlobalTyping
  # Import field element types if needed
  # alias ExEcc.Fields.OptimizedFieldElements, as: OptFQ

  # In Elixir, NewType is often just an alias or a struct for stronger typing.
  # For now, we'll use Elixir's built-in types or define simple type aliases.

  # G1Uncompressed = Optimized_Point3D[optimized_bls12_381_FQ]
  # Assuming Optimized_Point3D is like {Field, Field, Field} | nil
  # And optimized_bls12_381_FQ is an integer (representing the FQ element in optimized form)
  # This would be defined in a more specific module like ExEcc.BLS12381.Typing or similar
  # For now, a general representation:
  @type g1_uncompressed :: {integer, integer, integer} | nil

  # G1Compressed = NewType("G1Compressed", int)
  @type g1_compressed :: integer

  # G2Uncompressed = Optimized_Point3D[optimized_bls12_381_FQ2]
  # optimized_bls12_381_FQ2 would be a tuple or list of integers for optimized FQ2 elements.
  # e.g., {integer, integer} for c0 + c1*u
  # This depends on how ExEcc.Fields.OptimizedFieldElements.FQ2 is structured.
  # Let's assume an FQ2 element is represented as {c0, c1}.
  # Then a point is {{c0,c1}, {c0,c1}, {c0,c1}} | nil
  # Placeholder
  @type fq2_element_optimized :: {integer, integer}
  @type g2_uncompressed ::
          {fq2_element_optimized, fq2_element_optimized, fq2_element_optimized} | nil

  # G2Compressed = NewType("G2Compressed", Tuple[int, int])
  @type g2_compressed :: {integer, integer}
end
