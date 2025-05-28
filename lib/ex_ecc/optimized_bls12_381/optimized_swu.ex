defmodule ExEcc.OptimizedBLS12381.OptimizedSWU do
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.OptimizedBLS12381.Constants

  # Optimized SWU Map - FQ to G1'
  # Found in Section 4 of https://eprint.iacr.org/2019/403
  def optimized_swu_g1(t) do
    t2 = FQ.mul(t, t)
    iso_11_z_t2 = FQ.mul(Constants.iso_11_z(), t2)
    temp = FQ.add(iso_11_z_t2, FQ.mul(iso_11_z_t2, iso_11_z_t2))
    # -a(Z * t^2 + Z^2 * t^4)
    denominator = FQ.neg(FQ.mul(Constants.iso_11_a(), temp))
    temp_plus_one = FQ.add(temp, FQ.one())
    # b(Z * t^2 + Z^2 * t^4 + 1)
    numerator = FQ.mul(Constants.iso_11_b(), temp_plus_one)

    # Exceptional case
    denominator =
      if denominator == FQ.zero() do
        FQ.mul(Constants.iso_11_z(), Constants.iso_11_a())
      else
        denominator
      end

    # v = D^3
    v = FQ.pow(denominator, 3)
    # u = N^3 + a * N * D^2 + b* D^3
    u =
      FQ.add(
        FQ.pow(numerator, 3),
        FQ.add(
          FQ.mul(Constants.iso_11_a(), FQ.mul(numerator, FQ.pow(denominator, 2))),
          FQ.mul(Constants.iso_11_b(), v)
        )
      )

    # Attempt y = sqrt(u / v)
    {is_root, y_intermediate} = sqrt_division_fq(u, v)
    y = y_intermediate

    y =
      if not is_root do
        FQ.mul(FQ.mul(y, FQ.pow(t, 3)), Constants.sqrt_minus_11_cubed())
      else
        y
      end

    numerator =
      if not is_root do
        FQ.mul(numerator, iso_11_z_t2)
      else
        numerator
      end

    y =
      if FQ.sgn0(t) != FQ.sgn0(y) do
        FQ.neg(y)
      else
        y
      end

    y_final = FQ.mul(y, denominator)

    {numerator, y_final, denominator}
  end

  # Optimized SWU Map - FQ2 to G2': y^2 = x^3 + 240i * x + 1012 + 1012i
  # Found in Section 4 of https://eprint.iacr.org/2019/403
  def optimized_swu_g2(t) do
    t2 = FQ2.mul(t, t)
    iso_3_z_t2 = FQ2.mul(Constants.iso_3_z(), t2)
    temp = FQ2.add(iso_3_z_t2, FQ2.mul(iso_3_z_t2, iso_3_z_t2))
    # The initial calculation for denominator_val for optimized_swu_g2
    calculated_denominator = FQ2.neg(FQ2.mul(Constants.iso_3_a(), temp))
    temp_plus_one = FQ2.add(temp, FQ2.one())
    # b(Z * t^2 + Z^2 * t^4 + 1)
    numerator_val = FQ2.mul(Constants.iso_3_b(), temp_plus_one)

    # Exceptional case for denominator_val
    denominator_val =
      if calculated_denominator == FQ2.zero() do
        FQ2.mul(Constants.iso_3_z(), Constants.iso_3_a())
      else
        calculated_denominator
      end

    # v = D^3
    v = FQ2.pow(denominator_val, 3)
    # u = N^3 + a * N * D^2 + b* D^3
    u_val =
      FQ2.add(
        FQ2.pow(numerator_val, 3),
        FQ2.add(
          FQ2.mul(Constants.iso_3_a(), FQ2.mul(numerator_val, FQ2.pow(denominator_val, 2))),
          FQ2.mul(Constants.iso_3_b(), v)
        )
      )

    # Attempt y = sqrt(u / v)
    {success, sqrt_candidate_initial} = sqrt_division_fq2(u_val, v)
    y = sqrt_candidate_initial

    # Handle case where (u / v) is not square
    # sqrt_candidate(x1) = sqrt_candidate(x0) * t^3
    sqrt_candidate_transformed = FQ2.mul(sqrt_candidate_initial, FQ2.pow(t, 3))

    # u(x1) = Z^3 * t^6 * u(x0)
    u_transformed = FQ2.mul(FQ2.pow(iso_3_z_t2, 3), u_val)

    # Reduce to find the correct y
    {final_y, success_2_final, final_numerator} =
      Enum.reduce_while(Constants.etas(), {y, false, numerator_val}, fn eta,
                                                                        {current_y, success_2_acc,
                                                                         current_numerator} ->
        eta_sqrt_candidate = FQ2.mul(eta, sqrt_candidate_transformed)

        temp1 =
          FQ2.sub(FQ2.mul(FQ2.mul(eta_sqrt_candidate, eta_sqrt_candidate), v), u_transformed)

        if temp1 == FQ2.zero() and not success and not success_2_acc do
          {:halt, {eta_sqrt_candidate, true, current_numerator}}
        else
          {:cont, {current_y, success_2_acc, current_numerator}}
        end
      end)

    # This case should ideally be unreachable if the algorithm is correct.
    if not success and not success_2_final do
      raise "Hash to Curve - Optimized SWU failure"
    end

    numerator_final =
      if not success do
        FQ2.mul(final_numerator, iso_3_z_t2)
      else
        final_numerator
      end

    y_corrected_sign =
      if FQ2.sgn0(t) != FQ2.sgn0(final_y) do
        FQ2.neg(final_y)
      else
        final_y
      end

    y_overall_final = FQ2.mul(y_corrected_sign, denominator_val)

    {numerator_final, y_overall_final, denominator_val}
  end

  def sqrt_division_fq(u, v) do
    temp = FQ.mul(u, v)
    # P_MINUS_3_DIV_4 needs to be defined in Constants or passed if it's dynamic.
    # For now, assuming it is available via Constants module.
    exponent = Constants.p_minus_3_div_4()
    result = FQ.mul(temp, FQ.pow(FQ.mul(temp, FQ.pow(v, 2)), exponent))
    is_valid_root = FQ.sub(FQ.mul(FQ.mul(result, result), v), u) == FQ.zero()
    {is_valid_root, result}
  end

  # Square Root Division
  # Return: uv^7 * (uv^15)^((p^2 - 9) / 16) * root of unity
  # If valid square root is found return true, else false
  def sqrt_division_fq2(u, v) do
    temp1 = FQ2.mul(u, FQ2.pow(v, 7))
    temp2 = FQ2.mul(temp1, FQ2.pow(v, 8))

    # P_MINUS_9_DIV_16 needs to be available via Constants.
    exponent = Constants.p_minus_9_div_16()
    gamma = FQ2.mul(FQ2.pow(temp2, exponent), temp1)

    # Verify there is a valid root
    # POSITIVE_EIGHTH_ROOTS_OF_UNITY needs to be available via Constants.
    initial_accumulator = {false, gamma}

    {is_valid_root, result} =
      Enum.reduce_while(Constants.positive_eighth_roots_of_unity(), initial_accumulator, fn root,
                                                                                            {acc_is_valid,
                                                                                             acc_result} ->
        sqrt_candidate = FQ2.mul(root, gamma)
        check_val = FQ2.sub(FQ2.mul(FQ2.mul(sqrt_candidate, sqrt_candidate), v), u)

        if check_val == FQ2.zero() and not acc_is_valid do
          {:halt, {true, sqrt_candidate}}
        else
          {:cont, {acc_is_valid, acc_result}}
        end
      end)

    {is_valid_root, result}
  end

  # Optimal Map from 3-Isogenous Curve to G2
  def iso_map_g2(x, y, z) do
    # x-numerator, x-denominator, y-numerator, y-denominator
    # Initialize with FQ2 zero values
    mapped_values = List.duplicate(FQ2.zero(), 4)
    # z, z^2, z^3
    z_powers = [z, FQ2.pow(z, 2), FQ2.pow(z, 3)]

    # ISO_3_MAP_COEFFICIENTS comes from Constants
    # Horner Polynomial Evaluation
    {new_mapped_values, _} =
      Enum.reduce(
        Enum.with_index(Constants.iso_3_map_coefficients()),
        {mapped_values, z_powers},
        fn {k_i, i}, {current_mapped_values, current_z_powers} ->
          # last element
          val_i = Enum.at(k_i, -1)

          val_i_updated =
            Enum.reduce(
              Enum.with_index(Enum.reverse(Enum.slice(k_i, 0..(length(k_i) - 2)))),
              val_i,
              fn {k_i_j, j}, acc_val_i ->
                FQ2.add(FQ2.mul(acc_val_i, x), FQ2.mul(Enum.at(current_z_powers, j), k_i_j))
              end
            )

          {List.replace_at(current_mapped_values, i, val_i_updated), current_z_powers}
        end
      )

    x_num = Enum.at(new_mapped_values, 0)
    x_den = Enum.at(new_mapped_values, 1)
    y_num = Enum.at(new_mapped_values, 2)
    y_den = Enum.at(new_mapped_values, 3)

    # y-numerator * y
    y_num_updated = FQ2.mul(y_num, y)
    # y-denominator * z
    y_den_updated = FQ2.mul(y_den, z)

    # x-denominator * y-denominator
    z_g2 = FQ2.mul(x_den, y_den_updated)
    # x-numerator * y-denominator
    x_g2 = FQ2.mul(x_num, y_den_updated)
    # y-numerator * x-denominator
    y_g2 = FQ2.mul(x_den, y_num_updated)

    {x_g2, y_g2, z_g2}
  end

  # Optimal Map from 11-Isogenous Curve to G1
  def iso_map_g1(x, y, z) do
    # Initialize with FQ zero values
    mapped_values = List.duplicate(FQ.zero(), 4)

    # Max power needed is 15 for z, assuming ISO_11_MAP_COEFFICIENTS structure
    # Python code has z_powers up to z**15, implying coefficients might need up to that.
    # Let's generate powers of z up to a reasonable limit needed by the coefficients.
    # The maximum length of a coefficient list (minus one for the last term) will determine max power.
    # For now, creating up to z_powers[14] which is z^15, as in Python.
    z_powers = Enum.map(1..15, &FQ.pow(z, &1))

    # ISO_11_MAP_COEFFICIENTS from Constants
    # Horner Polynomial Evaluation
    {new_mapped_values, _} =
      Enum.reduce(
        Enum.with_index(Constants.iso_11_map_coefficients()),
        {mapped_values, z_powers},
        fn {k_i, i}, {current_mapped_values, current_z_powers} ->
          # last element
          val_i = Enum.at(k_i, -1)

          val_i_updated =
            Enum.reduce(
              Enum.with_index(Enum.reverse(Enum.slice(k_i, 0..(length(k_i) - 2)))),
              val_i,
              fn {k_i_j, j}, acc_val_i ->
                # Ensure j is within bounds for current_z_powers
                # Python code uses z_powers[j], Elixir Enum.at(current_z_powers, j)
                FQ.add(FQ.mul(acc_val_i, x), FQ.mul(Enum.at(current_z_powers, j), k_i_j))
              end
            )

          {List.replace_at(current_mapped_values, i, val_i_updated), current_z_powers}
        end
      )

    x_num = Enum.at(new_mapped_values, 0)
    x_den = Enum.at(new_mapped_values, 1)
    y_num = Enum.at(new_mapped_values, 2)
    y_den = Enum.at(new_mapped_values, 3)

    # y-numerator * y
    y_num_updated = FQ.mul(y_num, y)
    # y-denominator * z
    y_den_updated = FQ.mul(y_den, z)

    z_g1 = FQ.mul(x_den, y_den_updated)
    x_g1 = FQ.mul(x_num, y_den_updated)
    y_g1 = FQ.mul(x_den, y_num_updated)

    {x_g1, y_g1, z_g1}
  end
end
