defmodule ExEcc.BLS.HashToCurveTest do
  use ExUnit.Case
  alias ExEcc.BLS.HashToCurve
  alias ExEcc.Fields.OptimizedBLS12381FQ, as: FQ
  alias ExEcc.Fields.OptimizedBLS12381FQ2, as: FQ2
  alias ExEcc.Fields.OptimizedBLS12381FQ12, as: FQ12
  alias ExEcc.Fields.OptimizedBLS12381FQP, as: FQP
  alias ExEcc.OptimizedBLS12381.OptimizedCurve, as: Curve
  alias ExEcc.OptimizedBLS12381.OptimizedSWU
  alias ExEcc.FieldMath

  @dst_g1 "QUUX-V01-CS02-with-BLS12381G1_XMD:SHA-256_SSWU_RO_"
  @dst_g2 "QUUX-V01-CS02-with-BLS12381G2_XMD:SHA-256_SSWU_RO_"

  describe "iso_map_G2" do
    test "maps points correctly" do
      test_cases = [
        {
          FQ2.new({
            0x0888F3832AD680917A71A1816C939290473474982C647B0B196BA0EDF62A0BC1A15D3E87CF6A287137B16C057E1AC808,
            0x0B3D6E7A20275C100B460A900B23F2D8D5E9A53C3E59066E8D968D07AB0787940C0AC8A6C8C118FAD9068A2ECF00ADD7
          }),
          FQ2.new({
            0x08696DF8BAF8C488B7CFCA14CB984D0B78C998C3431E41700B493AAF921F779AA7F3660B1F5D6AC3BA4EBC85A1132CF3,
            0x053003D3ED23019E585CF255A58634CEDA4C362B2E1D75E2AE85F4D1EF9C400786256D4AEE443DD1C900DD72E4089F73
          }),
          FQ2.new({
            0x108F7DF15439154BF32D7E4D1B6FEFC4BEF7C39A16AACA469D249770AD7B9F4AD3EA3CE58333A3194177C2D14B5CD2BC,
            0x09E2E891E7A7AB58D5BF93864000ADBF0B6C31A8E35AB6AEC3B0820C2E536D6F0D170840B0AAFB470A9FD9B2F7DE3C27
          }),
          FQ2.new({
            0x168A912067A8F06CEB1F5F59DCEC69CE47F5A2B1696DFD5E67F1CF675587AD3A19831842D2543957BEE44FE29592996E,
            0x116F36861307AA38251CAA73AA44FA359732DD92
          }),
          FQ2.new({
            0x0D4976CD99F4AD7204BC5983F6CE590766852DB93E5BE6CAB4C28591013E132BC6100D42022D5B66CE68A64A6B2A9C24,
            0x0C6BA0E076144119F2B272718EC04C3FB037C9AA2C4074E64BE233AB27C0397BE175B9FDA277DCE8841669F787161AD2
          })
        },
        {
          FQ2.new({
            0x039C33A34D97134F01D334F13C76BD5BB803B853BE4221A826026BFC93B5CA39E74B51A15D00BF88DF4F655915553027,
            0x08DA2162E554A644AECC1F904F2B140D0296B7AC85B4EE59313DCEDE58B375C2E677160BC97CF8114361ABBE7D4672CD
          }),
          FQ2.new({
            0x1201968136C60428FB9DF8004C4915DC5E502D20D32F9DD87BC38163A52E2729289490030235E61EAEA098B0E8D63BF8,
            0x116524863E40B6437BBAB965CDB84614F2346F1AD40300E9B15C3BDDE498E1FC1F76346452D3CF25553E2A3B89D9C5B1
          }),
          FQ2.new({
            0x08C3BCEBE1FC7F9987AE406A78C3FC898AE0C8A2FF0139A523E3CE91263EAA617519FC1A1158AF39BBA705316C9C2678,
            0x0C9E92BB5509704DA0B6825A3AA36BA68A877875258F17C315FEA1527A82C7975E8439E91644616DABFD28E1DB43C1D9
          }),
          FQ2.new({
            0x1990072F0029639467E5C5EF9F65B31F194C31586D56141A7906DE6EE2B40803E06A301F9EEE9C8B04FA6AF8C5950F64,
            0x0910709BEC8515357CB68AE88EA0B7EC6D54190773CC82EDDA68180D62BA214737DC708A5DA815E8B872D3C5B31E5A00
          }),
          FQ2.new({
            0x12416C8B9159A047D5F92A6A4E941156E29E2A489B671D2FC3D8ED60FFA5F53FE846ECFB0090211197EF3BA4C07424F9,
            0x089977D619CEA9D6D11F7148E1CB7622E46153BF1B4D81944603AA72AEFA6CE7CF07550CB6B582D17440F5949D1214FA
          })
        }
      ]

      for {iso_x, iso_y, iso_z, g2_x, g2_y} <- test_cases do
        {result_x, result_y, result_z} = OptimizedSWU.iso_map_g2(iso_x, iso_y, iso_z)

        result_x = FieldMath.div(result_x, result_z)
        result_y = FieldMath.div(result_y, result_z)

        assert FieldMath.eq(g2_x, result_x)
        assert FieldMath.eq(g2_y, result_y)
      end
    end
  end

  describe "hash_to_G2" do
    test "hashes messages to G2 points correctly" do
      test_cases = [
        {
          "",
          FQ2.new({
            0x0141EBFBDCA40EB85B87142E130AB689C673CF60F1A3E98D69335266F30D9B8D4AC44C1038E9DCDD5393FAF5C41FB78A,
            0x05CB8437535E20ECFFAEF7752BADDF98034139C38452458BAEEFAB379BA13DFF5BF5DD71B72418717047F5B0F37DA03D
          }),
          FQ2.new({
            0x0503921D7F6A12805E72940B963C0CF3471C7B2A524950CA195D11062EE75EC076DAF2D4BC358C4B190C0C98064FDD92,
            0x12424AC32561493F3FE3C260708A12B7C620E7BE00099A974E259DDC7D1F6395C3C811CDD19F1E8DBF3E9ECFDCBAB8D6
          })
        },
        {
          "abc",
          FQ2.new({
            0x02C2D18E033B960562AAE3CAB37A27CE00D80CCD5BA4B7FE0E7A210245129DBEC7780CCC7954725F4168AFF2787776E6,
            0x139CDDBCCDC5E91B9623EFD38C49F81A6F83F175E80B06FC374DE9EB4B41DFE4CA3A230ED250FBE3A2ACF73A41177FD8
          }),
          FQ2.new({
            0x1787327B68159716A37440985269CF584BCB1E621D3A7202BE6EA05C4CFE244AEB197642555A0645FB87BF7466B2BA48,
            0x00AA65DAE3C8D732D10ECD2C50F8A1BAF3001578F71C694E03866E9F3D49AC1E1CE70DD94A733534F106D4CEC0EDDD16
          })
        }
      ]

      for {msg, x, y} <- test_cases do
        point = HashToCurve.hash_to_g2(msg, @dst_g2, ExEcc.BLS.Hash.sha256_function())
        assert Curve.is_on_curve(point, Curve.b2())

        # Affine
        # X / Z
        result_x = elem(point, 0) / elem(point, 2)
        # Y / Z
        result_y = elem(point, 1) / elem(point, 2)

        assert x == result_x
        assert y == result_y
      end
    end
  end

  describe "FQ sgn0" do
    test "computes sgn0 correctly" do
      test_cases = [
        {1, 0x00, 0},
        {1, 0x10, 0},
        {1, 0x01, 1},
        {2, 0x00, 0},
        {2, 0x10, 0},
        {2, 0x01, 1},
        {12, 0x00, 0},
        {12, 0x10, 0},
        {12, 0x01, 1}
      ]

      for {degree, value, expected} <- test_cases do
        x =
          case degree do
            1 -> FQ.new(value)
            2 -> FQ2.new({value, 0})
            12 -> FQ12.new(List.to_tuple([value | List.duplicate(0, 11)]))
          end

        assert FieldMath.sgn0(x) == expected

        if value != 0 do
          assert FieldMath.sgn0(x) != FieldMath.sgn0(FieldMath.neg(x))
        end
      end
    end
  end

  describe "FQ2 sgn0" do
    test "computes sgn0 correctly" do
      test_cases = [
        {{0x00, 0x00}, 0},
        {{0x10, 0x00}, 0},
        {{0x01, 0x00}, 1},
        {{0x01, 0x01}, 1},
        {{0x10, 0x10}, 0}
      ]

      for {value, expected} <- test_cases do
        x = FQ2.new(value)
        y = FQP.new(%FQP{modulus_coeffs: FQ2.fq2_modulus_coeffs()}, value)
        assert FieldMath.sgn0(x) == FieldMath.sgn0(y)
        assert FieldMath.sgn0(x) == expected
      end
    end
  end

  describe "hash_to_G1" do
    test "hashes messages to G1 points correctly" do
      test_cases = [
        {
          "",
          FQ.new(
            0x052926ADD2207B76CA4FA57A8734416C8DC95E24501772C814278700EED6D1E4E8CF62D9C09DB0FAC349612B759E79A1
          ),
          FQ.new(
            0x8BA738453BFED09CB546DBB0783DBB3A5F1F566ED67BB6BE0E8C67E2E81A4CC68EE29813BB7994998F3EAE0C9C6A265
          )
        },
        {
          "abc",
          FQ.new(
            0x03567BC5EF9C690C2AB2ECDF6A96EF1C139CC0B2F284DCA0A9A7943388A49A3AEE664BA5379A7655D3C68900BE2F6903
          ),
          FQ.new(
            0x0B9C15F3FE6E5CF4211F346271D7B01C8F3B28BE689C8429C85B67AF215533311F0B8DFAAA154FA6B88176C229F2885D
          )
        }
      ]

      for {msg, x, y} <- test_cases do
        point = HashToCurve.hash_to_g1(msg, @dst_g1, ExEcc.BLS.Hash.sha256_function())
        assert Curve.is_on_curve(point, Curve.b())

        # Affine
        # X / Z
        result_x = elem(point, 0) / elem(point, 2)
        # Y / Z
        result_y = elem(point, 1) / elem(point, 2)

        assert x == result_x
        assert y == result_y
      end
    end
  end
end
