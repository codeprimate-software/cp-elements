/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.junit.Test;

/**
 * Unit Tests for {@link NumberUtils}.
 *
 * @author John J. Blum
 * @see java.math.BigDecimal
 * @see java.math.BigInteger
 * @see org.junit.Test
 * @see org.cp.elements.lang.NumberUtils
 * @since 1.0.0
 */
public class NumberUtilsTests {

  private String formatAsHexadecimalString(String number) {
    return number.startsWith(NumberUtils.HEXADECIMAL_PREFIX_NOTATION) ? number
      : String.format("%1$s%2$s", NumberUtils.HEXADECIMAL_PREFIX_NOTATION, number);
  }

  @Test
  public void isBinaryString() {

    assertThat(NumberUtils.isBinaryString("0")).isTrue();
    assertThat(NumberUtils.isBinaryString("1")).isTrue();
    assertThat(NumberUtils.isBinaryString("0001")).isTrue();
    assertThat(NumberUtils.isBinaryString("0010")).isTrue();
    assertThat(NumberUtils.isBinaryString("10")).isTrue();
    assertThat(NumberUtils.isBinaryString("1010")).isTrue();
    assertThat(NumberUtils.isBinaryString("10001010")).isTrue();
  }

  @Test
  public void isNotBinaryString() {

    assertThat(NumberUtils.isBinaryString("O")).isFalse();
    assertThat(NumberUtils.isBinaryString("1O")).isFalse();
    assertThat(NumberUtils.isBinaryString("0llO")).isFalse();
    assertThat(NumberUtils.isBinaryString("1234567890")).isFalse();
    assertThat(NumberUtils.isBinaryString("CAFEBABE")).isFalse();
    assertThat(NumberUtils.isBinaryString("0xCAFEBABE")).isFalse();
    assertThat(NumberUtils.isBinaryString("TEXT")).isFalse();
  }

  @Test
  public void fromBinaryStringIsCorrect() {

    Integer value = 1248163264;
    String binaryString = Integer.toBinaryString(value);

    assertThat(NumberUtils.fromBinaryString(binaryString)).isEqualTo(value);
  }

  @Test
  public void fromBlankBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("  "))
      .withMessage("Binary String [  ] is required")
      .withNoCause();
  }

  @Test
  public void fromEmptyBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString(""))
      .withMessage("Binary String [] is required")
      .withNoCause();
  }

  @Test
  public void fromNullBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString(null))
      .withMessage("Binary String [null] is required")
      .withNoCause();
  }

  @Test
  public void fromHexBasedBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("B00BFAB5"))
      .withMessage("Binary String [B00BFAB5] must contain only 1s and 0s")
      .withNoCause();
  }

  @Test
  public void fromInvalidBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("1Ol01O"))
      .withMessage("Binary String [1Ol01O] must contain only 1s and 0s")
      .withNoCause();
  }

  @Test
  public void fromTextBasedBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("OxTEXT"))
      .withMessage("Binary String [OxTEXT] must contain only 1s and 0s")
      .withNoCause();
  }

  @Test
  public void isHexadecimalString() {

    assertThat(NumberUtils.isHexadecimalString("0xabcdef")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0xabCDef")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0xAbCDeF")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0xAbCdEf")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0xB00BFAB5")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0xCAFEBABE")).isTrue();
    assertThat(NumberUtils.isHexadecimalString("0x10a2B3c4D5e6F78")).isTrue();
  }

  @Test
  public void isNotHexadecimalString() {

    assertThat(NumberUtils.isHexadecimalString("10101010")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0123456789")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("9876543210")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("abcdef")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("ABCDEF")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0123456789ABCDEF")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xl0l0")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xC@B")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("OxCAFE")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xTEXT")).isFalse();
  }

  @Test
  public void fromHexadecimalStringIsCorrect() {

    Integer value = 0xB00BFAB;
    String hexadecimalString = Integer.toHexString(value);

    assertThat(NumberUtils.fromHexadecimalString(formatAsHexadecimalString(hexadecimalString))).isEqualTo(value);
  }

  @Test
  public void fromBlankHexadecimalString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromHexadecimalString("  "))
      .withMessage("Hexadecimal String [  ] is required")
      .withNoCause();
  }

  @Test
  public void fromEmptyHexadecimalString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromHexadecimalString(""))
      .withMessage("Hexadecimal String [] is required")
      .withNoCause();
  }

  @Test
  public void fromNullHexadecimalString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromHexadecimalString(null))
      .withMessage("Hexadecimal String [null] is required")
      .withNoCause();
  }

  @Test
  public void fromInvalidHexadecimalString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromHexadecimalString("0xCAT1NB0X"))
      .withMessage("Hexadecimal String [0xCAT1NB0X] must contain only digits [0-9] and letters [A-F]")
      .withNoCause();
  }

  @Test
  public void fromPrefixNotationHexadecimalString() {

    assertThat(NumberUtils.fromHexadecimalString("0x15")).isEqualTo(21);
    assertThat(NumberUtils.fromHexadecimalString("0x45")).isEqualTo(69);
    assertThat(NumberUtils.fromHexadecimalString("0x63")).isEqualTo(99);
  }

  @Test
  public void getBytes() {

    byte[] expectedValues = { (byte) 0xCA, (byte) 0xFE, (byte) 0xBA, (byte) 0xBE };
    byte[] actualValues = NumberUtils.getBytes(0xCAFEBABE);

    assertThat(actualValues).isNotNull();
    assertThat(actualValues.length).isEqualTo(expectedValues.length);

    int index = 0;

    for (byte expectedValue : expectedValues) {
      assertThat(actualValues[index++]).isEqualTo(expectedValue);
    }
  }

  @Test
  public void isDecimal() {
    assertThat(NumberUtils.isDecimal(Math.PI)).isTrue();
    assertThat(NumberUtils.isDecimal(1.23d)).isTrue();
    assertThat(NumberUtils.isDecimal(12.3d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.1d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.01d)).isTrue();
    assertThat(NumberUtils.isDecimal(1.0001d)).isTrue();
    assertThat(NumberUtils.isDecimal(0.123d)).isTrue();
  }

  @Test
  public void isNotDecimal() {
    assertThat(NumberUtils.isDecimal(0.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(-0.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(1.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(-1.0d)).isFalse();
    assertThat(NumberUtils.isDecimal(100.0d)).isFalse();
  }

  @Test
  public void isDecimalWithDouble() {
    assertThat(NumberUtils.isDecimal(new Double(Math.PI))).isTrue();
  }

  @Test
  public void isDecimalWithFloat() {
    assertThat(NumberUtils.isDecimal(new Float(3.14159f))).isTrue();
  }

  @Test
  public void isNotDecimalWithByte() {
    assertThat(NumberUtils.isDecimal(new Byte((byte) 0))).isFalse();
  }

  @Test
  public void isNotDecimalWithInteger() {
    assertThat(NumberUtils.isDecimal(new Integer(1))).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotDecimalWithNull() {
    assertThat(NumberUtils.isDecimal(null)).isFalse();
  }

  @Test
  public void isEven() {
    assertThat(NumberUtils.isEven(-10)).isTrue();
    assertThat(NumberUtils.isEven(-8)).isTrue();
    assertThat(NumberUtils.isEven(-6)).isTrue();
    assertThat(NumberUtils.isEven(-4)).isTrue();
    assertThat(NumberUtils.isEven(-2)).isTrue();
    assertThat(NumberUtils.isEven(0)).isTrue();
    assertThat(NumberUtils.isEven(2)).isTrue();
    assertThat(NumberUtils.isEven(4)).isTrue();
    assertThat(NumberUtils.isEven(6)).isTrue();
    assertThat(NumberUtils.isEven(8)).isTrue();
    assertThat(NumberUtils.isEven(10)).isTrue();
  }

  @Test
  public void isNotEven() {
    assertThat(NumberUtils.isEven(-11)).isFalse();
    assertThat(NumberUtils.isEven(-9)).isFalse();
    assertThat(NumberUtils.isEven(-7)).isFalse();
    assertThat(NumberUtils.isEven(-5)).isFalse();
    assertThat(NumberUtils.isEven(-3)).isFalse();
    assertThat(NumberUtils.isEven(-1)).isFalse();
    assertThat(NumberUtils.isEven(1)).isFalse();
    assertThat(NumberUtils.isEven(3)).isFalse();
    assertThat(NumberUtils.isEven(5)).isFalse();
    assertThat(NumberUtils.isEven(7)).isFalse();
    assertThat(NumberUtils.isEven(9)).isFalse();
    assertThat(NumberUtils.isEven(11)).isFalse();
  }

  @Test
  public void isBitwiseEven() {
    assertThat(NumberUtils.isBitwiseEven(-10)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-8)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-6)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-4)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(-2)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(0)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(2)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(4)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(6)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(8)).isTrue();
    assertThat(NumberUtils.isBitwiseEven(10)).isTrue();
  }

  @Test
  public void isNotBitwiseEven() {
    assertThat(NumberUtils.isBitwiseEven(-11)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-9)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-7)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-5)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-3)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(-1)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(1)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(3)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(5)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(7)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(9)).isFalse();
    assertThat(NumberUtils.isBitwiseEven(11)).isFalse();
  }

  @Test
  public void isNegative() {
    assertThat(NumberUtils.isNegative(-10.0d)).isTrue();
    assertThat(NumberUtils.isNegative(-1.0d)).isTrue();
    assertThat(NumberUtils.isNegative(-0.01d)).isTrue();
  }

  @Test
  public void isNotNegative() {
    assertThat(NumberUtils.isNegative(-0.0d)).isFalse();
    assertThat(NumberUtils.isNegative(0.0d)).isFalse();
    assertThat(NumberUtils.isNegative(0.01d)).isFalse();
    assertThat(NumberUtils.isNegative(1.0d)).isFalse();
    assertThat(NumberUtils.isNegative(10.0d)).isFalse();
  }

  @Test
  public void isOdd() {
    assertThat(NumberUtils.isOdd(-11)).isTrue();
    assertThat(NumberUtils.isOdd(-9)).isTrue();
    assertThat(NumberUtils.isOdd(-7)).isTrue();
    assertThat(NumberUtils.isOdd(-5)).isTrue();
    assertThat(NumberUtils.isOdd(-3)).isTrue();
    assertThat(NumberUtils.isOdd(-1)).isTrue();
    assertThat(NumberUtils.isOdd(1)).isTrue();
    assertThat(NumberUtils.isOdd(3)).isTrue();
    assertThat(NumberUtils.isOdd(5)).isTrue();
    assertThat(NumberUtils.isOdd(7)).isTrue();
    assertThat(NumberUtils.isOdd(9)).isTrue();
    assertThat(NumberUtils.isOdd(11)).isTrue();
  }

  @Test
  public void isNotOdd() {
    assertThat(NumberUtils.isOdd(-10)).isFalse();
    assertThat(NumberUtils.isOdd(-8)).isFalse();
    assertThat(NumberUtils.isOdd(-6)).isFalse();
    assertThat(NumberUtils.isOdd(-4)).isFalse();
    assertThat(NumberUtils.isOdd(-2)).isFalse();
    assertThat(NumberUtils.isOdd(0)).isFalse();
    assertThat(NumberUtils.isOdd(2)).isFalse();
    assertThat(NumberUtils.isOdd(4)).isFalse();
    assertThat(NumberUtils.isOdd(6)).isFalse();
    assertThat(NumberUtils.isOdd(8)).isFalse();
    assertThat(NumberUtils.isOdd(10)).isFalse();
  }

  @Test
  public void isBitwiseOdd() {
    assertThat(NumberUtils.isBitwiseOdd(-11)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-9)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-7)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-5)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-3)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(-1)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(1)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(3)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(5)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(7)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(9)).isTrue();
    assertThat(NumberUtils.isBitwiseOdd(11)).isTrue();
  }

  @Test
  public void isNotBitwiseOdd() {
    assertThat(NumberUtils.isBitwiseOdd(-10)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-8)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-6)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-4)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(-2)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(0)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(2)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(4)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(6)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(8)).isFalse();
    assertThat(NumberUtils.isBitwiseOdd(10)).isFalse();
  }

  @Test
  public void isPositive() {
    assertThat(NumberUtils.isPositive(0.01d)).isTrue();
    assertThat(NumberUtils.isPositive(1.0d)).isTrue();
    assertThat(NumberUtils.isPositive(10.0d)).isTrue();
  }

  @Test
  public void isNotPositive() {
    assertThat(NumberUtils.isPositive(-10.0d)).isFalse();
    assertThat(NumberUtils.isPositive(-1.0d)).isFalse();
    assertThat(NumberUtils.isPositive(-0.01d)).isFalse();
    assertThat(NumberUtils.isPositive(-0.0d)).isFalse();
    assertThat(NumberUtils.isPositive(0.0d)).isFalse();
  }

  @Test
  public void isWhole() {
    assertThat(NumberUtils.isWhole(-0.0d)).isTrue();
    assertThat(NumberUtils.isWhole(0.0d)).isTrue();
    assertThat(NumberUtils.isWhole(-1.0d)).isTrue();
    assertThat(NumberUtils.isWhole(1.0d)).isTrue();
    assertThat(NumberUtils.isWhole(100.0d)).isTrue();
  }

  @Test
  public void isNotWhole() {
    assertThat(NumberUtils.isWhole(Math.PI)).isFalse();
    assertThat(NumberUtils.isWhole(1.23d)).isFalse();
    assertThat(NumberUtils.isWhole(12.3d)).isFalse();
    assertThat(NumberUtils.isWhole(1.1d)).isFalse();
    assertThat(NumberUtils.isWhole(1.01d)).isFalse();
    assertThat(NumberUtils.isWhole(1.0001d)).isFalse();
    assertThat(NumberUtils.isWhole(0.123d)).isFalse();
  }

  @Test
  public void isWholeWithNumber() {
    assertThat(NumberUtils.isWhole(new Byte((byte) 0))).isTrue();
    assertThat(NumberUtils.isWhole(new Short((short) 256))).isTrue();
    assertThat(NumberUtils.isWhole(new Integer(65536))).isTrue();
    assertThat(NumberUtils.isWhole(new Long(4294967296L))).isTrue();
    assertThat(NumberUtils.isWhole(new BigInteger("1234567890987654321"))).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotWholeWithNumber() {
    assertThat(NumberUtils.isWhole(new Float(3.14159f))).isFalse();
    assertThat(NumberUtils.isWhole(new Double(Math.PI))).isFalse();
    assertThat(NumberUtils.isWhole(new BigDecimal(1.234567890d))).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotWholeWithNull() {
    assertThat(NumberUtils.isWhole(null)).isFalse();
  }

  @Test
  public void isZero() {
    assertThat(NumberUtils.isZero(0.0d)).isTrue();
    assertThat(NumberUtils.isZero(-0.0d)).isTrue();
    assertThat(NumberUtils.isZero(0)).isTrue();
    assertThat(NumberUtils.isZero(-0)).isTrue();
  }

  @Test
  public void isNotZero() {
    assertThat(NumberUtils.isZero(1.0d)).isFalse();
    assertThat(NumberUtils.isZero(10.0d)).isFalse();
    assertThat(NumberUtils.isZero(100.0d)).isFalse();
    assertThat(NumberUtils.isZero(-1.0d)).isFalse();
    assertThat(NumberUtils.isZero(-10.0d)).isFalse();
    assertThat(NumberUtils.isZero(-100.0d)).isFalse();
  }

  @Test
  public void isByteWithByte() {
    assertThat(NumberUtils.isByte(Byte.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isByte((byte) 0)).isTrue();
    assertThat(NumberUtils.isByte((byte) 64)).isTrue();
    assertThat(NumberUtils.isByte(Byte.MAX_VALUE)).isTrue();
  }

  @Test
  public void isByteWithByteValue() {
    assertThat(NumberUtils.isByte(100L)).isTrue();
  }

  @Test
  public void isNotByteWithDouble() {
    assertThat(NumberUtils.isByte(Math.PI)).isFalse();
  }

  @Test
  public void isNotByteWithNull() {
    assertThat(NumberUtils.isByte(null)).isFalse();
  }

  @Test
  public void isNotByteWithOverflowValue() {
    assertThat(NumberUtils.isByte(256)).isFalse();
  }

  @Test
  public void isNotByteWithUnderflowValue() {
    assertThat(NumberUtils.isByte(-255)).isFalse();
  }

  @Test
  public void isShortWithShort() {
    assertThat(NumberUtils.isShort(Short.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isShort((short) 0)).isTrue();
    assertThat(NumberUtils.isShort((short) 1024)).isTrue();
    assertThat(NumberUtils.isShort(Short.MAX_VALUE)).isTrue();
  }

  @Test
  public void isShortWithShortValue() {
    assertThat(NumberUtils.isShort(8192L)).isTrue();
  }

  @Test
  public void isNotShortWithDouble() {
    assertThat(NumberUtils.isShort(Math.PI)).isFalse();
  }

  @Test
  public void isNotShortWithNull() {
    assertThat(NumberUtils.isShort(null)).isFalse();
  }

  @Test
  public void isNotShortWithOverflowValue() {
    assertThat(NumberUtils.isShort(65536)).isFalse();
  }

  @Test
  public void isNotShortWithUnderflowValue() {
    assertThat(NumberUtils.isShort(-65535)).isFalse();
  }

  @Test
  public void isIntegerWithInteger() {
    assertThat(NumberUtils.isInteger(Integer.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isInteger(0)).isTrue();
    assertThat(NumberUtils.isInteger(1048576)).isTrue();
    assertThat(NumberUtils.isInteger(Integer.MAX_VALUE)).isTrue();
  }

  @Test
  public void isIntegerWithIntegerValue() {
    assertThat(NumberUtils.isInteger(1024000000)).isTrue();
  }

  @Test
  public void isNotIntegerWithDouble() {
    assertThat(NumberUtils.isInteger(Math.PI)).isFalse();
  }

  @Test
  public void isNotIntegerWithNull() {
    assertThat(NumberUtils.isInteger(null)).isFalse();
  }

  @Test
  public void isNotIntegerWithOverflowValue() {
    assertThat(NumberUtils.isInteger(4294967296L)).isFalse();
  }

  @Test
  public void isNotIntegerWithUnderflowValue() {
    assertThat(NumberUtils.isInteger(-4294967295L)).isFalse();
  }

  @Test
  public void isLongWithLong() {
    assertThat(NumberUtils.isLong(Long.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isLong(0L)).isTrue();
    assertThat(NumberUtils.isLong(4294967296L)).isTrue();
    assertThat(NumberUtils.isLong(Long.MAX_VALUE)).isTrue();
  }

  @Test
  public void isNotLongWithBigInteger() {
    assertThat(NumberUtils.isLong(new BigInteger("1234567890987654321"))).isFalse();
  }

  @Test
  public void isNotLongWithDouble() {
    assertThat(NumberUtils.isLong(Math.PI)).isFalse();
  }

  @Test
  public void isNotLongWithInteger() {
    assertThat(NumberUtils.isLong(123456789)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotLongWithNull() {
    assertThat(NumberUtils.isLong(null)).isFalse();
  }

  @Test
  public void isFloatWithFloat() {
    assertThat(NumberUtils.isFloat(Float.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isFloat(0.0f)).isTrue();
    assertThat(NumberUtils.isFloat(3.14159f)).isTrue();
    assertThat(NumberUtils.isFloat(Float.MAX_VALUE)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotFloatWithBigDecimal() {
    assertThat(NumberUtils.isFloat(new BigDecimal(3.14159f))).isFalse();
  }

  @Test
  public void isNotFloatWithDouble() {
    assertThat(NumberUtils.isFloat(Double.MIN_VALUE)).isFalse();
    assertThat(NumberUtils.isFloat(Double.MAX_VALUE)).isFalse();
  }

  @Test
  public void isNotFloatWithInteger() {
    assertThat(NumberUtils.isFloat(101)).isFalse();
  }

  @Test
  public void isNotFloatWithNull() {
    assertThat(NumberUtils.isFloat(null)).isFalse();
  }

  @Test
  public void isDoubleWithDouble() {
    assertThat(NumberUtils.isDouble(Double.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isDouble(0d)).isTrue();
    assertThat(NumberUtils.isDouble(Math.PI)).isTrue();
    assertThat(NumberUtils.isDouble(Double.MAX_VALUE)).isTrue();
  }

  @Test
  public void isNotDoubleWithBigDecimal() {
    assertThat(NumberUtils.isDouble(new BigDecimal(Math.PI))).isFalse();
  }

  @Test
  public void isNotDoubleWithFloat() {
    assertThat(NumberUtils.isDouble(3.14159f)).isFalse();
  }

  @Test
  public void isNotDoubleWithLong() {
    assertThat(NumberUtils.isDouble(1234567890987654321L)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isNotDoubleWithNull() {
    assertThat(NumberUtils.isDouble(null)).isFalse();
  }

  @Test
  public void byteValueOfByte() {
    assertThat(NumberUtils.byteValue(8)).isEqualTo((byte) 8);
  }

  @Test
  public void byteValueOfNonByte() {
    assertThat(NumberUtils.byteValue(64.0d)).isEqualTo((byte) 64);
  }

  @Test
  public void byteValueOfNull() {
    assertThat(NumberUtils.byteValue(null)).isEqualTo((byte) 0);
  }

  @Test
  public void shortValueOfShort() {
    assertThat(NumberUtils.shortValue(16)).isEqualTo((short) 16);
  }

  @Test
  public void shortValueOfNonShort() {
    assertThat(NumberUtils.shortValue(128.0d)).isEqualTo((short) 128);
  }

  @Test
  public void shortValueOfNull() {
    assertThat(NumberUtils.shortValue(null)).isEqualTo((short) 0);
  }

  @Test
  public void intValueOfInteger() {
    assertThat(NumberUtils.intValue(32)).isEqualTo(32);
  }

  @Test
  public void intValueOfNonInteger() {
    assertThat(NumberUtils.intValue(3.14159d)).isEqualTo(3);
    assertThat(NumberUtils.intValue(256L)).isEqualTo(256);
    assertThat(NumberUtils.intValue(8.8421)).isEqualTo(8);
  }

  @Test
  public void intValueOfNull() {
    assertThat(NumberUtils.intValue(null)).isEqualTo(0);
  }

  @Test
  public void longValueOfLong() {
    assertThat(NumberUtils.longValue(64L)).isEqualTo(64L);
  }

  @Test
  public void longValueOfNonLong() {
    assertThat(NumberUtils.longValue(512.0d)).isEqualTo(512L);
  }

  @Test
  public void longValueOfNull() {
    assertThat(NumberUtils.longValue(null)).isEqualTo(0L);
  }

  @Test
  public void floatValueOfFloat() {
    assertThat(NumberUtils.floatValue(3.14159f)).isEqualTo(3.14159f);
  }

  @Test
  public void floatValueOfNonFloat() {
    assertThat(NumberUtils.floatValue(1024)).isEqualTo(1024.0f);
  }

  @Test
  public void floatValueOfNull() {
    assertThat(NumberUtils.floatValue(null)).isEqualTo(0.0f);
  }

  @Test
  public void doubleValueOfDouble() {
    assertThat(NumberUtils.doubleValue(Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void doubleValueOfNonDouble() {
    assertThat(NumberUtils.doubleValue(2)).isEqualTo(2.0d);
    assertThat(NumberUtils.doubleValue(2.0f)).isEqualTo(2.0d);
  }

  @Test
  public void doubleValueOfNull() {
    assertThat(NumberUtils.doubleValue(null)).isEqualTo(0.0d);
  }

  @Test
  public void valueOfByte() {
    assertThat(NumberUtils.valueOf((byte) 8)).isEqualTo((byte) 8);
  }

  @Test
  public void valueOfNullByte() {
    assertThat(NumberUtils.valueOf((Byte) null)).isEqualTo((byte) 0);
  }

  @Test
  public void valueOfShort() {
    assertThat(NumberUtils.valueOf((short) 16)).isEqualTo((short) 16);
  }

  @Test
  public void valueOfNullShort() {
    assertThat(NumberUtils.valueOf((Short) null)).isEqualTo((short) 0);
  }

  @Test
  public void valueOfInteger() {
    assertThat(NumberUtils.valueOf(32)).isEqualTo(32);
  }

  @Test
  public void valueOfNullInteger() {
    assertThat(NumberUtils.valueOf((Integer) null)).isEqualTo(0);
  }

  @Test
  public void valueOfLong() {
    assertThat(NumberUtils.valueOf(64L)).isEqualTo(64L);
  }

  @Test
  public void valueOfNullLong() {
    assertThat(NumberUtils.valueOf((Long) null)).isEqualTo(0L);
  }

  @Test
  public void valueOfFloat() {
    assertThat(NumberUtils.valueOf(32.0f)).isEqualTo(32.0f);
  }

  @Test
  public void valueOfNullFloat() {
    assertThat(NumberUtils.valueOf((Float) null)).isEqualTo(0.0f);
  }

  @Test
  public void valueOfDouble() {
    assertThat(NumberUtils.valueOf(Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void valueOfNullDouble() {
    assertThat(NumberUtils.valueOf((Double) null)).isEqualTo(0.0d);
  }
}
