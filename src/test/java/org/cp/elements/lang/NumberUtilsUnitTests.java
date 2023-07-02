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
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.IntStream;

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
public class NumberUtilsUnitTests {

  private String formatAsHexadecimalString(String number) {
    return number.startsWith(NumberUtils.HEXADECIMAL_PREFIX_NOTATION) ? number
      : String.format("%1$s%2$s", NumberUtils.HEXADECIMAL_PREFIX_NOTATION, number);
  }

  @Test
  public void isBinaryString() {

    assertThat(NumberUtils.isBinaryString("0")).isTrue();
    assertThat(NumberUtils.isBinaryString("1")).isTrue();
    assertThat(NumberUtils.isBinaryString("01")).isTrue();
    assertThat(NumberUtils.isBinaryString("10")).isTrue();
    assertThat(NumberUtils.isBinaryString("0001")).isTrue();
    assertThat(NumberUtils.isBinaryString("0010")).isTrue();
    assertThat(NumberUtils.isBinaryString("0100")).isTrue();
    assertThat(NumberUtils.isBinaryString("1000")).isTrue();
    assertThat(NumberUtils.isBinaryString("1010")).isTrue();
    assertThat(NumberUtils.isBinaryString("10001010")).isTrue();
    assertThat(NumberUtils.isBinaryString("b0")).isTrue();
    assertThat(NumberUtils.isBinaryString("b01")).isTrue();
    assertThat(NumberUtils.isBinaryString("b010")).isTrue();
    assertThat(NumberUtils.isBinaryString("b101")).isTrue();
    assertThat(NumberUtils.isBinaryString("b10101")).isTrue();
  }

  @Test
  public void integerToBinaryStringIsBinaryString() {

    IntStream.range(0, 100).forEach(number ->
      assertThat(NumberUtils.isBinaryString(Integer.toBinaryString(number))).isTrue());

    assertThat(NumberUtils.isBinaryString(Integer.toBinaryString(1248163264))).isTrue();
  }

  @Test
  public void isNotBinaryString() {

    assertThat(NumberUtils.isBinaryString("O")).isFalse();
    assertThat(NumberUtils.isBinaryString("1O")).isFalse();
    assertThat(NumberUtils.isBinaryString("0l1O")).isFalse();
    assertThat(NumberUtils.isBinaryString("1234567890")).isFalse();
    assertThat(NumberUtils.isBinaryString("b12")).isFalse();
    assertThat(NumberUtils.isBinaryString("bCAFE")).isFalse();
    assertThat(NumberUtils.isBinaryString("CAFEBABE")).isFalse();
    assertThat(NumberUtils.isBinaryString("0xCAFEBABE")).isFalse();
    assertThat(NumberUtils.isBinaryString("TEXT")).isFalse();
  }

  @Test
  public void invalidValuesAreNotBinaryStrings() {

    Arrays.asList(null, "", "  ").forEach(value ->
      assertThat(NumberUtils.isBinaryString(value)).isFalse());
  }

  @Test
  public void fromBinaryStringIsCorrect() {

    Integer value = 1248163264;
    String binaryString = Integer.toBinaryString(value);

    assertThat(NumberUtils.fromBinaryString(binaryString)).isEqualTo(value);
    assertThat(NumberUtils.fromBinaryString("0")).isZero();
    assertThat(NumberUtils.fromBinaryString("01")).isOne();
  }

  @Test
  public void fromBinaryStringWithPrefixIsCorrect() {

    assertThat(NumberUtils.fromBinaryString("b0001")).isEqualTo(1);
    assertThat(NumberUtils.fromBinaryString("b0010")).isEqualTo(2);
    assertThat(NumberUtils.fromBinaryString("b0100")).isEqualTo(4);
    assertThat(NumberUtils.fromBinaryString("b1000")).isEqualTo(8);
    assertThat(NumberUtils.fromBinaryString("b1001")).isEqualTo(9);
    assertThat(NumberUtils.fromBinaryString("b1010")).isEqualTo(10);
    assertThat(NumberUtils.fromBinaryString("b1011")).isEqualTo(11);
    assertThat(NumberUtils.fromBinaryString("b1100")).isEqualTo(12);
    assertThat(NumberUtils.fromBinaryString("b1110")).isEqualTo(14);
    assertThat(NumberUtils.fromBinaryString("b1111")).isEqualTo(15);
    assertThat(NumberUtils.fromBinaryString("b10000")).isEqualTo(16);
    assertThat(NumberUtils.fromBinaryString("b00110110")).isEqualTo(54);
  }

  @Test
  public void fromHexBasedBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("B00BFAB5"))
      .withMessage("Binary String [B00BFAB5] must contain only 1s and 0s")
      .withNoCause();
  }

  @Test
  public void fromIllegalBinaryString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromBinaryString("1Ol01O"))
      .withMessage("Binary String [1Ol01O] must contain only 1s and 0s")
      .withNoCause();
  }

  @Test
  public void fromInvalidBinaryString() {

    Arrays.asList(null, "", "  ").forEach(value ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> NumberUtils.fromBinaryString(value))
        .withMessage("Binary String [%s] is required", value)
        .withNoCause());
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
    assertThat(NumberUtils.isHexadecimalString("0x0123456789AaBbCcDdEeFf")).isTrue();
  }

  @Test
  public void integerToHexStringIsHexadecimalString() {

    IntStream.range(0, 100).forEach(number ->
      assertThat(NumberUtils.isHexadecimalString(formatAsHexadecimalString(Integer.toHexString(number)))).isTrue());
  }

  @Test
  public void isNotHexadecimalString() {

    assertThat(NumberUtils.isHexadecimalString("0123456789")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("9876543210")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("abcdef")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("ABCDEF")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0123456789ABCDEF")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0123456789AaBbCcDdEeFf")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xl0l0")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xC@B")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("OxCAFE")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("0xTEXT")).isFalse();
  }

  @Test
  public void binaryStringIsNotHexadecimalString() {

    assertThat(NumberUtils.isHexadecimalString("1011")).isFalse();
    assertThat(NumberUtils.isHexadecimalString("10101010")).isFalse();
  }

  @Test
  public void integerStringIsNotHexadecimalString() {
    assertThat(NumberUtils.isHexadecimalString("123456789")).isFalse();
  }

  @Test
  public void invalidValuesAreNotHexadecimalString() {

    Arrays.asList(null, "", "  ").forEach(value ->
      assertThat(NumberUtils.isHexadecimalString(value)).isFalse());
  }

  @Test
  public void fromHexadecimalStringIsCorrect() {

    Integer value = 0xB00BFAB;
    String hexadecimalString = Integer.toHexString(value);

    assertThat(NumberUtils.fromHexadecimalString(formatAsHexadecimalString(hexadecimalString))).isEqualTo(value);
  }

  @Test
  public void fromIllegalHexadecimalString() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> NumberUtils.fromHexadecimalString("0xCAT1NB0X"))
      .withMessage("Hexadecimal String [0xCAT1NB0X] must contain only digits [0-9] and letters [A-F]")
      .withNoCause();
  }

  @Test
  public void fromInvalidHexadecimalString() {

    Arrays.asList(null, "", "  ").forEach(value ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> NumberUtils.fromHexadecimalString(value))
        .withMessage("Hexadecimal String [%s] is required", value)
        .withNoCause());
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
    assertThat(actualValues).hasSameSizeAs(expectedValues);

    int index = 0;

    for (byte expectedValue : expectedValues) {
      assertThat(actualValues[index++]).isEqualTo(expectedValue);
    }

    assertThat(index).isEqualTo(4);
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
  public void isDecimalWithPrimitiveFloat() {
    assertThat(NumberUtils.isDecimal(12.345f)).isTrue();
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
  public void isNotDecimalWithPrimitiveInteger() {
    assertThat(NumberUtils.isDecimal(2)).isFalse();
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
  public void isDecimalWithInteger() {
    assertThat(NumberUtils.isDecimal(new Integer(1))).isFalse();
  }

  @Test
  public void isDecimalWithLong() {
    assertThat(NumberUtils.isDecimal(new Long(9876543210L))).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void isDecimalWithNullIsNullSafe() {
    assertThat(NumberUtils.isDecimal(null)).isFalse();
  }

  @Test
  public void isEven() {

    for (int number = -100; number <= 100; number += 2) {
      assertThat(NumberUtils.isEven(number)).isTrue();
    }
  }

  @Test
  public void isNotEven() {

    for (int number = -99; number < 100; number += 2) {
      assertThat(NumberUtils.isEven(number)).isFalse();
    }
  }

  @Test
  public void isBitwiseEven() {

    for (int number = -100; number <= 100; number += 2) {
      assertThat(NumberUtils.isBitwiseEven(number)).isTrue();
    }
  }

  @Test
  public void isNotBitwiseEven() {

    for (int number = -99; number < 100; number += 2) {
      assertThat(NumberUtils.isBitwiseEven(number)).isFalse();
    }
  }

  @Test
  public void isNegative() {

    IntStream.range(-100, -1).forEach(number ->
      assertThat(NumberUtils.isNegative(number)).isTrue());
  }

  @Test
  public void isNotNegative() {

    IntStream.range(0, 100).forEach(number ->
      assertThat(NumberUtils.isNegative(number)).isFalse());
  }

  @Test
  public void isOdd() {

    for (int number = -99; number < 100; number += 2) {
      assertThat(NumberUtils.isOdd(number)).isTrue();
    }
  }

  @Test
  public void isNotOdd() {

    for (int number = -100; number <= 100; number += 2) {
      assertThat(NumberUtils.isOdd(number)).isFalse();
    }
  }

  @Test
  public void isBitwiseOdd() {

    for (int number = -99; number < 100; number += 2) {
      assertThat(NumberUtils.isBitwiseOdd(number)).isTrue();
    }
  }

  @Test
  public void isNotBitwiseOdd() {

    for (int number = -100; number <= 100; number += 2) {
      assertThat(NumberUtils.isBitwiseOdd(number)).isFalse();
    }
  }

  @Test
  public void isPositive() {

    IntStream.range(1, 100).forEach(number ->
      assertThat(NumberUtils.isPositive(number)).isTrue());
  }

  @Test
  public void isNotPositive() {

    IntStream.range(-100, 0).forEach(number ->
      assertThat(NumberUtils.isPositive(number)).isFalse());
  }

  @Test
  public void isPrimeNumber() {

    IntStream.of(
        2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101,
        103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223,
        227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349,
        353, 359, 367, 373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479,
        487, 491, 499, 503, 509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619,
        631, 641, 643, 647, 653, 659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769,
        773, 787, 797, 809, 811, 821, 823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929,
        937, 941, 947, 953, 967, 971, 977, 983, 991, 997)
      .forEach(number -> assertThat(NumberUtils.isPrime(number)).isTrue());
  }

  @Test
  public void isNotPrimeNumber() {

    Set<Integer> primeNumbers  = new HashSet<>(Arrays.asList(
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103,
      107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229,
      233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293, 307, 311, 313, 317, 331, 337, 347, 349, 353, 359, 367,
      373, 379, 383, 389, 397, 401, 409, 419, 421, 431, 433, 439, 443, 449, 457, 461, 463, 467, 479, 487, 491, 499, 503,
      509, 521, 523, 541, 547, 557, 563, 569, 571, 577, 587, 593, 599, 601, 607, 613, 617, 619, 631, 641, 643, 647, 653,
      659, 661, 673, 677, 683, 691, 701, 709, 719, 727, 733, 739, 743, 751, 757, 761, 769, 773, 787, 797, 809, 811, 821,
      823, 827, 829, 839, 853, 857, 859, 863, 877, 881, 883, 887, 907, 911, 919, 929, 937, 941, 947, 953, 967, 971, 977,
      983, 991, 997));

    IntStream.range(0, 1000)
      .filter(number -> !primeNumbers.contains(number))
      .forEach(number -> assertThat(NumberUtils.isPrime(number))
        .describedAs("Number [%d] is not prime", number)
        .isFalse());
  }

  @Test
  public void evenNumbersAreNotPrimeNumber() {

    for (int number = 4; number <= 1000; number += 2) {
      assertThat(NumberUtils.isPrime(number))
        .describedAs("Number [%d] is prime", number)
        .isFalse();
    }
  }

  @Test
  public void oddNumbersByThreeAreNotPrimeNumber() {

    for (int number = 6; number <= 1000; number += 2) {
      assertThat(NumberUtils.isPrime(number))
        .describedAs("Number [%d] is prime", number)
        .isFalse();
    }
  }

  @Test
  public void oneIsNotPrime() {
    assertThat(NumberUtils.isPrime(1)).isFalse();
  }

  @Test
  public void zeroIsNotPrime() {
    assertThat(NumberUtils.isPrime(0)).isFalse();
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
  @SuppressWarnings("all")
  public void isWholeWithFloatingPointNumber() {

    assertThat(NumberUtils.isWhole(new Float(3.14159f))).isFalse();
    assertThat(NumberUtils.isWhole(new Double(Math.PI))).isFalse();
    assertThat(NumberUtils.isWhole(new BigDecimal(1.234567890d))).isFalse();
  }

  @Test
  public void isWholeWithIntegralNumber() {

    assertThat(NumberUtils.isWhole(new Byte((byte) 0))).isTrue();
    assertThat(NumberUtils.isWhole(new Short((short) 256))).isTrue();
    assertThat(NumberUtils.isWhole(new Integer(65536))).isTrue();
    assertThat(NumberUtils.isWhole(new Long(4294967296L))).isTrue();
    assertThat(NumberUtils.isWhole(new BigInteger("1234567890987654321"))).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void isWholeWithNullIsNullSafe() {
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
    assertThat(NumberUtils.isZero(0.1)).isFalse();
    assertThat(NumberUtils.isZero(-0.01)).isFalse();
    assertThat(NumberUtils.isZero(0.001)).isFalse();
    assertThat(NumberUtils.isZero(-1.0d)).isFalse();
    assertThat(NumberUtils.isZero(-10.0d)).isFalse();
    assertThat(NumberUtils.isZero(-100.0d)).isFalse();
  }

  @Test
  public void isByteWithByte() {

    assertThat(NumberUtils.isByte(Byte.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isByte((byte) 0)).isTrue();
    assertThat(NumberUtils.isByte((byte) 1)).isTrue();
    assertThat(NumberUtils.isByte((byte) 2)).isTrue();
    assertThat(NumberUtils.isByte((byte) 4)).isTrue();
    assertThat(NumberUtils.isByte((byte) 8)).isTrue();
    assertThat(NumberUtils.isByte((byte) 16)).isTrue();
    assertThat(NumberUtils.isByte((byte) 32)).isTrue();
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
    assertThat(NumberUtils.isShort((short) 2048)).isTrue();
    assertThat(NumberUtils.isShort((short) 4096)).isTrue();
    assertThat(NumberUtils.isShort((short) 8192)).isTrue();
    assertThat(NumberUtils.isShort((short) 16384)).isTrue();
    assertThat(NumberUtils.isShort(Short.MAX_VALUE)).isTrue();
  }

  @Test
  public void isShortWithShortValue() {
    assertThat(NumberUtils.isShort(20_000L)).isTrue();
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
    assertThat(NumberUtils.isInteger(1_048_576)).isTrue();
    assertThat(NumberUtils.isInteger(1_073_741_824)).isTrue();
    assertThat(NumberUtils.isInteger(Integer.MAX_VALUE)).isTrue();
  }

  @Test
  public void isIntegerWithIntegerValue() {
    assertThat(NumberUtils.isInteger(2_048_000_000)).isTrue();
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
    assertThat(NumberUtils.isInteger(4_294_967_296L)).isFalse();
  }

  @Test
  public void isNotIntegerWithUnderflowValue() {
    assertThat(NumberUtils.isInteger(-4_294_967_295L)).isFalse();
  }

  @Test
  public void isLongWithLong() {

    assertThat(NumberUtils.isLong(Long.MIN_VALUE)).isTrue();
    assertThat(NumberUtils.isLong(0L)).isTrue();
    assertThat(NumberUtils.isLong(4_294_967_296L)).isTrue();
    assertThat(NumberUtils.isLong(Long.MAX_VALUE)).isTrue();
  }

  @Test
  public void isNotLongWithBigDecimal() {
    assertThat(NumberUtils.isLong(new BigDecimal("123456789.9876543210"))).isFalse();
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
  public void isNotDoubleWithBigInteger() {
    assertThat(NumberUtils.isDouble(new BigInteger("1234567890"))).isFalse();
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
    assertThat(NumberUtils.byteValue(null)).isZero();
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
    assertThat(NumberUtils.shortValue(null)).isZero();
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
    assertThat(NumberUtils.intValue(null)).isZero();
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
    assertThat(NumberUtils.longValue(null)).isZero();
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
    assertThat(NumberUtils.floatValue(null)).isZero();
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
    assertThat(NumberUtils.doubleValue(null)).isZero();
  }

  @Test
  public void valueOfByte() {
    assertThat(NumberUtils.valueOf((byte) 8)).isEqualTo((byte) 8);
  }

  @Test
  public void valueOfNullByte() {
    assertThat(NumberUtils.valueOf((Byte) null)).isZero();
  }

  @Test
  public void valueOfShort() {
    assertThat(NumberUtils.valueOf((short) 16)).isEqualTo((short) 16);
  }

  @Test
  public void valueOfNullShort() {
    assertThat(NumberUtils.valueOf((Short) null)).isZero();
  }

  @Test
  public void valueOfInteger() {
    assertThat(NumberUtils.valueOf(32)).isEqualTo(32);
  }

  @Test
  public void valueOfNullInteger() {
    assertThat(NumberUtils.valueOf((Integer) null)).isZero();
  }

  @Test
  public void valueOfLong() {
    assertThat(NumberUtils.valueOf(64L)).isEqualTo(64L);
  }

  @Test
  public void valueOfNullLong() {
    assertThat(NumberUtils.valueOf((Long) null)).isZero();
  }

  @Test
  public void valueOfFloat() {
    assertThat(NumberUtils.valueOf(32.0f)).isEqualTo(32.0f);
  }

  @Test
  public void valueOfNullFloat() {
    assertThat(NumberUtils.valueOf((Float) null)).isZero();
  }

  @Test
  public void valueOfDouble() {
    assertThat(NumberUtils.valueOf(Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void valueOfNullDouble() {
    assertThat(NumberUtils.valueOf((Double) null)).isZero();
  }
}
