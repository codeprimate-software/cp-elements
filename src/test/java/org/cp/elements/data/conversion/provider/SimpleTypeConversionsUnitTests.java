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
package org.cp.elements.data.conversion.provider;

import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.lang.LangExtensions.assertThat;
import static org.cp.elements.lang.ThrowableAssertions.assertThatThrowableOfType;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.time.Instant;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.conversion.ConversionException;
import org.cp.elements.security.model.User;
import org.cp.elements.util.ArrayUtils;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Unit Tests for {@link SimpleTypeConversions}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.provider.SimpleTypeConversions
 * @since 1.0.0
 */
public class SimpleTypeConversionsUnitTests {

  @Test
  public void findByTargetType() {

    for (SimpleTypeConversions conversion : SimpleTypeConversions.values()) {
      assertThat(conversion.getTargetType()).isNotNull();
      assertThat(conversion.conversionFunction()).isNotNull();
      assertThat(SimpleTypeConversions.findBy(conversion.getTargetType())).isEqualTo(conversion);
    }
  }

  @Test
  public void findByInvalidTargetType() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleTypeConversions.findBy(Instant.class))
      .withMessage("No SimpleTypeConversion exists for target type [%s]", Instant.class.getName())
      .withNoCause();
  }

  @Test
  public void convertsStringToBigDecimal() {
    assertThat(SimpleTypeConversions.BIG_DECIMAL_CONVERTER.convert("3.14159"))
      .isEqualTo(new BigDecimal("3.14159"));
  }

  @Test
  public void convertsStringToBigInteger() {
    assertThat(SimpleTypeConversions.BIG_INTEGER_CONVERTER.convert("123456789"))
      .isEqualTo(new BigInteger("123456789"));
  }

  @Test
  public void convertsStringToBooleanTrue() {

    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("true")).isTrue();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("True")).isTrue();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("TRUE")).isTrue();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert(" TrUe  ")).isTrue();
  }

  @Test
  public void convertsStringToBooleanFalse() {

    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("false")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("!true")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("yes")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("no")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("1")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("0")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert(" ")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("null")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert("nil")).isFalse();
    assertThat(SimpleTypeConversions.BOOLEAN_CONVERTER.convert(null)).isFalse();
  }

  @Test
  public void convertsBinaryStringToByte() {

    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("b10")).isEqualTo((byte) 2);
    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("b1010")).isEqualTo((byte) 10);
  }

  @Test
  public void convertsDecimalStringToByte() {

    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("10")).isEqualTo((byte) 10);
    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("45")).isEqualTo((byte) 45);
    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("64")).isEqualTo((byte) 64);
  }

  @Test
  public void convertsHexStringToByte() {

    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("0x45")).isEqualTo((byte) 69);
    assertThat(SimpleTypeConversions.BYTE_CONVERTER.convert("0x63")).isEqualTo((byte) 99);
  }

  @Test
  public void convertInvalidBinaryStringToByte() {

    // The number is NOT binary, it is ten thousand one hundred and one.
    // A binary number requires the binary number prefix notation 'b' as in 'b10101'.
    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> SimpleTypeConversions.BYTE_CONVERTER.convert("10101"))
      .havingMessage("Cannot convert [10101] into a Byte")
      .causedBy(NumberFormatException.class)
      .havingMessageStartingWith("Value out of range")
      .withNoCause();
  }

  @Test
  public void convertInvalidHexStringToByte() {

    Arrays.stream(ArrayUtils.asArray("Ox0010", "0XAF", "x0011")).forEach(value ->
      assertThatThrowableOfType(ConversionException.class)
        .isThrownBy(args -> SimpleTypeConversions.BYTE_CONVERTER.convert(value))
        .havingMessage("Cannot convert [%s] into a Byte", value)
        .causedBy(NumberFormatException.class)
        .havingMessageContaining("%s", value)
        .withNoCause());
  }

  @Test
  public void convertsBinaryStringToShort() {
    assertThat(SimpleTypeConversions.SHORT_CONVERTER.convert("b0101001001111000")).isEqualTo((short) 21112);
  }

  @Test
  public void convertsDecimalStringToShort() {

    assertThat(SimpleTypeConversions.SHORT_CONVERTER.convert("8192")).isEqualTo((short) 8192);
    assertThat(SimpleTypeConversions.SHORT_CONVERTER.convert("16_384")).isEqualTo((short) 16384);
  }

  @Test
  public void convertHexStringToShort() {
    assertThat(SimpleTypeConversions.SHORT_CONVERTER.convert("0x5E4e")).isEqualTo((short) 24142);
  }

  @Test
  public void convertBinaryStringToInteger() {
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("b111101100000101110101010")).isEqualTo(16_124_842);
  }

  @Test
  public void convertsDecimalStringToInteger() {

    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("1024000")).isEqualTo(1024000);
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("2_048_000")).isEqualTo(2048000);
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("4_096000")).isEqualTo(4096000);
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("-100")).isEqualTo(-100);
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("-200_000")).isEqualTo(-200_000);
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("+1")).isEqualTo(1);
  }

  @Test
  public void convertHexStringToInteger() {
    assertThat(SimpleTypeConversions.INTEGER_CONVERTER.convert("0x311D4E0")).isEqualTo(51_500_256);
  }

  @Test
  public void convertInvalidStringToInteger() {

    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> SimpleTypeConversions.INTEGER_CONVERTER.convert("*12345"))
      .havingMessageContaining("Cannot convert [*12345] into an Integer")
      .causedBy(NumberFormatException.class)
      .havingMessageContaining("*12345")
      .withNoCause();
  }

  @Test
  public void convertsStringToLong() {
    assertThat(SimpleTypeConversions.LONG_CONVERTER.convert("8192000000")).isEqualTo(8_192_000_000L);
  }

  @Test
  public void convertsStringToFloat() {
    assertThat(SimpleTypeConversions.FLOAT_CONVERTER.convert("3.14159")).isEqualTo(3.14159f);
  }

  @Test
  public void convertsStringToDouble() {
    assertThat(SimpleTypeConversions.DOUBLE_CONVERTER.convert(String.valueOf(Math.PI))).isEqualTo(Math.PI);
  }

  @Test
  public void convertInvalidStringToDouble() {

    assertThatThrowableOfType(ConversionException.class)
      .isThrownBy(args -> SimpleTypeConversions.DOUBLE_CONVERTER.convert("3.14.159"))
      .havingMessage("Cannot convert [3.14.159] into a Double")
      .causedBy(NumberFormatException.class)
      .havingMessageContaining("multiple points")
      .withNoCause();
  }

  @Test
  public void convertsStringToString() {

    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert("test")).isEqualTo("test");
    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert("mock")).isEqualTo("mock");
    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert("null")).isEqualTo("null");
  }

  @Test
  public void convertsObjectToString() {

    User<Integer> jonDoe = TestUser.as("jonDoe");

    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert(jonDoe)).isEqualTo(String.format("%s.%s(jonDoe)",
      this.getClass().getSimpleName(), jonDoe.getClass().getSimpleName()));
  }

  @Test
  public void convertsCharacterToString() {
    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert('X')).isEqualTo("X");
  }

  @Test
  public void convertsBooleanToString() {

    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert(true)).isEqualTo("true");
    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert(Boolean.FALSE)).isEqualTo("false");
  }

  @Test
  public void convertsNullToString() {
    assertThat(SimpleTypeConversions.STRING_CONVERTER.convert(null)).isEqualTo("null");
  }

  @Getter
  @ToString(of = "name", includeFieldNames = false)
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser implements User<Integer> {

    @Setter
    private Integer id;

    @lombok.NonNull
    private final String name;

  }
}
