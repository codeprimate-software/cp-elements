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
package org.cp.elements.enums;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;
import java.util.Locale;
import java.util.Optional;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.MathUtils;
import org.cp.elements.lang.StringUtils;

/**
 * Unit Tests for {@link LengthUnit}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.enums.LengthUnit
 * @since 1.0.0
 */
public class LengthUnitUnitTests {

  @Test
  public void defaultLengthUnitIsCorrect() {

    // USA
    //assertThat(LengthUnit.getDefault()).isEqualTo(LengthUnit.FOOT);

    assertThat(LengthUnit.getDefault())
      .isEqualTo(Optional.of(Locale.getDefault().getCountry())
        .filter(Locale.US.getCountry()::equals)
        .map(it -> LengthUnit.FOOT)
        .orElse(LengthUnit.METER));
  }

  @Test
  public void valueOfReturnsLengthUnit() {

    Arrays.stream(LengthUnit.values()).forEach(it ->
      assertThat(LengthUnit.valueOf(it.name())).isEqualTo(it));
  }

  @Test
  public void valueOfAbbreviationsReturnsLengthUnit() {

    Arrays.stream(LengthUnit.values()).forEach(it ->
      assertThat(LengthUnit.valueOfAbbreviation(it.getAbbreviation())).isEqualTo(it));
  }

  @Test
  public void valueOfInvalidAbbreviationsReturnsNull() {

    assertThat(LengthUnit.valueOfAbbreviation("")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("  ")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("1/4INCH")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("1/2in")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("IN")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("inches")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("foot")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("FEET")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("yrd")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("MI")).isNull();
    assertThat(LengthUnit.valueOfAbbreviation("Mile")).isNull();
  }

  @Test
  public void valueOfNullAbbreviationIsNullSafeAndReturnsNull() {
    assertThat(LengthUnit.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfNamesReturnsLengthUnit() {

    Arrays.stream(LengthUnit.values()).forEach(it ->
      assertThat(LengthUnit.valueOfName(it.name())).isEqualTo(it));
  }

  @Test
  public void valueOfInvalidNamesReturnsNull() {

    assertThat(LengthUnit.valueOfName("")).isNull();
    assertThat(LengthUnit.valueOfName("  ")).isNull();
    assertThat(LengthUnit.valueOfName("PeakOMeter")).isNull();
    assertThat(LengthUnit.valueOfName("PeekOMeter")).isNull();
    assertThat(LengthUnit.valueOfName("micro")).isNull();
    assertThat(LengthUnit.valueOfName("MEETR")).isNull();
    assertThat(LengthUnit.valueOfName("KillOMeter")).isNull();
    assertThat(LengthUnit.valueOfName("INC")).isNull();
    assertThat(LengthUnit.valueOfName("bigfoot")).isNull();
    assertThat(LengthUnit.valueOfName("feet")).isNull();
    assertThat(LengthUnit.valueOfName("YardAge")).isNull();
    assertThat(LengthUnit.valueOfName("Yardstick")).isNull();
    assertThat(LengthUnit.valueOfName("MILEage")).isNull();
  }

  @Test
  public void valueOfNullNameIsNullSafeAndReturnsNull() {
    assertThat(LengthUnit.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfUpperAndLowerCaseNamesReturnsLengthUnit() {

    assertThat(LengthUnit.valueOfName(LengthUnit.NANOMETER.name())).isEqualTo(LengthUnit.NANOMETER);
    assertThat(LengthUnit.valueOfName("Micrometer")).isEqualTo(LengthUnit.MICROMETER);
    assertThat(LengthUnit.valueOfName("MilliMeter")).isEqualTo(LengthUnit.MILLIMETER);
    assertThat(LengthUnit.valueOfName("CentiMETER")).isEqualTo(LengthUnit.CENTIMETER);
    assertThat(LengthUnit.valueOfName("DECImeter")).isEqualTo(LengthUnit.DECIMETER);
    assertThat(LengthUnit.valueOfName("MeTeR")).isEqualTo(LengthUnit.METER);
    assertThat(LengthUnit.valueOfName("INch")).isEqualTo(LengthUnit.INCH);
    assertThat(LengthUnit.valueOfName("fOOt")).isEqualTo(LengthUnit.FOOT);
    assertThat(LengthUnit.valueOfName("YarD")).isEqualTo(LengthUnit.YARD);
    assertThat(LengthUnit.valueOfName("MILE")).isEqualTo(LengthUnit.MILE);
  }

  @Test
  public void pluralizedNamesAreCorrect() {

    Arrays.stream(LengthUnit.values())
      .filter(it -> !Arrays.asList(LengthUnit.FOOT, LengthUnit.INCH).contains(it))
      .forEach(it -> assertThat(it.getPluralName()).isEqualTo(it.name().concat("S")));
  }

  @Test
  public void pluralizedNamesForInchesAndFeetAreCorrect() {

    assertThat(LengthUnit.INCH.getPluralName()).isEqualTo("INCHES");
    assertThat(LengthUnit.FOOT.getPluralName()).isEqualTo("FEET");
  }

  @Test
  @SuppressWarnings("all")
  public void meterConversionFactorIsCorrect() {

    double tenInchesToMeters =
      MathUtils.truncateToPrecision(10.0d * LengthUnit.INCH.getMeterConversionFactor(), 3);

    assertThat(tenInchesToMeters).isEqualTo(0.254d);

    double twelveFeetToMeters =
      MathUtils.truncateToPrecision(12.0d * LengthUnit.FOOT.getMeterConversionFactor(),4);

    assertThat(twelveFeetToMeters).isEqualTo(3.6576d);

    assertThat(100.0d * LengthUnit.YARD.getMeterConversionFactor()).isEqualTo(91.44d);
    assertThat(5.0d * LengthUnit.MILE.getMeterConversionFactor()).isEqualTo(8046.7d);
    assertThat(1.0d * LengthUnit.METER.getMeterConversionFactor()).isOne();
    assertThat(10.0d * LengthUnit.METER.getMeterConversionFactor()).isEqualTo(10.0d);
    assertThat(100.0d * LengthUnit.METER.getMeterConversionFactor()).isEqualTo(100.0d);
    assertThat(10.0d * LengthUnit.DECAMETER.getMeterConversionFactor()).isEqualTo(100.0d);
    assertThat(100.0d * LengthUnit.HECTOMETER.getMeterConversionFactor()).isEqualTo(10_000.0d);
    assertThat(1_000.0d * LengthUnit.KILOMETER.getMeterConversionFactor()).isEqualTo(1_000_000.0d);
    assertThat(5.0d * LengthUnit.DECIMETER.getMeterConversionFactor()).isEqualTo(0.5d);
    assertThat(10.0d * LengthUnit.DECIMETER.getMeterConversionFactor()).isEqualTo(1.0d);
    assertThat(10.0d * LengthUnit.CENTIMETER.getMeterConversionFactor()).isEqualTo(0.1d);
    assertThat(100.0d * LengthUnit.CENTIMETER.getMeterConversionFactor()).isEqualTo(1.0d);
    assertThat(10.0d * LengthUnit.MILLIMETER.getMeterConversionFactor()).isEqualTo(0.01d);
    assertThat(100.0d * LengthUnit.MILLIMETER.getMeterConversionFactor()).isEqualTo(0.1d);
    assertThat(1000.0d * LengthUnit.MILLIMETER.getMeterConversionFactor()).isEqualTo(1.0d);
  }

  @Test
  public void toStringReturnsName() {

    Arrays.stream(LengthUnit.values()).forEach(it ->
      assertThat(it.toString()).isEqualTo(StringUtils.capitalize(it.name().toLowerCase())));
  }
}
