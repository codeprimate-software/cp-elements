/*
 * Copyright 2016 Author or Authors.
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

import org.cp.elements.lang.StringUtils;
import org.junit.Test;

/**
 * Unit tests for {@link Distance}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.enums.Distance
 * @since 1.0.0
 */
public class DistanceTests {

  @Test
  public void getDefaultDistanceIsCorrect() {

    // US
    //assertThat(Distance.getDefault()).isEqualTo(Distance.FOOT);

    assertThat(Distance.getDefault())
      .isEqualTo(Optional.of(Locale.getDefault().getCountry())
        .filter(Locale.US.getCountry()::equals)
        .map(it -> Distance.FOOT)
        .orElse(Distance.METER));
  }

  @Test
  public void valueOfReturnsDistance() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(Distance.valueOf(distance.name())).isEqualTo(distance));
  }

  @Test
  public void valueOfAbbreviationsReturnsDistance() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(Distance.valueOfAbbreviation(distance.getAbbreviation())).isEqualTo(distance));
  }

  @Test
  public void valueOfNullAbbreviationReturnsNull() {
    assertThat(Distance.valueOfAbbreviation(null)).isNull();
  }

  @Test
  public void valueOfMixedCaseAbbreviationsReturnsDistance() {

    assertThat(Distance.valueOfAbbreviation("NM")).isEqualTo(Distance.NANOMETER);
    assertThat(Distance.valueOfAbbreviation("Um")).isEqualTo(Distance.MICROMETER);
    assertThat(Distance.valueOfAbbreviation("mm")).isEqualTo(Distance.MILLIMETER);
    assertThat(Distance.valueOfAbbreviation("M")).isEqualTo(Distance.METER);
    assertThat(Distance.valueOfAbbreviation("kM")).isEqualTo(Distance.KILOMETER);
  }

  @Test
  public void valueOfInvalidAbbreviationsReturnsNull() {

    assertThat(Distance.valueOfAbbreviation("mile")).isNull();
    assertThat(Distance.valueOfAbbreviation("inches")).isNull();
    assertThat(Distance.valueOfAbbreviation("FEET")).isNull();
    assertThat(Distance.valueOfAbbreviation("1/4INCH")).isNull();
  }

  @Test
  public void valueOfNamesReturnsDistance() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(Distance.valueOfName(distance.name())).isEqualTo(distance));
  }

  @Test
  public void valueOfNullNameReturnsNull() {
    assertThat(Distance.valueOfName(null)).isNull();
  }

  @Test
  public void valueOfMixedCaseNamesReturnsDistance() {

    assertThat(Distance.valueOfName(Distance.NANOMETER.name())).isEqualTo(Distance.NANOMETER);
    assertThat(Distance.valueOfName("Micrometer")).isEqualTo(Distance.MICROMETER);
    assertThat(Distance.valueOfName("MilliMeter")).isEqualTo(Distance.MILLIMETER);
    assertThat(Distance.valueOfName("CentiMETER")).isEqualTo(Distance.CENTIMETER);
    assertThat(Distance.valueOfName("MeTeR")).isEqualTo(Distance.METER);
    assertThat(Distance.valueOfName("INch")).isEqualTo(Distance.INCH);
    assertThat(Distance.valueOfName("fOOt")).isEqualTo(Distance.FOOT);
    assertThat(Distance.valueOfName("YarD")).isEqualTo(Distance.YARD);
    assertThat(Distance.valueOfName("MILE")).isEqualTo(Distance.MILE);
  }

  @Test
  public void valueOfInvalidNamesReturnsNull() {

    assertThat(Distance.valueOfName("KillOMeter")).isNull();
    assertThat(Distance.valueOfName("MEETR")).isNull();
    assertThat(Distance.valueOfName("Mileage")).isNull();
  }

  @Test
  public void pluralizedNamesAreCorrect() {

    assertThat(Distance.NANOMETER.getPluralName()).isEqualTo(Distance.NANOMETER.name().concat("S"));
    assertThat(Distance.MICROMETER.getPluralName()).isEqualTo(Distance.MICROMETER.name().concat("S"));
    assertThat(Distance.MILLIMETER.getPluralName()).isEqualTo(Distance.MILLIMETER.name().concat("S"));
    assertThat(Distance.METER.getPluralName()).isEqualTo(Distance.METER.name().concat("S"));
    assertThat(Distance.KILOMETER.getPluralName()).isEqualTo(Distance.KILOMETER.name().concat("S"));
    assertThat(Distance.INCH.getPluralName()).isEqualTo("INCHES");
    assertThat(Distance.FOOT.getPluralName()).isEqualTo("FEET");
    assertThat(Distance.YARD.getPluralName()).isEqualTo(Distance.YARD.name().concat("S"));
    assertThat(Distance.MILE.getPluralName()).isEqualTo(Distance.MILE.name().concat("S"));
  }

  @Test
  public void toStringReturnsName() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(distance.toString()).isEqualTo(StringUtils.capitalize(distance.name().toLowerCase())));
  }
}
