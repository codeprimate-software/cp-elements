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
  public void valueOfAbbreviationsIsCorrect() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(Distance.valueOfAbbreviation(distance.getAbbreviation())).isEqualTo(distance));
  }

  @Test
  public void valueOfMixedCaseAbbreviationsIsCorrect() {

    assertThat(Distance.valueOfAbbreviation("NM")).isEqualTo(Distance.NANOMETER);
    assertThat(Distance.valueOfAbbreviation("Um")).isEqualTo(Distance.MICROMETER);
    assertThat(Distance.valueOfAbbreviation("mm")).isEqualTo(Distance.MILLIMETER);
    assertThat(Distance.valueOfAbbreviation("M")).isEqualTo(Distance.METER);
    assertThat(Distance.valueOfAbbreviation("kM")).isEqualTo(Distance.KILOMETER);
  }

  @Test
  public void valueOfMixedCaseNamesIsCorrect() {

    assertThat(Distance.valueOfName(Distance.NANOMETER.name())).isEqualTo(Distance.NANOMETER);
    assertThat(Distance.valueOfName("Micrometer")).isEqualTo(Distance.MICROMETER);
    assertThat(Distance.valueOfName("MilliMeter")).isEqualTo(Distance.MILLIMETER);
    assertThat(Distance.valueOfName("CentiMETER")).isEqualTo(Distance.CENTIMETER);
    assertThat(Distance.valueOfName("MeTeR")).isEqualTo(Distance.METER);
    assertThat(Distance.valueOfName("INch")).isEqualTo(Distance.INCH);
    assertThat(Distance.valueOfName("fOOt")).isEqualTo(Distance.FOOT);
    assertThat(Distance.valueOfName("YarD")).isEqualTo(Distance.YARD);
  }

  @Test
  public void valueOfNamesIsCorrect() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(Distance.valueOfName(distance.name())).isEqualTo(distance));
  }

  @Test
  public void toStringReturnsName() {

    Arrays.stream(Distance.values()).forEach(distance ->
      assertThat(distance.toString()).isEqualTo(StringUtils.capitalize(distance.name().toLowerCase())));
  }
}
