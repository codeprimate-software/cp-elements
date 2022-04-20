/*
 * Copyright 2016 Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit Tests for {@link JavaVersion}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.JavaVersion
 * @since 1.0.0
 */
public class JavaVersionUnitTests {

  @Test
  public void parseFakeJavaVersion() {

    JavaVersion javaVersion = JavaVersion.parse("2.8.16_248");

    assertThat(javaVersion).isNotNull();
    assertThat(javaVersion.getMajor()).isEqualTo(2);
    assertThat(javaVersion.getMinor()).isEqualTo(8);
    assertThat(javaVersion.getPatch()).isEqualTo(16);
    assertThat(javaVersion.getBuildNumber()).isEqualTo(248);
  }

  @Test
  public void parseJavaVersionWithMajorVersionNumber() {
    assertThat(JavaVersion.parse("17")).isEqualTo(JavaVersion.SEVENTEEN);
  }

  @Test
  public void parseJavaVersionWithMajorMinorVersionNumber() {
    assertThat(JavaVersion.parse("1.8")).isEqualTo(JavaVersion.EIGHT);
  }

  @Test
  public void parseJavaVersionWithMajorMinorPatchVersionNumber() {
    assertThat(JavaVersion.parse("1.8.0")).isEqualTo(JavaVersion.EIGHT);
  }

  @Test
  public void parseJavaVersionWithMajorMinorPatchAndBuildVersionVersionNumbers() {

    assertThat(JavaVersion.parse("1.8.0_")).isEqualTo(JavaVersion.EIGHT);
    assertThat(JavaVersion.parse("1.8._321")).isEqualTo(JavaVersion.EIGHT);
    assertThat(JavaVersion.parse("1.8.0_321")).isEqualTo(JavaVersion.EIGHT);
  }

  private void parseIllegalJavaVersionString(String javaVersion) {

    try {
      JavaVersion.parse(javaVersion);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Java version [%s] must not be null or empty", javaVersion);
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseBlankJavaVersion() {
    parseIllegalJavaVersionString("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseEmptyJavaVersion() {
    parseIllegalJavaVersionString("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void parseNullJavaVersion() {
    parseIllegalJavaVersionString(null);
  }

  @Test
  public void majorMinorPatchVersionIsCorrect() {

    assertThat(JavaVersion.EIGHT.getMajor()).isEqualTo(1);
    assertThat(JavaVersion.EIGHT.getMinor()).isEqualTo(8);
    assertThat(JavaVersion.EIGHT.getPatch()).isEqualTo(0);
    assertThat(JavaVersion.ELEVEN.getMajor()).isEqualTo(11);
    assertThat(JavaVersion.ELEVEN.getMinor()).isEqualTo(0);
    assertThat(JavaVersion.ELEVEN.getPatch()).isEqualTo(0);
    assertThat(JavaVersion.SIXTEEN.getMajor()).isEqualTo(16);
    assertThat(JavaVersion.SIXTEEN.getMinor()).isEqualTo(0);
    assertThat(JavaVersion.SIXTEEN.getPatch()).isEqualTo(0);
    assertThat(JavaVersion.SEVENTEEN.getMajor()).isEqualTo(17);
    assertThat(JavaVersion.SEVENTEEN.getMinor()).isEqualTo(0);
    assertThat(JavaVersion.SEVENTEEN.getPatch()).isEqualTo(0);
  }

  @Test
  public void iOlderThanIsTrue() {

    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.NINE)).isTrue();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.ELEVEN)).isTrue();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.SEVENTEEN)).isTrue();
  }

  @Test
  public void isOlderThanIsFalse() {

    assertThat(JavaVersion.SEVENTEEN.isOlderThan(JavaVersion.ELEVEN)).isFalse();
    assertThat(JavaVersion.ELEVEN.isOlderThan(JavaVersion.EIGHT)).isFalse();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.EIGHT)).isFalse();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.SEVEN)).isFalse();
  }

  @Test
  public void isNewerThanOrEqualToIsTrue() {

    assertThat(JavaVersion.SEVENTEEN.isNewerThanOrEqualTo(JavaVersion.ELEVEN)).isTrue();
    assertThat(JavaVersion.ELEVEN.isNewerThanOrEqualTo(JavaVersion.EIGHT)).isTrue();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.EIGHT)).isTrue();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.SEVEN)).isTrue();
  }

  @Test
  public void isNewerThanOrEqualToIsFalse() {

    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.NINE)).isFalse();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.ELEVEN)).isFalse();
    assertThat(JavaVersion.ELEVEN.isNewerThanOrEqualTo(JavaVersion.SEVENTEEN)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void compareToIsEqualTo() {
    assertThat(JavaVersion.EIGHT.compareTo(JavaVersion.EIGHT)).isZero();
  }

  @Test
  public void compareToIsGreaterThan() {
    assertThat(JavaVersion.SEVENTEEN.compareTo(JavaVersion.EIGHT)).isGreaterThan(0);
  }

  @Test
  public void compareToIsLessThan() {

    assertThat(JavaVersion.EIGHT.compareTo(JavaVersion.SEVENTEEN)).isLessThan(0);
    assertThat(JavaVersion.EIGHT.compareTo(JavaVersion.parse("1.8.0_321"))).isLessThan(0);
  }
}
