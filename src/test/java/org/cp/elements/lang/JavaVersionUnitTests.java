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

import java.util.Arrays;
import java.util.Set;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link JavaVersion}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.JavaVersion
 * @since 1.0.0
 */
public class JavaVersionUnitTests {

  private void assertJavaVersionMajorMinorPatch(JavaVersion javaVersion, int expectedMajor) {
    assertJavaVersionMajorMinorPatch(javaVersion, expectedMajor, 0);
  }

  private void assertJavaVersionMajorMinorPatch(JavaVersion javaVersion, int expectedMajor, int expectedMinor) {

    assertThat(javaVersion).isNotNull();
    assertThat(javaVersion.getMajor()).isEqualTo(expectedMajor);
    assertThat(javaVersion.getMinor()).isEqualTo(expectedMinor);
    assertThat(javaVersion.getPatch()).isZero();
    assertThat(javaVersion.getBuildNumber()).isZero();
  }

  @Test
  public void parseFakeJavaVersion() {

    JavaVersion javaVersion = JavaVersion.parse("2.8.16_256");

    assertThat(javaVersion).isNotNull();
    assertThat(javaVersion.getMajor()).isEqualTo(2);
    assertThat(javaVersion.getMinor()).isEqualTo(8);
    assertThat(javaVersion.getPatch()).isEqualTo(16);
    assertThat(javaVersion.getBuildNumber()).isEqualTo(256);
  }

  @Test
  public void parseJavaVersion() {

    Set<JavaVersion> javaVersions = JavaVersion.values();

    assertThat(javaVersions).hasSize(22);

    javaVersions.forEach(javaVersion -> {

      JavaVersion parsedJavaVersion = JavaVersion.parse(javaVersion.toString());

      assertThat(parsedJavaVersion).isNotSameAs(javaVersion);
      assertThat(parsedJavaVersion).isEqualTo(javaVersion);
    });
  }

  @Test
  public void parseJavaVersionWithMajorVersionNumber() {
    assertThat(JavaVersion.parse("17")).isEqualTo(JavaVersion.SEVENTEEN);
  }

  @Test
  public void parseJavaVersionWithMajorMinorVersionNumbers() {
    assertThat(JavaVersion.parse("1.8")).isEqualTo(JavaVersion.EIGHT);
  }

  @Test
  public void parseJavaVersionWithMajorMinorPatchVersionNumbers() {
    assertThat(JavaVersion.parse("1.8.0")).isEqualTo(JavaVersion.EIGHT);
  }

  @Test
  public void parseJavaVersionWithMajorMinorPatchAndBuildVersionVersionNumbers() {

    assertThat(JavaVersion.parse("1.8.0_")).isEqualTo(JavaVersion.EIGHT);
    assertThat(JavaVersion.parse("1.8._321")).isEqualTo(JavaVersion.EIGHT);
    assertThat(JavaVersion.parse("1.8.0_321")).isEqualTo(JavaVersion.EIGHT);
  }

  @Test
  public void parseIllegalJavaVersion() {

    Arrays.asList("  ", "", null).forEach(javaVersionString -> assertThatIllegalArgumentException()
      .isThrownBy(() -> JavaVersion.parse(javaVersionString))
      .withMessage("Java version [%s] is required", javaVersionString)
      .withNoCause());
  }

  @Test
  public void majorMinorPatchVersionIsCorrect() {

    assertJavaVersionMajorMinorPatch(JavaVersion.EIGHT, 1, 8);
    assertJavaVersionMajorMinorPatch(JavaVersion.ELEVEN, 11);
    assertJavaVersionMajorMinorPatch(JavaVersion.FOURTEEN, 14);
    assertJavaVersionMajorMinorPatch(JavaVersion.SIXTEEN, 16);
    assertJavaVersionMajorMinorPatch(JavaVersion.SEVENTEEN, 17);
    assertJavaVersionMajorMinorPatch(JavaVersion.TWENTY_ONE, 21);
  }

  @Test
  public void isOlderThanIsTrue() {

    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.NINE)).isTrue();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.ELEVEN)).isTrue();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.FOURTEEN)).isTrue();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.SEVENTEEN)).isTrue();
  }

  @Test
  public void isOlderThanIsFalse() {

    assertThat(JavaVersion.SEVENTEEN.isOlderThan(JavaVersion.FOURTEEN)).isFalse();
    assertThat(JavaVersion.FOURTEEN.isOlderThan(JavaVersion.ELEVEN)).isFalse();
    assertThat(JavaVersion.ELEVEN.isOlderThan(JavaVersion.EIGHT)).isFalse();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.EIGHT)).isFalse();
    assertThat(JavaVersion.EIGHT.isOlderThan(JavaVersion.SEVEN)).isFalse();
  }

  @Test
  public void isNewerThanOrEqualToIsTrue() {

    assertThat(JavaVersion.SEVENTEEN.isNewerThanOrEqualTo(JavaVersion.FOURTEEN)).isTrue();
    assertThat(JavaVersion.FOURTEEN.isNewerThanOrEqualTo(JavaVersion.EIGHT)).isTrue();
    assertThat(JavaVersion.ELEVEN.isNewerThanOrEqualTo(JavaVersion.EIGHT)).isTrue();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.EIGHT)).isTrue();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.SEVEN)).isTrue();
  }

  @Test
  public void isNewerThanOrEqualToIsFalse() {

    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.NINE)).isFalse();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.ELEVEN)).isFalse();
    assertThat(JavaVersion.EIGHT.isNewerThanOrEqualTo(JavaVersion.FOURTEEN)).isFalse();
    assertThat(JavaVersion.ELEVEN.isNewerThanOrEqualTo(JavaVersion.SEVENTEEN)).isFalse();
  }

  @Test
  public void isJava8IsCorrect() {

    JavaVersion.values().forEach(javaVersion ->
      assertThat(javaVersion.isJava8()).isEqualTo(JavaVersion.EIGHT.equals(javaVersion)));
  }

  @Test
  public void isJava11IsCorrect() {

    JavaVersion.values().forEach(javaVersion ->
      assertThat(javaVersion.isJava11()).isEqualTo(JavaVersion.ELEVEN.equals(javaVersion)));
  }

  @Test
  public void isJava14IsCorrect() {

    JavaVersion.values().forEach(javaVersion ->
      assertThat(javaVersion.isJava14()).isEqualTo(JavaVersion.FOURTEEN.equals(javaVersion)));
  }

  @Test
  public void isJava17IsCorrect() {

    JavaVersion.values().forEach(javaVersion ->
      assertThat(javaVersion.isJava17()).isEqualTo(JavaVersion.SEVENTEEN.equals(javaVersion)));
  }

  @Test
  public void isJava21IsCorrect() {

    JavaVersion.values().forEach(javaVersion ->
      assertThat(javaVersion.isJava21()).isEqualTo(JavaVersion.TWENTY_ONE.equals(javaVersion)));
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
