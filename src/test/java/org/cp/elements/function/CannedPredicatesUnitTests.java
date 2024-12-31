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
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Instant;
import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link CannedPredicates}.
 *
 * @author John Blum
 * @see java.util.function.Predicate
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.function.CannedPredicates
 * @since 1.0.0
 */
class CannedPredicatesUnitTests {

  @Test
  void acceptAllWithAnyObjectReturnsTrue() {

    Arrays.asList(new Object(), false, 'x', 0, Math.PI, "TEST", Instant.now()).forEach(value ->
      assertThat(CannedPredicates.ACCEPT_ALL.test(value)).isTrue());
  }

  @Test
  void acceptAllWithNullReturnsTrue() {
    assertThat(CannedPredicates.ACCEPT_ALL.test(null)).isTrue();
  }

  @Test
  void defaultWithAnyObjectIsTrue() {

    assertThat(CannedPredicates.DEFAULT.test(new Object())).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(false)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test('X')).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(2)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(Math.PI)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test("TEST")).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(Instant.now())).isTrue();
  }

  @Test
  void defaultWithNullIsTrue() {
    assertThat(CannedPredicates.DEFAULT.test(null)).isTrue();
  }

  @Test
  void isFalseWithFalse() {

    assertThat(CannedPredicates.IS_FALSE.test(false)).isTrue();
    assertThat(CannedPredicates.IS_FALSE.test(Boolean.FALSE)).isTrue();
  }

  @Test
  void isFalseWithNonBooleanValueIsFalse() {

    assertThat(CannedPredicates.IS_FALSE.test(new Object())).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test('X')).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(2)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(Math.PI)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test("TEST")).isFalse();
  }

  @Test
  void isFalseWithNullIsFalse() {
    assertThat(CannedPredicates.IS_FALSE.test(null)).isFalse();
  }

  @Test
  void isFalseWithTrue() {

    assertThat(CannedPredicates.IS_FALSE.test(true)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(Boolean.TRUE)).isFalse();
  }

  @Test
  void isTrueWithTrue() {

    assertThat(CannedPredicates.IS_TRUE.test(true)).isTrue();
    assertThat(CannedPredicates.IS_TRUE.test(Boolean.TRUE)).isTrue();
  }

  @Test
  void isTrueWithNonBooleanValuesIsFalse() {

    assertThat(CannedPredicates.IS_TRUE.test(new Object())).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test('X')).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(2)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(Math.PI)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test("TEST")).isFalse();
  }

  @Test
  void isTrueWithNullBooleanValueIsFalse() {
    assertThat(CannedPredicates.IS_TRUE.test(null)).isFalse();
  }

  @Test
  public void isTrueWithFalse() {

    assertThat(CannedPredicates.IS_TRUE.test(false)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(Boolean.FALSE)).isFalse();
  }

  @Test
  void notNullWithNonNullObject() {

    assertThat(CannedPredicates.NOT_NULL.test(new Object())).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test('X')).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test(2)).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test(Math.PI)).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test("TEST")).isTrue();
  }

  @Test
  void notNullWithNullObject() {
    assertThat(CannedPredicates.NOT_NULL.test(null)).isFalse();
  }

  @Test
  void rejectAllWithAnyObjectReturnsTrue() {

    Arrays.asList(new Object(), false, 'x', 0, Math.PI, "TEST", Instant.now()).forEach(value ->
      assertThat(CannedPredicates.REJECT_ALL.test(value)).isFalse());
  }

  @Test
  void rejectAllWithNullReturnsTrue() {
    assertThat(CannedPredicates.REJECT_ALL.test(null)).isFalse();
  }
}
