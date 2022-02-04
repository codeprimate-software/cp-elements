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
package org.cp.elements.function;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Instant;

import org.junit.Test;

/**
 * Unit Tests for {@link CannedPredicates}.
 *
 * @author John Blum
 * @see java.util.function.Predicate
 * @see org.junit.Test
 * @see org.cp.elements.function.CannedPredicates
 * @since 1.0.0
 */
public class CannedPredicatesUnitTests {

  @Test
  public void defaultWithAnyObjectIsTrue() {

    assertThat(CannedPredicates.DEFAULT.test(new Object())).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(false)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test('X')).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(2)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(Math.PI)).isTrue();
    assertThat(CannedPredicates.DEFAULT.test("TEST")).isTrue();
    assertThat(CannedPredicates.DEFAULT.test(Instant.now())).isTrue();
  }

  @Test
  public void defaultWithNullIsTrue() {
    assertThat(CannedPredicates.DEFAULT.test(null)).isTrue();
  }

  @Test
  public void isFalseWithFalse() {

    assertThat(CannedPredicates.IS_FALSE.test(false)).isTrue();
    assertThat(CannedPredicates.IS_FALSE.test(Boolean.FALSE)).isTrue();
  }

  @Test
  public void isFalseWithNonBooleanValueIsFalse() {

    assertThat(CannedPredicates.IS_FALSE.test(new Object())).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test('X')).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(2)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(Math.PI)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test("TEST")).isFalse();
  }

  @Test
  public void isFalseWithNullIsFalse() {
    assertThat(CannedPredicates.IS_FALSE.test(null)).isFalse();
  }

  @Test
  public void isFalseWithTrue() {

    assertThat(CannedPredicates.IS_FALSE.test(true)).isFalse();
    assertThat(CannedPredicates.IS_FALSE.test(Boolean.TRUE)).isFalse();
  }

  @Test
  public void isTrueWithTrue() {

    assertThat(CannedPredicates.IS_TRUE.test(true)).isTrue();
    assertThat(CannedPredicates.IS_TRUE.test(Boolean.TRUE)).isTrue();
  }

  @Test
  public void isTrueWithNonBooleanValuesIsFalse() {

    assertThat(CannedPredicates.IS_TRUE.test(new Object())).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test('X')).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(2)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(Math.PI)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test("TEST")).isFalse();
  }

  @Test
  public void isTrueWithNullBooleanValueIsFalse() {
    assertThat(CannedPredicates.IS_TRUE.test(null)).isFalse();
  }

  @Test
  public void isTrueWithFalse() {

    assertThat(CannedPredicates.IS_TRUE.test(false)).isFalse();
    assertThat(CannedPredicates.IS_TRUE.test(Boolean.FALSE)).isFalse();
  }

  @Test
  public void notNullWithNonNullObject() {

    assertThat(CannedPredicates.NOT_NULL.test(new Object())).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test('X')).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test(2)).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test(Math.PI)).isTrue();
    assertThat(CannedPredicates.NOT_NULL.test("TEST")).isTrue();
  }

  @Test
  public void notNullWithNullObject() {
    assertThat(CannedPredicates.NOT_NULL.test(null)).isFalse();
  }
}
