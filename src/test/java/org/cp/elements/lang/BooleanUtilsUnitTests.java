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

import java.util.concurrent.atomic.AtomicBoolean;

import org.junit.Test;

/**
 * Unit Tests for {@link BooleanUtils}.
 *
 * @author John J. Blum
 * @see java.lang.Boolean
 * @see java.util.concurrent.atomic.AtomicBoolean
 * @see org.junit.Test
 * @see org.cp.elements.lang.BooleanUtils
 * @since 1.0.0
 */
public class BooleanUtilsUnitTests {

  @Test
  @SuppressWarnings("all")
  public void andIsTrue() {

    assertThat(BooleanUtils.and(true)).isTrue();
    assertThat(BooleanUtils.and(Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.and(true, true)).isTrue();
    assertThat(BooleanUtils.and(Boolean.TRUE, Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.and(true, true, true)).isTrue();
    assertThat(BooleanUtils.and(Boolean.TRUE, Boolean.TRUE, Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.and(!Boolean.FALSE)).isTrue();
    assertThat(BooleanUtils.and(true, !false)).isTrue();
    assertThat(BooleanUtils.and(!Boolean.FALSE, !false, !Boolean.FALSE)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void andIsFalse() {

    assertThat(BooleanUtils.and(true, true, true, false)).isFalse();
    assertThat(BooleanUtils.and(true, true, true, Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.and(false, true, true, true)).isFalse();
    assertThat(BooleanUtils.and(Boolean.FALSE, true, true, true)).isFalse();
    assertThat(BooleanUtils.and(true, false, true)).isFalse();
    assertThat(BooleanUtils.and(true, Boolean.FALSE, true)).isFalse();
    assertThat(BooleanUtils.and(false)).isFalse();
    assertThat(BooleanUtils.and(Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.and(!Boolean.TRUE)).isFalse();
    assertThat(BooleanUtils.and(!Boolean.TRUE, true)).isFalse();
    assertThat(BooleanUtils.and(Boolean.TRUE, !true, true)).isFalse();
  }

  @Test
  public void andNullIsFalse() {

    assertThat(BooleanUtils.and((Boolean[]) null)).isFalse();
    assertThat(BooleanUtils.and(null, null)).isFalse();
    assertThat(BooleanUtils.and(true, null)).isFalse();
    assertThat(BooleanUtils.and(null, true)).isFalse();
    assertThat(BooleanUtils.and(true, true, true, null)).isFalse();
    assertThat(BooleanUtils.and(null, true, true, true)).isFalse();
  }

  @Test
  public void negateFalseIsTrue() {

    assertThat(BooleanUtils.negate(false)).isTrue();
    assertThat(BooleanUtils.negate(Boolean.FALSE)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void negateNullIsFalse() {
    assertThat(BooleanUtils.negate(null)).isFalse();
  }

  @Test
  public void negateTrueIsFalse() {

    assertThat(BooleanUtils.negate(true)).isFalse();
    assertThat(BooleanUtils.negate(Boolean.TRUE)).isFalse();
  }

  @Test
  public void notFalseIsTrue() {

    assertThat(BooleanUtils.not(false)).isTrue();
    assertThat(BooleanUtils.not(Boolean.FALSE)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void notNullIsFalse() {
    assertThat(BooleanUtils.not(null)).isFalse();
  }

  @Test
  public void notTrueIsFalse() {

    assertThat(BooleanUtils.not(true)).isFalse();
    assertThat(BooleanUtils.not(Boolean.TRUE)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void orIsFalse() {

    assertThat(BooleanUtils.or(false)).isFalse();
    assertThat(BooleanUtils.or(Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.or(false, false)).isFalse();
    assertThat(BooleanUtils.or(Boolean.FALSE, Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.or(false, false, false)).isFalse();
    assertThat(BooleanUtils.or(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.or(!Boolean.TRUE, false)).isFalse();
    assertThat(BooleanUtils.or(!Boolean.TRUE, Boolean.FALSE)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void orIsTrue() {

    assertThat(BooleanUtils.or(true)).isTrue();
    assertThat(BooleanUtils.or(Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.or(false, true)).isTrue();
    assertThat(BooleanUtils.or(true, false)).isTrue();
    assertThat(BooleanUtils.or(true, false, false, false)).isTrue();
    assertThat(BooleanUtils.or(false, false, false, true)).isTrue();
    assertThat(BooleanUtils.or(!Boolean.FALSE)).isTrue();
    assertThat(BooleanUtils.or(!Boolean.FALSE, false)).isTrue();
  }

  @Test
  public void orNullIsFalse() {

    assertThat(BooleanUtils.or((Boolean[]) null)).isFalse();
    assertThat(BooleanUtils.or(null, null)).isFalse();
    assertThat(BooleanUtils.or(false, null)).isFalse();
  }

  @Test
  public void toBooleanIsTrue() {
    assertThat(BooleanUtils.toBoolean(true)).isSameAs(Boolean.TRUE);
  }

  @Test
  public void toBooleanIsFalse() {
    assertThat(BooleanUtils.toBoolean(false)).isSameAs(Boolean.FALSE);
  }

  @Test
  public void toStringIsSuccessful() {

    assertThat(BooleanUtils.toString(true, "true", "false")).isEqualTo("true");
    assertThat(BooleanUtils.toString(false, "true", "false")).isEqualTo("false");
    assertThat(BooleanUtils.toString(true, "Yes", "No")).isEqualTo("Yes");
    assertThat(BooleanUtils.toString(false, "Yes", "No")).isEqualTo("No");
    assertThat(BooleanUtils.toString(true, "1", "0")).isEqualTo("1");
    assertThat(BooleanUtils.toString(false, "1", "0")).isEqualTo("0");
    assertThat(BooleanUtils.toString(true, "false", "true")).isEqualTo("false");
    assertThat(BooleanUtils.toString(false, "false", "true")).isEqualTo("true");
    assertThat(BooleanUtils.toString(true, "null", null)).isEqualTo("null");
    assertThat(BooleanUtils.toString(false, "null", null)).isNull();
    assertThat(BooleanUtils.toString(Boolean.TRUE, "not null", "null")).isEqualTo("not null");
    assertThat(BooleanUtils.toString(Boolean.FALSE, "not null", "null")).isEqualTo("null");
    assertThat(BooleanUtils.toString(null, "not null", "null")).isEqualTo("null");
  }

  @Test
  public void valueOfAtomicBooleanIsTrue() {
    assertThat(BooleanUtils.valueOf(new AtomicBoolean(true))).isTrue();
  }

  @Test
  public void valueOfAtomicBooleanIsFalse() {
    assertThat(BooleanUtils.valueOf(new AtomicBoolean(false))).isFalse();
  }

  @Test
  public void valueOfNullAtomicBooleanIsFalse() {
    assertThat(BooleanUtils.valueOf((AtomicBoolean) null)).isFalse();
  }

  @Test
  public void valueOfBooleanIsTrue() {

    assertThat(BooleanUtils.valueOf(true)).isTrue();
    assertThat(BooleanUtils.valueOf(Boolean.TRUE)).isTrue();
  }

  @Test
  public void valueOfBooleanIsFalse() {

    assertThat(BooleanUtils.valueOf(false)).isFalse();
    assertThat(BooleanUtils.valueOf(Boolean.FALSE)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void valueOfNullBooleanIsFalse() {
    assertThat(BooleanUtils.valueOf((Boolean) null)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void xorIsTrue() {

    assertThat(BooleanUtils.xor(true)).isTrue();
    assertThat(BooleanUtils.xor(Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.xor(true, null)).isTrue();
    assertThat(BooleanUtils.xor(true, false)).isTrue();
    assertThat(BooleanUtils.xor(true, Boolean.FALSE)).isTrue();
    assertThat(BooleanUtils.xor(true, !true)).isTrue();
    assertThat(BooleanUtils.xor(true, !Boolean.TRUE)).isTrue();
    assertThat(BooleanUtils.xor(true, false, false, false)).isTrue();
    assertThat(BooleanUtils.xor(false, true)).isTrue();
    assertThat(BooleanUtils.xor(false, false, false, true)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void xorIsFalse() {

    assertThat(BooleanUtils.xor(false)).isFalse();
    assertThat(BooleanUtils.xor(Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.xor(false, false)).isFalse();
    assertThat(BooleanUtils.xor(Boolean.FALSE, Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.xor(false, false, false)).isFalse();
    assertThat(BooleanUtils.xor(Boolean.FALSE, Boolean.FALSE, Boolean.FALSE)).isFalse();
    assertThat(BooleanUtils.xor(false, !Boolean.TRUE, false)).isFalse();
    assertThat(BooleanUtils.xor(true, false, true)).isFalse();
    assertThat(BooleanUtils.xor(true, true)).isFalse();
    assertThat(BooleanUtils.xor(true, Boolean.TRUE)).isFalse();
    assertThat(BooleanUtils.xor(Boolean.TRUE, Boolean.TRUE)).isFalse();
  }

  @Test
  public void xorNullIsFalse() {

    assertThat(BooleanUtils.xor((Boolean[]) null)).isFalse();
    assertThat(BooleanUtils.xor(null, null)).isFalse();
    assertThat(BooleanUtils.xor(false, null)).isFalse();
  }
}
