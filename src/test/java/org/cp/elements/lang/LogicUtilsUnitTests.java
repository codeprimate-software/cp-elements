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

import org.junit.Test;

/**
 * Unit Tests for {@link LogicUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.LogicUtils
 */
@SuppressWarnings("unused")
public class LogicUtilsUnitTests {

  @Test
  @SuppressWarnings("all")
  public void falseXorFalseIsFalse() {
    assertThat(LogicUtils.xor(false, false)).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void falseXorTrueIsTrue() {
    assertThat(LogicUtils.xor(false, true)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void trueXorFalseIsTrue() {
    assertThat(LogicUtils.xor(true, false)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void trueXorTrueIsFalse() {
    assertThat(LogicUtils.xor(true, true)).isFalse();
  }
}
