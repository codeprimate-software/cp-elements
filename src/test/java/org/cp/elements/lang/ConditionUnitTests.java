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
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.Test;

/**
 * Unit Tests for {@link Condition}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Condition
 * @since 1.0.0
 */
public class ConditionUnitTests {

  @Test
  public void nullSafetyWithNullConditionReturningTrue() {

    Condition condition = Condition.nullSafeCondition(null, true);

    assertThat(condition).isNotNull();
    assertThat(condition.evaluate()).isTrue();
  }

  @Test
  public void nullSafetyWithNullConditionReturningFalse() {

    Condition condition = Condition.nullSafeCondition(null, false);

    assertThat(condition).isNotNull();
    assertThat(condition.evaluate()).isFalse();
  }

  @Test
  public void nullSafetyWithNonNullCondition() {

    Condition mockCondition = mock(Condition.class);

    assertThat(Condition.nullSafeCondition(mockCondition, true)).isSameAs(mockCondition);
  }

  @Test
  public void trueConditionReturnsTrue() {
    assertThat(Condition.TRUE_CONDITION.evaluate()).isTrue();
  }

  @Test
  public void falseConditionReturnsFalse() {
    assertThat(Condition.FALSE_CONDITION.evaluate()).isFalse();
  }

  @Test
  public void trueAndThenTrueConditionReturnsTrue() {
    assertThat(Condition.TRUE_CONDITION.andThen(Condition.TRUE_CONDITION).evaluate()).isTrue();
  }

  @Test
  public void falseAndThenTrueConditionReturnsFalse() {

    assertThat(Condition.FALSE_CONDITION.andThen(Condition.TRUE_CONDITION).evaluate()).isFalse();
    assertThat(Condition.TRUE_CONDITION.andThen(Condition.FALSE_CONDITION).evaluate()).isFalse();
  }

  @Test
  public void falseAndThenFalseConditionReturnsFalse() {
    assertThat(Condition.FALSE_CONDITION.andThen(Condition.FALSE_CONDITION).evaluate()).isFalse();
  }

  @Test
  public void trueOrElseTrueConditionReturnsTrue() {
    assertThat(Condition.TRUE_CONDITION.orElse(Condition.TRUE_CONDITION).evaluate()).isTrue();
  }

  @Test
  public void falseOrElseTrueConditionReturnsTrue() {

    assertThat(Condition.FALSE_CONDITION.orElse(Condition.TRUE_CONDITION).evaluate()).isTrue();
    assertThat(Condition.TRUE_CONDITION.orElse(Condition.FALSE_CONDITION).evaluate()).isTrue();
  }

  @Test
  public void falseOrElseFalseConditionReturnsFalse() {
    assertThat(Condition.FALSE_CONDITION.orElse(Condition.FALSE_CONDITION).evaluate()).isFalse();
  }

  @Test
  public void trueXorTrueConditionReturnsFalse() {
    assertThat(Condition.TRUE_CONDITION.xor(Condition.TRUE_CONDITION).evaluate()).isFalse();
  }

  @Test
  public void trueXorFalseConditionReturnsTrue() {

    assertThat(Condition.TRUE_CONDITION.xor(Condition.FALSE_CONDITION).evaluate()).isTrue();
    assertThat(Condition.FALSE_CONDITION.xor(Condition.TRUE_CONDITION).evaluate()).isTrue();
  }

  @Test
  public void falseXorFalseConditionReturnsFalse() {
    assertThat(Condition.FALSE_CONDITION.xor(Condition.FALSE_CONDITION).evaluate()).isFalse();
  }

  @Test
  public void withTrueAndThenMustEvaluateCondition() {

    Condition mockCondition = mock(Condition.class);

    doReturn(false).when(mockCondition).evaluate();

    assertThat(Condition.TRUE_CONDITION.andThen(mockCondition).evaluate()).isFalse();

    verify(mockCondition, times(1)).evaluate();
    verifyNoMoreInteractions(mockCondition);
  }

  @Test
  public void withFalseAndThenDoesNotEvaluateCondition() {

    Condition mockCondition = mock(Condition.class);

    doReturn(true).when(mockCondition).evaluate();

    assertThat(Condition.FALSE_CONDITION.andThen(mockCondition).evaluate()).isFalse();

    verifyNoInteractions(mockCondition);
  }

  @Test
  public void withTrueOrElseDoesNotEvaluateCondition() {

    Condition mockCondition = mock(Condition.class);

    doReturn(false).when(mockCondition).evaluate();

    assertThat(Condition.TRUE_CONDITION.orElse(mockCondition).evaluate()).isTrue();

    verifyNoInteractions(mockCondition);
  }

  @Test
  public void withFalseOrElseMustEvaluateCondition() {

    Condition mockCondition = mock(Condition.class);

    doReturn(true).when(mockCondition).evaluate();

    assertThat(Condition.FALSE_CONDITION.orElse(mockCondition).evaluate()).isTrue();

    verify(mockCondition, times(1)).evaluate();
    verifyNoMoreInteractions(mockCondition);
  }

  @Test
  public void xorMustEvaluateThisConditionWithTheGivenCondition() {

    Condition mockCondition = mock(Condition.class);

    when(mockCondition.evaluate()).thenReturn(false);

    assertThat(Condition.TRUE_CONDITION.xor(mockCondition).evaluate()).isTrue();
    assertThat(Condition.FALSE_CONDITION.xor(mockCondition).evaluate()).isFalse();

    verify(mockCondition, times(2)).evaluate();
    verifyNoMoreInteractions(mockCondition);
  }
}
