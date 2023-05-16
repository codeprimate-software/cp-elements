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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.function.Supplier;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link LogicalOperator}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.LogicalOperator
 * @since 1.0.0
 */
public class LogicalOperatorTest {

  @SuppressWarnings("unchecked")
  private <T> Supplier<T> mockSupplier(String name) {
    return mock(Supplier.class, name);
  }

  @Test
  public void andOperator() {

    assertThat(LogicalOperator.AND).isNotNull();
    assertThat(LogicalOperator.AND.getDescription()).isEqualTo("and");
    assertThat(LogicalOperator.AND.getOpposite()).isEqualTo(LogicalOperator.OR);
    assertThat(LogicalOperator.AND.getSymbol()).isEqualTo("&&");
    assertThat(LogicalOperator.AND.isBinary()).isTrue();
    assertThat(LogicalOperator.AND.isTernary()).isFalse();
    assertThat(LogicalOperator.AND.isUnary()).isFalse();
  }

  @Test
  public void andOperatorEvaluation() {

    assertThat(LogicalOperator.AND.evaluate(true)).isTrue();
    assertThat(LogicalOperator.AND.evaluate(true, true)).isTrue();
    assertThat(LogicalOperator.AND.evaluate(true, true, true)).isTrue();
    assertThat(LogicalOperator.AND.evaluate(true, false, true)).isFalse();
    assertThat(LogicalOperator.AND.evaluate(false, true)).isFalse();
    assertThat(LogicalOperator.AND.evaluate(true, false)).isFalse();
    assertThat(LogicalOperator.AND.evaluate(false)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void andOperatorSupplierEvaluation() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");

    when(mockSupplierOne.get()).thenReturn(true);
    when(mockSupplierTwo.get()).thenReturn(true);

    assertThat(LogicalOperator.AND.evaluate(mockSupplierOne, mockSupplierTwo)).isTrue();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, times(1)).get();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void andOperatorSupplierEvaluationShortCircuits() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");

    when(mockSupplierOne.get()).thenReturn(false);
    when(mockSupplierTwo.get()).thenReturn(true);

    assertThat(LogicalOperator.AND.evaluate(mockSupplierOne, mockSupplierTwo)).isFalse();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, never()).get();
  }

  @Test
  public void negateAndOperatorEvaluation() {

    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, false, false)).isTrue();
  }

  @Test
  public void notOperator() {

    LogicalOperator NOT = new LogicalOperator.LogicalNot(LogicalOperator.AND);

    assertThat(NOT).isInstanceOf(LogicalOperator.LogicalNot.class);
    assertThat(NOT.getDescription()).isEqualTo("not");
    assertThat(NOT.getOpposite()).isSameAs(NOT);
    assertThat(NOT.getSymbol()).isEqualTo("!");
    assertThat(NOT.isBinary()).isFalse();
    assertThat(NOT.isTernary()).isFalse();
    assertThat(NOT.isUnary()).isTrue();
  }

  @Test
  public void orOperator() {

    assertThat(LogicalOperator.OR).isNotNull();
    assertThat(LogicalOperator.OR.getDescription()).isEqualTo("or");
    assertThat(LogicalOperator.OR.getOpposite()).isEqualTo(LogicalOperator.AND);
    assertThat(LogicalOperator.OR.getSymbol()).isEqualTo("||");
    assertThat(LogicalOperator.OR.isBinary()).isTrue();
    assertThat(LogicalOperator.OR.isTernary()).isFalse();
    assertThat(LogicalOperator.OR.isUnary()).isFalse();
  }

  @Test
  public void orOperatorEvaluation() {

    assertThat(LogicalOperator.OR.evaluate(true)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(true, true)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(true, true, true)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(false, false, true)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(false, true, false)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(true, false)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(false, true)).isTrue();
    assertThat(LogicalOperator.OR.evaluate(false)).isFalse();
    assertThat(LogicalOperator.OR.evaluate(false, false)).isFalse();
    assertThat(LogicalOperator.OR.evaluate(false, false, false)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void orOperatorSupplierEvaluation() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");
    Supplier<Boolean> mockSupplierThree = mockSupplier("MockSupplierThree");

    when(mockSupplierOne.get()).thenReturn(false);
    when(mockSupplierTwo.get()).thenReturn(false);
    when(mockSupplierThree.get()).thenReturn(false);

    assertThat(LogicalOperator.OR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree)).isFalse();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, times(1)).get();
    verify(mockSupplierThree, times(1)).get();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void orOperatorSupplierEvaluationShortcircuits() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");
    Supplier<Boolean> mockSupplierThree = mockSupplier("MockSupplierThree");

    when(mockSupplierOne.get()).thenReturn(true);
    when(mockSupplierTwo.get()).thenReturn(false);
    when(mockSupplierThree.get()).thenReturn(false);

    assertThat(LogicalOperator.OR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree)).isTrue();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, never()).get();
    verify(mockSupplierThree, never()).get();
  }

  @Test
  public void negateOrOperatorEvaluation() {

    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true, true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, true)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, false, true)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, true)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, false)).isTrue();
  }

  @Test
  public void xorOperator() {

    assertThat(LogicalOperator.XOR).isNotNull();
    assertThat(LogicalOperator.XOR.getDescription()).isEqualTo("xor");
    assertThat(LogicalOperator.XOR.getOpposite()).isSameAs(LogicalOperator.XOR);
    assertThat(LogicalOperator.XOR.getSymbol()).isEqualTo("^");
    assertThat(LogicalOperator.XOR.isBinary()).isTrue();
    assertThat(LogicalOperator.XOR.isTernary()).isFalse();
    assertThat(LogicalOperator.XOR.isUnary()).isFalse();
  }

  @Test
  public void xorOperatorEvaluation() {

    assertThat(LogicalOperator.XOR.evaluate(true)).isTrue();
    assertThat(LogicalOperator.XOR.evaluate(true, false)).isTrue();
    assertThat(LogicalOperator.XOR.evaluate(true, false, false)).isTrue();
    assertThat(LogicalOperator.XOR.evaluate(false)).isFalse();
    assertThat(LogicalOperator.XOR.evaluate(false, false)).isFalse();
    assertThat(LogicalOperator.XOR.evaluate(false, false, false)).isFalse();
    assertThat(LogicalOperator.XOR.evaluate(true, false, true)).isFalse();
    assertThat(LogicalOperator.XOR.evaluate(false, true, true)).isFalse();
    assertThat(LogicalOperator.XOR.evaluate(true, true, true)).isFalse();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void xorOperatorSupplierEvaluation() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");
    Supplier<Boolean> mockSupplierThree = mockSupplier("MockSupplierThree");
    Supplier<Boolean> mockSupplierFour = mockSupplier("MockSupplierFour");

    when(mockSupplierOne.get()).thenReturn(false);
    when(mockSupplierTwo.get()).thenReturn(true);
    when(mockSupplierThree.get()).thenReturn(false);
    when(mockSupplierFour.get()).thenReturn(false);

    assertThat(LogicalOperator.XOR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree, mockSupplierFour))
      .isTrue();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, times(1)).get();
    verify(mockSupplierThree, times(1)).get();
    verify(mockSupplierFour, times(1)).get();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void xorOperatorSupplierEvaluationShortCircuits() {

    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");
    Supplier<Boolean> mockSupplierThree = mockSupplier("MockSupplierThree");
    Supplier<Boolean> mockSupplierFour = mockSupplier("MockSupplierFour");

    when(mockSupplierOne.get()).thenReturn(true);
    when(mockSupplierTwo.get()).thenReturn(true);
    when(mockSupplierThree.get()).thenReturn(false);
    when(mockSupplierFour.get()).thenReturn(false);

    assertThat(LogicalOperator.XOR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree, mockSupplierFour))
      .isFalse();

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, times(1)).get();
    verify(mockSupplierThree, never()).get();
    verify(mockSupplierFour, never()).get();
  }

  @Test
  public void negateXorOperatorEvaluation() {

    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false, false)).isFalse();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, false, false)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false, true)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, true, true)).isTrue();
    assertThat(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, true, true)).isTrue();
  }
}
