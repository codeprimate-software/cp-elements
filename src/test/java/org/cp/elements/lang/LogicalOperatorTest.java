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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.instanceOf;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.function.Supplier;

import org.junit.Test;

/**
 * The LogicalOperatorTest class is a test suite of test cases testing the contract and functionality of the
 * {@link LogicalOperator} class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.LogicalOperator
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LogicalOperatorTest {

  @SuppressWarnings("unchecked")
  protected <T> Supplier<T> mockSupplier(String name) {
    return mock(Supplier.class, name);
  }

  @Test
  public void andOperator() {
    assertThat(LogicalOperator.AND, is(notNullValue()));
    assertThat(LogicalOperator.AND.getDescription(), is(equalTo("and")));
    assertThat(LogicalOperator.AND.getOpposite(), is(equalTo(LogicalOperator.OR)));
    assertThat(LogicalOperator.AND.getSymbol(), is(equalTo("&&")));
    assertThat(LogicalOperator.AND.isBinary(), is(true));
    assertThat(LogicalOperator.AND.isTernary(), is(false));
    assertThat(LogicalOperator.AND.isUnary(), is(false));
  }

  @Test
  public void andOperatorEvaluation() {
    assertTrue(LogicalOperator.AND.evaluate(true));
    assertTrue(LogicalOperator.AND.evaluate(true, true));
    assertTrue(LogicalOperator.AND.evaluate(true, true, true));
    assertFalse(LogicalOperator.AND.evaluate(true, false, true));
    assertFalse(LogicalOperator.AND.evaluate(false, true));
    assertFalse(LogicalOperator.AND.evaluate(true, false));
    assertFalse(LogicalOperator.AND.evaluate(false));
  }

  @Test
  @SuppressWarnings("unchecked")
  public void andOperatorSupplierEvaluation() {
    Supplier<Boolean> mockSupplierOne = mockSupplier("MockSupplierOne");
    Supplier<Boolean> mockSupplierTwo = mockSupplier("MockSupplierTwo");

    when(mockSupplierOne.get()).thenReturn(true);
    when(mockSupplierTwo.get()).thenReturn(true);

    assertThat(LogicalOperator.AND.evaluate(mockSupplierOne, mockSupplierTwo), is(true));

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

    assertThat(LogicalOperator.AND.evaluate(mockSupplierOne, mockSupplierTwo), is(false));

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, never()).get();
  }

  @Test
  public void negateAndOperatorEvaluation() {
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(false));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, false, false));
  }

  @Test
  public void notOperator() {
    LogicalOperator NOT = new LogicalOperator.LogicalNot(LogicalOperator.AND);

    assertThat(NOT, is(instanceOf(LogicalOperator.LogicalNot.class)));
    assertThat(NOT.getDescription(), is(equalTo("not")));
    assertThat(NOT.getOpposite(), is(sameInstance(NOT)));
    assertThat(NOT.getSymbol(), is(equalTo("!")));
    assertThat(NOT.isBinary(), is(false));
    assertThat(NOT.isTernary(), is(false));
    assertThat(NOT.isUnary(), is(true));
  }

  @Test
  public void orOperator() {
    assertThat(LogicalOperator.OR, is(notNullValue()));
    assertThat(LogicalOperator.OR.getDescription(), is(equalTo("or")));
    assertThat(LogicalOperator.OR.getOpposite(), is(equalTo(LogicalOperator.AND)));
    assertThat(LogicalOperator.OR.getSymbol(), is(equalTo("||")));
    assertThat(LogicalOperator.OR.isBinary(), is(true));
    assertThat(LogicalOperator.OR.isTernary(), is(false));
    assertThat(LogicalOperator.OR.isUnary(), is(false));
  }

  @Test
  public void orOperatorEvaluation() {
    assertTrue(LogicalOperator.OR.evaluate(true));
    assertTrue(LogicalOperator.OR.evaluate(true, true));
    assertTrue(LogicalOperator.OR.evaluate(true, true, true));
    assertTrue(LogicalOperator.OR.evaluate(false, false, true));
    assertTrue(LogicalOperator.OR.evaluate(false, true, false));
    assertTrue(LogicalOperator.OR.evaluate(true, false));
    assertTrue(LogicalOperator.OR.evaluate(false, true));
    assertFalse(LogicalOperator.OR.evaluate(false));
    assertFalse(LogicalOperator.OR.evaluate(false, false));
    assertFalse(LogicalOperator.OR.evaluate(false, false, false));
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

    assertThat(LogicalOperator.OR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree), is(false));

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

    assertThat(LogicalOperator.OR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree), is(true));

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, never()).get();
    verify(mockSupplierThree, never()).get();
  }

  @Test
  public void negateOrOperatorEvaluation() {
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, false));
  }

  @Test
  public void xorOperator() {
    assertThat(LogicalOperator.XOR, is(notNullValue()));
    assertThat(LogicalOperator.XOR.getDescription(), is(equalTo("xor")));
    assertThat(LogicalOperator.XOR.getOpposite(), is(sameInstance(LogicalOperator.XOR)));
    assertThat(LogicalOperator.XOR.getSymbol(), is(equalTo("^")));
    assertThat(LogicalOperator.XOR.isBinary(), is(true));
    assertThat(LogicalOperator.XOR.isTernary(), is(false));
    assertThat(LogicalOperator.XOR.isUnary(), is(false));
  }

  @Test
  public void xorOperatorEvaluation() {
    assertTrue(LogicalOperator.XOR.evaluate(true));
    assertTrue(LogicalOperator.XOR.evaluate(true, false));
    assertTrue(LogicalOperator.XOR.evaluate(true, false, false));
    assertFalse(LogicalOperator.XOR.evaluate(false));
    assertFalse(LogicalOperator.XOR.evaluate(false, false));
    assertFalse(LogicalOperator.XOR.evaluate(false, false, false));
    assertFalse(LogicalOperator.XOR.evaluate(true, false, true));
    assertFalse(LogicalOperator.XOR.evaluate(false, true, true));
    assertFalse(LogicalOperator.XOR.evaluate(true, true, true));
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

    assertThat(LogicalOperator.XOR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree, mockSupplierFour),
      is(true));

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

    assertThat(LogicalOperator.XOR.evaluate(mockSupplierOne, mockSupplierTwo, mockSupplierThree, mockSupplierFour),
      is(false));

    verify(mockSupplierOne, times(1)).get();
    verify(mockSupplierTwo, times(1)).get();
    verify(mockSupplierThree, never()).get();
    verify(mockSupplierFour, never()).get();
  }

  @Test
  public void negateXorOperatorEvaluation() {
    assertFalse(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true));
    assertFalse(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false));
    assertFalse(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(false, true, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.XOR).evaluate(true, true, true));
  }

}
