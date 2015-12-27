/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 *
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 *
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 *
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 *
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The RelationalOperatorTest class is a test suite of test cases testing the contract and functionality of the
 * RelationalOperator class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.RelationalOperator
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class RelationalOperatorTest {

  private static final RelationalOperator<String> NULL = null;

  @Test
  public void testEqualTo() {
    final RelationalOperator<String> equalTo = RelationalOperator.equalTo("test");

    assertNotNull(equalTo);
    assertEquals("equal to", equalTo.getDescription());
    assertEquals("==", equalTo.getSymbol());
  }

  @Test
  public void testEqualToEvaluation() {
    final RelationalOperator<Double> equalTo = RelationalOperator.equalTo(3.0d);

    assertFalse(equalTo.evaluate(0.0d));
    assertFalse(equalTo.evaluate(1.0d));
    assertFalse(equalTo.evaluate(2.99d));
    assertFalse(equalTo.evaluate(-3.0d));
    assertTrue(equalTo.evaluate(3.0));
    assertFalse(equalTo.evaluate(3.01d));
    assertFalse(equalTo.evaluate(5.0d));
    assertFalse(equalTo.evaluate(13.0d));
    assertFalse(equalTo.evaluate(30.0d));
  }

  @Test
  public void testGreaterThan() {
    final RelationalOperator<String> greaterThan = RelationalOperator.greaterThan("test");

    assertNotNull(greaterThan);
    assertEquals("greater than", greaterThan.getDescription());
    assertEquals(">", greaterThan.getSymbol());
  }

  @Test
  public void testGreaterThanEvaluation() {
    final RelationalOperator<Double> greaterThan = RelationalOperator.greaterThan(3.0d);

    assertFalse(greaterThan.evaluate(1.0d));
    assertFalse(greaterThan.evaluate(2.0d));
    assertFalse(greaterThan.evaluate(3.0d));
    assertTrue(greaterThan.evaluate(4.0d));
    assertTrue(greaterThan.evaluate(5.0d));
  }

  @Test
  public void testGreaterThanAndLessThan() {
    final RelationalOperator<String> greaterThanAndLessThan =
      RelationalOperator.greaterThanAndLessThan("test", "testing");

    assertNotNull(greaterThanAndLessThan);
    assertEquals("greater than and less than", greaterThanAndLessThan.getDescription());
    assertEquals("> && <", greaterThanAndLessThan.getSymbol());
  }

  @Test
  public void testGreaterThanAndLessThanEvaluation() {
    final RelationalOperator<Double> greaterThanAndLessThan = RelationalOperator.greaterThanAndLessThan(2.0d, 4.0d);

    assertFalse(greaterThanAndLessThan.evaluate(1.0d));
    assertFalse(greaterThanAndLessThan.evaluate(2.0d));
    assertTrue(greaterThanAndLessThan.evaluate(3.0d));
    assertFalse(greaterThanAndLessThan.evaluate(4.0d));
    assertFalse(greaterThanAndLessThan.evaluate(5.0d));
  }

  @Test
  public void testGreaterThanAndLessThanEqualTo() {
    final RelationalOperator<String> greaterThanAndLessThanEqualTo =
      RelationalOperator.greaterThanAndLessThanEqualTo("test", "testing");

    assertNotNull(greaterThanAndLessThanEqualTo);
    assertEquals("greater than and less than equal to", greaterThanAndLessThanEqualTo.getDescription());
    assertEquals("> && <=", greaterThanAndLessThanEqualTo.getSymbol());
  }

  @Test
  public void testGreaterThanAndLessThanEqualToEvaluation() {
    final RelationalOperator<Double> greaterThanAndLessThanEqualTo =
      RelationalOperator.greaterThanAndLessThanEqualTo(2.0d, 4.0d);

    assertFalse(greaterThanAndLessThanEqualTo.evaluate(1.0d));
    assertFalse(greaterThanAndLessThanEqualTo.evaluate(2.0d));
    assertTrue(greaterThanAndLessThanEqualTo.evaluate(3.0d));
    assertTrue(greaterThanAndLessThanEqualTo.evaluate(4.0d));
    assertFalse(greaterThanAndLessThanEqualTo.evaluate(5.0d));
  }

  @Test
  public void testGreaterThanEqualTo() {
    final RelationalOperator<String> greaterThanEqualTo = RelationalOperator.greaterThanEqualTo("test");

    assertNotNull(greaterThanEqualTo);
    assertEquals("greater than equal to", greaterThanEqualTo.getDescription());
    assertEquals(">=", greaterThanEqualTo.getSymbol());
  }

  @Test
  public void testGreaterThanEqualToEvaluation() {
    final RelationalOperator<Double> greaterThanEqualTo = RelationalOperator.greaterThanEqualTo(3.0d);

    assertFalse(greaterThanEqualTo.evaluate(1.0d));
    assertFalse(greaterThanEqualTo.evaluate(2.0d));
    assertTrue(greaterThanEqualTo.evaluate(3.0d));
    assertTrue(greaterThanEqualTo.evaluate(4.0d));
    assertTrue(greaterThanEqualTo.evaluate(5.0d));
  }

  @Test
  public void testGreaterThanEqualToAndLessThan() {
    final RelationalOperator<String> greaterThanEqualToAndLessThan =
      RelationalOperator.greaterThanEqualToAndLessThan("test", "testing");

    assertNotNull(greaterThanEqualToAndLessThan);
    assertEquals("greater than equal to and less than", greaterThanEqualToAndLessThan.getDescription());
    assertEquals(">= && <", greaterThanEqualToAndLessThan.getSymbol());
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEvaluation() {
    final RelationalOperator<Double> greaterThanEqualToAndLessThan =
      RelationalOperator.greaterThanEqualToAndLessThan(2.0d, 4.0d);

    assertFalse(greaterThanEqualToAndLessThan.evaluate(1.0d));
    assertTrue(greaterThanEqualToAndLessThan.evaluate(2.0d));
    assertTrue(greaterThanEqualToAndLessThan.evaluate(3.0d));
    assertFalse(greaterThanEqualToAndLessThan.evaluate(4.0d));
    assertFalse(greaterThanEqualToAndLessThan.evaluate(5.0d));
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEqualTo() {
    final RelationalOperator<String> greaterThanEqualToAndLessThanEqualTo =
      RelationalOperator.greaterThanEqualToAndLessThanEqualTo("test", "testing");

    assertNotNull(greaterThanEqualToAndLessThanEqualTo);
    assertEquals("greater than equal to and less than equal to", greaterThanEqualToAndLessThanEqualTo.getDescription());
    assertEquals(">= && <=", greaterThanEqualToAndLessThanEqualTo.getSymbol());
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEqualToEvaluation() {
    final RelationalOperator<Double> greaterThanEqualToAndLessThanEqualTo =
      RelationalOperator.greaterThanEqualToAndLessThanEqualTo(2.0d, 4.0d);

    assertFalse(greaterThanEqualToAndLessThanEqualTo.evaluate(1.0d));
    assertTrue(greaterThanEqualToAndLessThanEqualTo.evaluate(2.0d));
    assertTrue(greaterThanEqualToAndLessThanEqualTo.evaluate(3.0d));
    assertTrue(greaterThanEqualToAndLessThanEqualTo.evaluate(4.0d));
    assertFalse(greaterThanEqualToAndLessThanEqualTo.evaluate(5.0d));
  }

  @Test
  public void testLessThan() {
    final RelationalOperator<String> lessThan = RelationalOperator.lessThan("test");

    assertNotNull(lessThan);
    assertEquals("less than", lessThan.getDescription());
    assertEquals("<", lessThan.getSymbol());
  }

  @Test
  public void testLessThanEvaluation() {
    final RelationalOperator<Double> lessThan = RelationalOperator.lessThan(3.0d);

    assertTrue(lessThan.evaluate(1.0d));
    assertTrue(lessThan.evaluate(2.0d));
    assertFalse(lessThan.evaluate(3.0d));
    assertFalse(lessThan.evaluate(4.0d));
    assertFalse(lessThan.evaluate(5.0d));
  }

  @Test
  public void testLessThanOrGreaterThan() {
    final RelationalOperator<String> lessThanOrGreaterThan =
      RelationalOperator.lessThanOrGreaterThan("test", "testing");

    assertNotNull(lessThanOrGreaterThan);
    assertEquals("less than or greater than", lessThanOrGreaterThan.getDescription());
    assertEquals("< || >", lessThanOrGreaterThan.getSymbol());
  }

  @Test
  public void testLessThanOrGreaterThanEvaluation() {
    final RelationalOperator<Double> lessThanOrGreaterThan = RelationalOperator.lessThanOrGreaterThan(2.0d, 4.0d);

    assertTrue(lessThanOrGreaterThan.evaluate(1.0d));
    assertFalse(lessThanOrGreaterThan.evaluate(2.0d));
    assertFalse(lessThanOrGreaterThan.evaluate(3.0d));
    assertFalse(lessThanOrGreaterThan.evaluate(4.0d));
    assertTrue(lessThanOrGreaterThan.evaluate(5.0d));
  }

  @Test
  public void testLessThanOrGreaterThanEqualTo() {
    final RelationalOperator<String> lessThanOrGreaterThanEqualTo =
      RelationalOperator.lessThanOrGreaterThanEqualTo("test", "testing");

    assertNotNull(lessThanOrGreaterThanEqualTo);
    assertEquals("less than or greater than equal to", lessThanOrGreaterThanEqualTo.getDescription());
    assertEquals("< || >=", lessThanOrGreaterThanEqualTo.getSymbol());
  }

  @Test
  public void testLessThanOrGreaterThanEqualToEvaluation() {
    final RelationalOperator<Double> lessThanOrGreaterThanEqualTo =
      RelationalOperator.lessThanOrGreaterThanEqualTo(2.0d, 4.0d);

    assertTrue(lessThanOrGreaterThanEqualTo.evaluate(1.0d));
    assertFalse(lessThanOrGreaterThanEqualTo.evaluate(2.0d));
    assertFalse(lessThanOrGreaterThanEqualTo.evaluate(3.0d));
    assertTrue(lessThanOrGreaterThanEqualTo.evaluate(4.0d));
    assertTrue(lessThanOrGreaterThanEqualTo.evaluate(5.0d));
  }

  @Test
  public void testLessThanEqualTo() {
    final RelationalOperator<String> lessThanEqualTo = RelationalOperator.lessThanEqualTo("test");

    assertNotNull(lessThanEqualTo);
    assertEquals("less than equal to", lessThanEqualTo.getDescription());
    assertEquals("<=", lessThanEqualTo.getSymbol());
  }

  @Test
  public void testLessThanEqualToEvaluation() {
    final RelationalOperator<Double> lessThanEqualTo = RelationalOperator.lessThanEqualTo(3.0d);

    assertTrue(lessThanEqualTo.evaluate(1.0d));
    assertTrue(lessThanEqualTo.evaluate(2.0d));
    assertTrue(lessThanEqualTo.evaluate(3.0d));
    assertFalse(lessThanEqualTo.evaluate(4.0d));
    assertFalse(lessThanEqualTo.evaluate(5.0d));
  }

  @Test
  public void testLessThanEqualToOrGreaterThan() {
    final RelationalOperator<String> lessThanEqualToOrGreaterThan =
      RelationalOperator.lessThanEqualToOrGreaterThan("test", "testing");

    assertNotNull(lessThanEqualToOrGreaterThan);
    assertEquals("less than equal to or greater than", lessThanEqualToOrGreaterThan.getDescription());
    assertEquals("<= || >", lessThanEqualToOrGreaterThan.getSymbol());
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEvaluation() {
    final RelationalOperator<Double> lessThanEqualToOrGreaterThan =
      RelationalOperator.lessThanEqualToOrGreaterThan(2.0d, 4.0d);

    assertTrue(lessThanEqualToOrGreaterThan.evaluate(1.0d));
    assertTrue(lessThanEqualToOrGreaterThan.evaluate(2.0d));
    assertFalse(lessThanEqualToOrGreaterThan.evaluate(3.0d));
    assertFalse(lessThanEqualToOrGreaterThan.evaluate(4.0d));
    assertTrue(lessThanEqualToOrGreaterThan.evaluate(5.0d));
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEqualTo() {
    final RelationalOperator<String> lessThanEqualToOrGreaterThanEqualTo =
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo("test", "testing");

    assertNotNull(lessThanEqualToOrGreaterThanEqualTo);
    assertEquals("less than equal to or greater than equal to", lessThanEqualToOrGreaterThanEqualTo.getDescription());
    assertEquals("<= || >=", lessThanEqualToOrGreaterThanEqualTo.getSymbol());
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEqualToEvaluation() {
    final RelationalOperator<Double> lessThanEqualToOrGreaterThanEqualTo =
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(2.0d, 4.0d);

    assertTrue(lessThanEqualToOrGreaterThanEqualTo.evaluate(1.0d));
    assertTrue(lessThanEqualToOrGreaterThanEqualTo.evaluate(2.0d));
    assertFalse(lessThanEqualToOrGreaterThanEqualTo.evaluate(3.0d));
    assertTrue(lessThanEqualToOrGreaterThanEqualTo.evaluate(4.0d));
    assertTrue(lessThanEqualToOrGreaterThanEqualTo.evaluate(5.0d));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithBlankDescription() {
    new MockRelationalOperator<>(" ", "<>", "test");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithEmptyDescription() {
    new MockRelationalOperator<>("", "<>", "test");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithNullDescription() {
    new MockRelationalOperator<>(null, "<>", "test");
  }

  @Test(expected = NullPointerException.class)
  public void testRelationalOperatorInstantiationWithNullExpectedValue() {
    new MockRelationalOperator<>("description", "<>", null);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithBlankSymbol() {
    new MockRelationalOperator<>("description", " ", "test");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithEmptySymbol() {
    new MockRelationalOperator<>("description", "", "test");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testRelationalOperatorInstantiationWithNullSymbol() {
    new MockRelationalOperator<>("description", null, "test");
  }

  @Test
  public void testRelationalOperatorComposition() throws Exception {
    MockRelationalOperator<String> leftOperand = new MockRelationalOperator<>("left operand", "<>", "test");
    MockRelationalOperator<String> rightOperand = new MockRelationalOperator<>("right operand", "<>", "test");

    assertNull(RelationalOperator.ComposableRelationalOperator.compose(NULL, LogicalOperator.AND, NULL));
    assertSame(leftOperand, RelationalOperator.ComposableRelationalOperator.compose(leftOperand, LogicalOperator.AND, null));
    assertSame(rightOperand, RelationalOperator.ComposableRelationalOperator.compose(null, LogicalOperator.AND, rightOperand));

    RelationalOperator op = RelationalOperator.ComposableRelationalOperator.compose(
      leftOperand, LogicalOperator.AND, rightOperand);

    assertTrue(op instanceof RelationalOperator.ComposableRelationalOperator);
    assertSame(leftOperand, ((RelationalOperator.ComposableRelationalOperator) op).getLeftOperand());
    assertEquals(LogicalOperator.AND, ((RelationalOperator.ComposableRelationalOperator) op).getOperator());
    assertSame(rightOperand, ((RelationalOperator.ComposableRelationalOperator) op).getRightOperand());
  }

  protected static final class MockRelationalOperator<T extends Comparable<T>> extends RelationalOperator.AbstractRelationalOperator<T> {

    public MockRelationalOperator(final String description, final String symbol, final T expectedValue) {
      super(description, symbol, expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }

}
