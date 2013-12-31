/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.lang;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The LogicalOperatorTest class is a test suite of test cases testing the contract and functionality of the 
 * LogicalOperator class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.LogicalOperator
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class LogicalOperatorTest {

  @Test
  public void testAndOperator() {
    assertNotNull(LogicalOperator.AND);
    assertEquals("and", LogicalOperator.AND.getDescription());
    assertEquals(LogicalOperator.OR, LogicalOperator.AND.getOpposite());
    assertEquals("&&", LogicalOperator.AND.getSymbol());
    assertTrue(LogicalOperator.AND.isBinary());
    assertFalse(LogicalOperator.AND.isTernary());
    assertFalse(LogicalOperator.AND.isUnary());
  }

  @Test
  public void testAndOperatorEvaluation() {
    assertTrue(LogicalOperator.AND.evaluate(true));
    assertTrue(LogicalOperator.AND.evaluate(true, true));
    assertTrue(LogicalOperator.AND.evaluate(true, true, true));
    assertFalse(LogicalOperator.AND.evaluate(true, false, true));
    assertFalse(LogicalOperator.AND.evaluate(false, true));
    assertFalse(LogicalOperator.AND.evaluate(true, false));
    assertFalse(LogicalOperator.AND.evaluate(false));
  }

  @Test
  public void testNegateAndOperatorEvaluation() {
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, true, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(true, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.AND).evaluate(false));
  }

  @Test
  public void testOrOperator() {
    assertNotNull(LogicalOperator.OR);
    assertEquals("or", LogicalOperator.OR.getDescription());
    assertEquals(LogicalOperator.AND, LogicalOperator.OR.getOpposite());
    assertEquals("||", LogicalOperator.OR.getSymbol());
    assertTrue(LogicalOperator.OR.isBinary());
    assertFalse(LogicalOperator.OR.isTernary());
    assertFalse(LogicalOperator.OR.isUnary());
  }

  @Test
  public void testOrOperatorEvaluation() {
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
  public void testNegateOrOperatorEvaluation() {
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, true, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, true));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, true, false));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(true, false));
    assertFalse(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, true));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false));
    assertTrue(LogicalOperator.negate(LogicalOperator.OR).evaluate(false, false, false));
  }

  @Test
  public void testXorOperator() {
    assertNotNull(LogicalOperator.XOR);
    assertEquals("xor", LogicalOperator.XOR.getDescription());
    assertEquals("^", LogicalOperator.XOR.getSymbol());
    assertTrue(LogicalOperator.XOR.isBinary());
    assertFalse(LogicalOperator.XOR.isTernary());
    assertFalse(LogicalOperator.XOR.isUnary());
  }

  @Test
  public void testXorOperatorEvaluation() {
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
  public void testNegateXorOperatorEvaluation() {
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
