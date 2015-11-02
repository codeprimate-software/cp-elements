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

package org.cp.elements.biz.rules;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The AbstractRuleTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractRule class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.biz.rules.AbstractRule
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractRuleTest {

  @Test
  public void testSetAndGetId() {
    AbstractRule<Object, Long> rule = new TestRule<>();

    rule.setId(1l);

    assertEquals(1l, rule.getId().longValue());
  }

  @Test(expected = NullPointerException.class)
  public void testSetIdWithNull() {
    try {
      new TestRule<Object, Long>().setId(null);
    }
    catch (NullPointerException expected) {
      assertEquals(String.format("The identifier for Rule (%1$s) cannot be null!", TestRule.class.getName()),
        expected.getMessage());
      throw expected;
    }
  }

  @Test(expected = IllegalStateException.class)
  public void testGetIdWithUnsetId() {
    try {
      new TestRule<Object, Long>().getId();
    }
    catch (IllegalStateException expected) {
      assertEquals(String.format("The identifier for Rule (%1$s) was not properly initialized!",
        TestRule.class.getName()), expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testSetAndGetExpectedOutcome() {
    AbstractRule<Object, Long> rule = new TestRule<>();

    assertEquals(AbstractRule.DEFAULT_EXPECTED_OUTCOME, rule.getExpectedOutcome());

    rule.setExpectedOutcome(true);

    assertTrue(rule.getExpectedOutcome());

    rule.setExpectedOutcome(false);

    assertFalse(rule.getExpectedOutcome());
  }

  @Test
  public void testSetAndIsThrowExceptionOnFailure() {
    AbstractRule<Object, Long> rule = new TestRule<>();

    assertEquals(AbstractRule.DEFAULT_THROW_EXCEPTION_ON_FAILURE, rule.isThrowExceptionOnFailure());

    rule.setThrowExceptionOnFailure(true);

    assertTrue(rule.isThrowExceptionOnFailure());

    rule.setThrowExceptionOnFailure(false);

    assertFalse(rule.isThrowExceptionOnFailure());
  }

  protected static final class TestRule<T, ID extends Comparable<ID>> extends AbstractRule<T, ID> {

    private final boolean evaluateResult;

    public TestRule() {
      this(true);
    }

    public TestRule(final boolean evaluateResult) {
      this.evaluateResult = evaluateResult;
    }

    @Override
    public boolean evaluate(final Object obj) {
      return evaluateResult;
    }
  }

}
