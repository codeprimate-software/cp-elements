/*
 * Copyright 2016 Author or Authors.
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

package org.cp.elements.biz.rules;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

/**
 * The AbstractRuleTest class is a test suite of test cases testing the contract and functionality of the
 * AbstractRule class.
 *
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
