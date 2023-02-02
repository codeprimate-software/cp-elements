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
package org.cp.elements.biz.rules;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;

import org.junit.Test;

import org.cp.elements.lang.annotation.Nullable;

/**
 * Unit Tests for {@link AbstractRule}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.biz.rules.AbstractRule
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AbstractRuleUnitTests {

  @Test
  public void setAndGetId() {

    AbstractRule<Object, Long> rule = new TestRule<>();

    rule.setId(1L);

    assertThat(rule.getId().longValue()).isOne();
    assertThat(rule.<Rule<Object, Long>>identifiedBy(2L)).isSameAs(rule);
    assertThat(rule.getId()).isEqualTo(2L);
  }

  @Test
  public void setIdToNull() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new TestRule<Object, Long>().setId(null))
      .withMessage("Identifier for Rule [%s] is required", TestRule.class.getName())
      .withNoCause();
  }

  @Test
  public void getIdWhenUnset() {

    assertThatIllegalStateException()
      .isThrownBy(() -> new TestRule<Object, Long>().getId())
      .withMessage("Identifier for Rule [%s] was not properly initialized", TestRule.class.getName())
      .withNoCause();
  }

  @Test
  public void setAndGetExpectedOutcome() {

    AbstractRule<Object, Long> rule = new TestRule<>();

    assertThat(rule.getExpectedOutcome()).isEqualTo(AbstractRule.DEFAULT_EXPECTED_OUTCOME);

    rule.setExpectedOutcome(true);

    assertThat(rule.getExpectedOutcome()).isTrue();

    rule.setExpectedOutcome(false);

    assertThat(rule.getExpectedOutcome()).isFalse();
  }

  @Test
  public void setAndIsThrowExceptionOnFailure() {

    AbstractRule<Object, Long> rule = new TestRule<>();

    assertThat(rule.isThrowExceptionOnFailure()).isEqualTo(AbstractRule.DEFAULT_THROW_EXCEPTION_ON_FAILURE);

    rule.setThrowExceptionOnFailure(true);

    assertThat(rule.isThrowExceptionOnFailure()).isTrue();

    rule.setThrowExceptionOnFailure(false);

    assertThat(rule.isThrowExceptionOnFailure()).isFalse();
  }

  static final class TestRule<T, ID extends Comparable<ID>> extends AbstractRule<T, ID> {

    private final boolean evaluateResult;

    public TestRule() {
      this(true);
    }

    public TestRule(boolean evaluateResult) {
      this.evaluateResult = evaluateResult;
    }

    @Override
    public boolean evaluate(@Nullable Object obj) {
      return this.evaluateResult;
    }
  }
}
