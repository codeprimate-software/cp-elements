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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link RelationalOperator}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.RelationalOperator
 * @since 1.0.0
 */
public class RelationalOperatorTest {

  private static final RelationalOperator<String> NULL = null;

  @Test
  public void testEqualTo() {

    RelationalOperator<String> equalTo = RelationalOperator.equalTo("test");

    assertThat(equalTo).isNotNull();
    assertThat(equalTo.getDescription()).isEqualTo("equal to");
    assertThat(equalTo.getSymbol()).isEqualTo("==");
  }

  @Test
  public void testEqualToEvaluation() {

    RelationalOperator<Double> equalTo = RelationalOperator.equalTo(3.0d);

    assertThat(equalTo.evaluate(0.0d)).isFalse();
    assertThat(equalTo.evaluate(1.0d)).isFalse();
    assertThat(equalTo.evaluate(2.99d)).isFalse();
    assertThat(equalTo.evaluate(-3.0d)).isFalse();
    assertThat(equalTo.evaluate(3.0)).isTrue();
    assertThat(equalTo.evaluate(3.01d)).isFalse();
    assertThat(equalTo.evaluate(5.0d)).isFalse();
    assertThat(equalTo.evaluate(13.0d)).isFalse();
    assertThat(equalTo.evaluate(30.0d)).isFalse();
  }

  @Test
  public void testGreaterThan() {

    RelationalOperator<String> greaterThan = RelationalOperator.greaterThan("test");

    assertThat(greaterThan).isNotNull();
    assertThat(greaterThan.getDescription()).isEqualTo("greater than");
    assertThat(greaterThan.getSymbol()).isEqualTo(">");
  }

  @Test
  public void testGreaterThanEvaluation() {

    RelationalOperator<Double> greaterThan = RelationalOperator.greaterThan(3.0d);

    assertThat(greaterThan.evaluate(1.0d)).isFalse();
    assertThat(greaterThan.evaluate(2.0d)).isFalse();
    assertThat(greaterThan.evaluate(3.0d)).isFalse();
    assertThat(greaterThan.evaluate(4.0d)).isTrue();
    assertThat(greaterThan.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testGreaterThanAndLessThan() {

    RelationalOperator<String> greaterThanAndLessThan =
      RelationalOperator.greaterThanAndLessThan("test", "testing");

    assertThat(greaterThanAndLessThan).isNotNull();
    assertThat(greaterThanAndLessThan.getDescription()).isEqualTo("greater than and less than");
    assertThat(greaterThanAndLessThan.getSymbol()).isEqualTo("> && <");
  }

  @Test
  public void testGreaterThanAndLessThanEvaluation() {

    RelationalOperator<Double> greaterThanAndLessThan =
      RelationalOperator.greaterThanAndLessThan(2.0d, 4.0d);

    assertThat(greaterThanAndLessThan.evaluate(1.0d)).isFalse();
    assertThat(greaterThanAndLessThan.evaluate(2.0d)).isFalse();
    assertThat(greaterThanAndLessThan.evaluate(3.0d)).isTrue();
    assertThat(greaterThanAndLessThan.evaluate(4.0d)).isFalse();
    assertThat(greaterThanAndLessThan.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testGreaterThanAndLessThanEqualTo() {

    RelationalOperator<String> greaterThanAndLessThanEqualTo =
      RelationalOperator.greaterThanAndLessThanEqualTo("test", "testing");

    assertThat(greaterThanAndLessThanEqualTo).isNotNull();
    assertThat(greaterThanAndLessThanEqualTo.getDescription()).isEqualTo("greater than and less than equal to");
    assertThat(greaterThanAndLessThanEqualTo.getSymbol()).isEqualTo("> && <=");
  }

  @Test
  public void testGreaterThanAndLessThanEqualToEvaluation() {

    RelationalOperator<Double> greaterThanAndLessThanEqualTo =
      RelationalOperator.greaterThanAndLessThanEqualTo(2.0d, 4.0d);

    assertThat(greaterThanAndLessThanEqualTo.evaluate(1.0d)).isFalse();
    assertThat(greaterThanAndLessThanEqualTo.evaluate(2.0d)).isFalse();
    assertThat(greaterThanAndLessThanEqualTo.evaluate(3.0d)).isTrue();
    assertThat(greaterThanAndLessThanEqualTo.evaluate(4.0d)).isTrue();
    assertThat(greaterThanAndLessThanEqualTo.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testGreaterThanEqualTo() {

    RelationalOperator<String> greaterThanEqualTo = RelationalOperator.greaterThanEqualTo("test");

    assertThat(greaterThanEqualTo).isNotNull();
    assertThat(greaterThanEqualTo.getDescription()).isEqualTo("greater than equal to");
    assertThat(greaterThanEqualTo.getSymbol()).isEqualTo(">=");
  }

  @Test
  public void testGreaterThanEqualToEvaluation() {

    RelationalOperator<Double> greaterThanEqualTo = RelationalOperator.greaterThanEqualTo(3.0d);

    assertThat(greaterThanEqualTo.evaluate(1.0d)).isFalse();
    assertThat(greaterThanEqualTo.evaluate(2.0d)).isFalse();
    assertThat(greaterThanEqualTo.evaluate(3.0d)).isTrue();
    assertThat(greaterThanEqualTo.evaluate(4.0d)).isTrue();
    assertThat(greaterThanEqualTo.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testGreaterThanEqualToAndLessThan() {

    RelationalOperator<String> greaterThanEqualToAndLessThan =
      RelationalOperator.greaterThanEqualToAndLessThan("test", "testing");

    assertThat(greaterThanEqualToAndLessThan).isNotNull();
    assertThat(greaterThanEqualToAndLessThan.getDescription()).isEqualTo("greater than equal to and less than");
    assertThat(greaterThanEqualToAndLessThan.getSymbol()).isEqualTo(">= && <");
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEvaluation() {

    RelationalOperator<Double> greaterThanEqualToAndLessThan =
      RelationalOperator.greaterThanEqualToAndLessThan(2.0d, 4.0d);

    assertThat(greaterThanEqualToAndLessThan.evaluate(1.0d)).isFalse();
    assertThat(greaterThanEqualToAndLessThan.evaluate(2.0d)).isTrue();
    assertThat(greaterThanEqualToAndLessThan.evaluate(3.0d)).isTrue();
    assertThat(greaterThanEqualToAndLessThan.evaluate(4.0d)).isFalse();
    assertThat(greaterThanEqualToAndLessThan.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEqualTo() {

    RelationalOperator<String> greaterThanEqualToAndLessThanEqualTo =
      RelationalOperator.greaterThanEqualToAndLessThanEqualTo("test", "testing");

    assertThat(greaterThanEqualToAndLessThanEqualTo).isNotNull();
    assertThat(greaterThanEqualToAndLessThanEqualTo.getDescription()).isEqualTo(
      "greater than equal to and less than equal to");
    assertThat(greaterThanEqualToAndLessThanEqualTo.getSymbol()).isEqualTo(">= && <=");
  }

  @Test
  public void testGreaterThanEqualToAndLessThanEqualToEvaluation() {

    RelationalOperator<Double> greaterThanEqualToAndLessThanEqualTo =
      RelationalOperator.greaterThanEqualToAndLessThanEqualTo(2.0d, 4.0d);

    assertThat(greaterThanEqualToAndLessThanEqualTo.evaluate(1.0d)).isFalse();
    assertThat(greaterThanEqualToAndLessThanEqualTo.evaluate(2.0d)).isTrue();
    assertThat(greaterThanEqualToAndLessThanEqualTo.evaluate(3.0d)).isTrue();
    assertThat(greaterThanEqualToAndLessThanEqualTo.evaluate(4.0d)).isTrue();
    assertThat(greaterThanEqualToAndLessThanEqualTo.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testLessThan() {

    RelationalOperator<String> lessThan = RelationalOperator.lessThan("test");

    assertThat(lessThan).isNotNull();
    assertThat(lessThan.getDescription()).isEqualTo("less than");
    assertThat(lessThan.getSymbol()).isEqualTo("<");
  }

  @Test
  public void testLessThanEvaluation() {

    RelationalOperator<Double> lessThan = RelationalOperator.lessThan(3.0d);

    assertThat(lessThan.evaluate(1.0d)).isTrue();
    assertThat(lessThan.evaluate(2.0d)).isTrue();
    assertThat(lessThan.evaluate(3.0d)).isFalse();
    assertThat(lessThan.evaluate(4.0d)).isFalse();
    assertThat(lessThan.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testLessThanOrGreaterThan() {

    RelationalOperator<String> lessThanOrGreaterThan =
      RelationalOperator.lessThanOrGreaterThan("test", "testing");

    assertThat(lessThanOrGreaterThan).isNotNull();
    assertThat(lessThanOrGreaterThan.getDescription()).isEqualTo("less than or greater than");
    assertThat(lessThanOrGreaterThan.getSymbol()).isEqualTo("< || >");
  }

  @Test
  public void testLessThanOrGreaterThanEvaluation() {

    RelationalOperator<Double> lessThanOrGreaterThan = RelationalOperator.lessThanOrGreaterThan(2.0d, 4.0d);

    assertThat(lessThanOrGreaterThan.evaluate(1.0d)).isTrue();
    assertThat(lessThanOrGreaterThan.evaluate(2.0d)).isFalse();
    assertThat(lessThanOrGreaterThan.evaluate(3.0d)).isFalse();
    assertThat(lessThanOrGreaterThan.evaluate(4.0d)).isFalse();
    assertThat(lessThanOrGreaterThan.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testLessThanOrGreaterThanEqualTo() {

    RelationalOperator<String> lessThanOrGreaterThanEqualTo =
      RelationalOperator.lessThanOrGreaterThanEqualTo("test", "testing");

    assertThat(lessThanOrGreaterThanEqualTo).isNotNull();
    assertThat(lessThanOrGreaterThanEqualTo.getDescription()).isEqualTo("less than or greater than equal to");
    assertThat(lessThanOrGreaterThanEqualTo.getSymbol()).isEqualTo("< || >=");
  }

  @Test
  public void testLessThanOrGreaterThanEqualToEvaluation() {

    RelationalOperator<Double> lessThanOrGreaterThanEqualTo =
      RelationalOperator.lessThanOrGreaterThanEqualTo(2.0d, 4.0d);

    assertThat(lessThanOrGreaterThanEqualTo.evaluate(1.0d)).isTrue();
    assertThat(lessThanOrGreaterThanEqualTo.evaluate(2.0d)).isFalse();
    assertThat(lessThanOrGreaterThanEqualTo.evaluate(3.0d)).isFalse();
    assertThat(lessThanOrGreaterThanEqualTo.evaluate(4.0d)).isTrue();
    assertThat(lessThanOrGreaterThanEqualTo.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testLessThanEqualTo() {

    RelationalOperator<String> lessThanEqualTo = RelationalOperator.lessThanEqualTo("test");

    assertThat(lessThanEqualTo).isNotNull();
    assertThat(lessThanEqualTo.getDescription()).isEqualTo("less than equal to");
    assertThat(lessThanEqualTo.getSymbol()).isEqualTo("<=");
  }

  @Test
  public void testLessThanEqualToEvaluation() {

    RelationalOperator<Double> lessThanEqualTo = RelationalOperator.lessThanEqualTo(3.0d);

    assertThat(lessThanEqualTo.evaluate(1.0d)).isTrue();
    assertThat(lessThanEqualTo.evaluate(2.0d)).isTrue();
    assertThat(lessThanEqualTo.evaluate(3.0d)).isTrue();
    assertThat(lessThanEqualTo.evaluate(4.0d)).isFalse();
    assertThat(lessThanEqualTo.evaluate(5.0d)).isFalse();
  }

  @Test
  public void testLessThanEqualToOrGreaterThan() {

    RelationalOperator<String> lessThanEqualToOrGreaterThan =
      RelationalOperator.lessThanEqualToOrGreaterThan("test", "testing");

    assertThat(lessThanEqualToOrGreaterThan).isNotNull();
    assertThat(lessThanEqualToOrGreaterThan.getDescription()).isEqualTo("less than equal to or greater than");
    assertThat(lessThanEqualToOrGreaterThan.getSymbol()).isEqualTo("<= || >");
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEvaluation() {

    RelationalOperator<Double> lessThanEqualToOrGreaterThan =
      RelationalOperator.lessThanEqualToOrGreaterThan(2.0d, 4.0d);

    assertThat(lessThanEqualToOrGreaterThan.evaluate(1.0d)).isTrue();
    assertThat(lessThanEqualToOrGreaterThan.evaluate(2.0d)).isTrue();
    assertThat(lessThanEqualToOrGreaterThan.evaluate(3.0d)).isFalse();
    assertThat(lessThanEqualToOrGreaterThan.evaluate(4.0d)).isFalse();
    assertThat(lessThanEqualToOrGreaterThan.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEqualTo() {

    RelationalOperator<String> lessThanEqualToOrGreaterThanEqualTo =
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo("test", "testing");

    assertThat(lessThanEqualToOrGreaterThanEqualTo).isNotNull();
    assertThat(lessThanEqualToOrGreaterThanEqualTo.getDescription()).isEqualTo(
      "less than equal to or greater than equal to");
    assertThat(lessThanEqualToOrGreaterThanEqualTo.getSymbol()).isEqualTo("<= || >=");
  }

  @Test
  public void testLessThanEqualToOrGreaterThanEqualToEvaluation() {

    RelationalOperator<Double> lessThanEqualToOrGreaterThanEqualTo =
      RelationalOperator.lessThanEqualToOrGreaterThanEqualTo(2.0d, 4.0d);

    assertThat(lessThanEqualToOrGreaterThanEqualTo.evaluate(1.0d)).isTrue();
    assertThat(lessThanEqualToOrGreaterThanEqualTo.evaluate(2.0d)).isTrue();
    assertThat(lessThanEqualToOrGreaterThanEqualTo.evaluate(3.0d)).isFalse();
    assertThat(lessThanEqualToOrGreaterThanEqualTo.evaluate(4.0d)).isTrue();
    assertThat(lessThanEqualToOrGreaterThanEqualTo.evaluate(5.0d)).isTrue();
  }

  @Test
  public void testRelationalOperatorInstantiationWithBlankDescription() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>(" ", "<>", "test"))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithEmptyDescription() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>("", "<>", "test"))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithNullDescription() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>(null, "<>", "test"))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithNullExpectedValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>("description", "<>", null))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithBlankSymbol() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>("description", " ", "test"))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithEmptySymbol() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>("description", "", "test"))
      .withNoCause();
  }

  @Test
  public void testRelationalOperatorInstantiationWithNullSymbol() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new MockRelationalOperator<>("description", null, "test"))
      .withNoCause();
  }

  @Test
  @SuppressWarnings("all")
  public void testRelationalOperatorComposition() {

    MockRelationalOperator<String> leftOperand = new MockRelationalOperator<>("left operand", "<>", "test");
    MockRelationalOperator<String> rightOperand = new MockRelationalOperator<>("right operand", "<>", "test");

    assertThat(RelationalOperator.ComposableRelationalOperator.compose(NULL, LogicalOperator.AND, NULL)).isNotNull();
    assertThat(RelationalOperator.ComposableRelationalOperator.compose(leftOperand, LogicalOperator.AND, null)).isSameAs(leftOperand);
    assertThat(RelationalOperator.ComposableRelationalOperator.compose(null, LogicalOperator.AND, rightOperand)).isSameAs(rightOperand);

    RelationalOperator op = RelationalOperator.ComposableRelationalOperator
      .compose(leftOperand, LogicalOperator.AND, rightOperand);

    assertThat(op).isInstanceOf(RelationalOperator.ComposableRelationalOperator.class);
    assertThat(((RelationalOperator.ComposableRelationalOperator) op).getLeftOperand()).isSameAs(leftOperand);
    assertThat(((RelationalOperator.ComposableRelationalOperator) op).getOperator()).isEqualTo(LogicalOperator.AND);
    assertThat(((RelationalOperator.ComposableRelationalOperator) op).getRightOperand()).isSameAs(rightOperand);
  }

  protected static final class MockRelationalOperator<T extends Comparable<T>>
      extends RelationalOperator.AbstractRelationalOperator<T> {

    public MockRelationalOperator(final String description, final String symbol, final T expectedValue) {
      super(description, symbol, expectedValue);
    }

    @Override
    public boolean evaluate(final T actualValue) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }
  }
}
