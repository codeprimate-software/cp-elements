/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.functions;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.verifyNoInteractions;

import java.math.BigDecimal;
import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Constants;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link Sum}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Sum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class SumUnitTests {

  @Mock
  private Function<Object, Number> mockFunction;

  @Test
  void newSum() {

    Sum<Object> sum = Sum.of(this.mockFunction);

    assertThat(sum).isNotNull();
    assertThat(sum.getName()).isEqualTo(Constants.UNKNOWN);

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newNamedSum() {

    Sum<Object> sum = Sum.of(this.mockFunction).named("TestSum");

    assertThat(sum).isNotNull();
    assertThat(sum.getName()).isEqualTo("TestSum");

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newSumWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Sum.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void sumOfValues() {

    Sum<Integer> sum = Sum.<Integer>of(Function.identity()).named("Integer Sum");

    assertThat(sum).isNotNull();
    assertThat(sum.getName()).isEqualTo("Integer Sum");
    assertThat(sum.apply(1, 2, null, 3, 4, 5, null, 6, null, null)).isEqualTo(BigDecimal.valueOf(21.0));
  }

  @Test
  void sumOfNullValues() {
    assertThat(Sum.<Integer>of(Function.identity()).apply(null, null, null)).isEqualTo(BigDecimal.ZERO);
  }
}
