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
 * Unit Tests for {@link Avg}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Avg
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class AvgUnitTests {

  @Mock
  private Function<Object, Integer> mockFunction;

  @Test
  void newAvg() {

    Avg<Object> avg = Avg.of(this.mockFunction);

    assertThat(avg).isNotNull();
    assertThat(avg.getName()).isEqualTo(Constants.UNKNOWN);

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newNamedAvg() {

    Avg<Object> avg = Avg.of(this.mockFunction).named("TestAverage");

    assertThat(avg).isNotNull();
    assertThat(avg.getName()).isEqualTo("TestAverage");

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newNameWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Avg.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void averagesNumbers() {

    Avg<Integer> avg = Avg.<Integer>of(Function.identity()).named("Integer Average");

    assertThat(avg).isNotNull();
    assertThat(avg.getName()).isEqualTo("Integer Average");
    assertThat(avg.apply(1, 2, 3, 4, 5)).isEqualTo(BigDecimal.valueOf(3.0));
  }

  @Test
  void averageNoNumbers() {
    assertThat(Avg.of(Function.identity()).apply(null, null, null)).isEqualTo(BigDecimal.ZERO);
  }
}
