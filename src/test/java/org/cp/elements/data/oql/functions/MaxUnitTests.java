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

import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Constants;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link Max}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Max
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class MaxUnitTests {

  @Mock
  private Function<Object, Integer> mockFunction;

  @Test
  void newMax() {

    Max<Object, Integer> max = Max.of(this.mockFunction);

    assertThat(max).isNotNull();
    assertThat(max.getName()).isEqualTo(Constants.UNKNOWN);

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newNamedMax() {

    Max<Object, Integer> max = Max.of(this.mockFunction).named("TestMax");

    assertThat(max).isNotNull();
    assertThat(max.getName()).isEqualTo("TestMax");

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newMaxWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Max.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void findsMax() {

    Max<Integer, Integer> max = Max.<Integer, Integer>of(Function.identity()).named("Maximum Integer");

    assertThat(max).isNotNull();
    assertThat(max.getName()).isEqualTo("Maximum Integer");
    assertThat(max.apply(2, 8, 1, 4)).isEqualTo(8);
  }

  @Test
  void findsNoMax() {
    assertThat(Max.<Integer, Integer>of(Function.identity()).apply(null, null, null)).isNull();;
  }
}
