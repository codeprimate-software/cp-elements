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
 * Unit Tests for {@link Min}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Min
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class MinUnitTests {

  @Mock
  private Function<Object, Integer> mockFunction;

  @Test
  void newMin() {

    Min<Object, Integer> min = Min.of(this.mockFunction);

    assertThat(min).isNotNull();
    assertThat(min.getName()).isEqualTo(Constants.UNKNOWN);

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  void newNamedMin() {

    Min<Object, Integer> min = Min.of(this.mockFunction).named("TestMin");

    assertThat(min).isNotNull();
    assertThat(min.getName()).isEqualTo("TestMin");

    verifyNoInteractions(this.mockFunction);
  }

  @Test
  @SuppressWarnings("all")
  void newMinWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Min.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void findsMin() {

    Min<Integer, Integer> min = Min.<Integer, Integer>of(Function.identity()).named("Minimum Integer");

    assertThat(min).isNotNull();
    assertThat(min.getName()).isEqualTo("Minimum Integer");
    assertThat(min.apply(2, 4, 1, 8)).isEqualTo(1);
  }

  @Test
  void findsNoMin() {
    assertThat(Min.<Integer, Integer>of(Function.identity()).apply(null, null, null)).isNull();
  }
}
