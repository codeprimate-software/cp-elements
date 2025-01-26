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
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.function.Function;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.security.model.User;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link Concat}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.functions.Concat
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
public class ConcatUnitTests {

  @Mock
  private Function<Object, ?> mockFunction;

  @Test
  @SuppressWarnings("unchecked")
  void newConcat() {

    Concat<Object> concat = Concat.of(this.mockFunction);

    assertThat(concat).isNotNull();
    assertThat(concat.getName()).isEqualTo(Constants.UNKNOWN);

    verify(this.mockFunction, times(1)).andThen(isA(Function.class));
  }

  @Test
  @SuppressWarnings("unchecked")
  void newNamedConcat() {

    Concat<Object> concat = Concat.of(this.mockFunction).named("TestConcat");

    assertThat(concat).isNotNull();
    assertThat(concat.getName()).isEqualTo("TestConcat");

    verify(this.mockFunction, times(1)).andThen(isA(Function.class));
  }

  @Test
  void newConcatWithNullFunction() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Concat.of(null))
      .withMessage("Function is required")
      .withNoCause();
  }

  @Test
  void concatStrings() {

    Concat<String> concat = Concat.<String>of(Function.identity()).named("String Concatenation");

    assertThat(concat).isNotNull();
    assertThat(concat.getName()).isEqualTo("String Concatenation");
    assertThat(concat.apply("test", "testing", "tested")).isEqualTo("test, testing, tested");
  }

  @Test
  void concatNoStrings() {

    Concat<Object> concat = Concat.of(Function.identity())
      .delimitedWith(StringUtils.SINGLE_SPACE)
      .named("String Concatenation");

    assertThat(concat).isNotNull();
    assertThat(concat.getName()).isEqualTo("String Concatenation");
    assertThat(concat.apply(null, null, null)).isEqualTo("null null null");
  }

  @Test
  @SuppressWarnings("unchecked")
  void concatUsernames() {

    Function<User<String>, String> function = User::getName;

    Concat<User<String>> concat = Concat.of(function)
      .delimitedWith(StringUtils.SEMICOLON_SPACE_SEPARATOR)
      .named("User Concatenation");

    assertThat(concat).isNotNull();
    assertThat(concat.getName()).isEqualTo("User Concatenation");
    assertThat(concat.apply(User.named("jonDoe"), User.named("janeDoe"), User.named("pieDoe")))
      .isEqualTo("jonDoe; janeDoe; pieDoe");
  }
}
