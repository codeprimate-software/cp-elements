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
 * Unit Tests for {@link Nameable}.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Nameable
 * @since 2.0.0
 */
public class NameableUnitTests {

  @Test
  void namedNameable() {

    Nameable<String> namedObject = Nameable.named("test");

    assertThat(namedObject).isNotNull();
    assertThat(namedObject.getName()).isEqualTo("test");
  }

  @Test
  @SuppressWarnings("all")
  void withNoName() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> Nameable.named(null))
      .withMessage("Name is required")
      .withNoCause();
  }
}
