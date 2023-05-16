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
package org.cp.elements.dao;

import static org.assertj.core.api.Assertions.assertThat;

import java.util.Arrays;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link CrudOperation}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.dao.CrudOperation
 * @since 1.0.0
 */
public class CrudOperationUnitTests {

  @Test
  public void valueOfIgnoreCaseWithEnumeratedValuesIsCorrect() {

    Arrays.stream(CrudOperation.values()).forEach(enumeratedValue ->
      assertThat(CrudOperation.valueOfIgnoreCase(enumeratedValue.name()).orElse(null)).isEqualTo(enumeratedValue));
  }

  @Test
  public void valueOfIgnoreCaseWithValidValuesIsCorrect() {

    assertThat(CrudOperation.valueOfIgnoreCase("CREATE").orElse(null)).isEqualTo(CrudOperation.CREATE);
    assertThat(CrudOperation.valueOfIgnoreCase("Read").orElse(null)).isEqualTo(CrudOperation.READ);
    assertThat(CrudOperation.valueOfIgnoreCase("update").orElse(null)).isEqualTo(CrudOperation.UPDATE);
    assertThat(CrudOperation.valueOfIgnoreCase(" DeLeTe  ").orElse(null)).isEqualTo(CrudOperation.DELETE);
  }

  @Test
  public void valueOfIgnoreCaseWithBlankValueReturnsOptionalEmpty() {
    assertThat(CrudOperation.valueOfIgnoreCase("  ").isPresent()).isFalse();
  }

  @Test
  public void valueOfIgnoreCaseWithEmptyValueReturnsOptionalEmpty() {
    assertThat(CrudOperation.valueOfIgnoreCase("").isPresent()).isFalse();
  }

  @Test
  public void valueOfIgnoreCaseWithInvalidValuesReturnsOptionalEmpty() {

    assertThat(CrudOperation.valueOfIgnoreCase("ATE").isPresent()).isFalse();
    assertThat(CrudOperation.valueOfIgnoreCase("INserT").isPresent()).isFalse();
    assertThat(CrudOperation.valueOfIgnoreCase("READING").isPresent()).isFalse();
    assertThat(CrudOperation.valueOfIgnoreCase("up").isPresent()).isFalse();
    assertThat(CrudOperation.valueOfIgnoreCase(" UP DATE  ").isPresent()).isFalse();
    assertThat(CrudOperation.valueOfIgnoreCase("Remove").isPresent()).isFalse();
  }

  @Test
  public void valueOfIgnoreCaseWithNullValueIsNullSafeReturnsOptionalEmpty() {
    assertThat(CrudOperation.valueOfIgnoreCase(null).isPresent()).isFalse();
  }
}
