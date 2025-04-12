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
package org.cp.elements.data.conversion.converters;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.util.Arrays;
import java.util.UUID;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link UUIDConverter}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.conversion.converters.UUIDConverter
 * @since 2.0.0
 */
public class UUIDConverterUnitTests {

  private final UUIDConverter converter = new UUIDConverter();

  @Test
  void stringToUUID() {

    UUID uuid = UUID.randomUUID();
    String uuidString = uuid.toString();
    UUID value = this.converter.convert(uuidString);

    assertThat(value).isNotNull();
    assertThat(value).isNotSameAs(uuid).isEqualTo(uuid);
    assertThat(value).hasToString(uuidString);
  }

  @Test
  void fromInvalidString() {

    Arrays.asList("  ", "", null).forEach(string ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> this.converter.convert(string))
        .withMessage("String [%s] to convert to a UUID is required", string)
        .withNoCause());
  }
}
