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
package org.cp.elements.util;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.Mockito.mock;

import java.util.Properties;

import org.junit.Test;

/**
 * Unit Tests for {@link PropertiesUtils}.
 *
 * @author John Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.PropertiesUtils
 * @since 1.0.0
 */
public class PropertiesUtilsUnitTests {

  @Test
  public void nullSafePropertiesWithNonNullProperties() {

    Properties mockProperties = mock(Properties.class);

    assertThat(PropertiesUtils.nullSafeProperties(mockProperties)).isSameAs(mockProperties);
  }

  @Test
  public void nullSafePropertiesWithNullProperties() {

    Properties properties = PropertiesUtils.nullSafeProperties(null);

    assertThat(properties).isNotNull();
    assertThat(properties).isEmpty();
  }

  @Test
  public void singletonPropertiesIsCorrect() {

    Properties properties = PropertiesUtils.singletonProperties("mockProperty", "mockValue");

    assertThat(properties).isNotNull();
    assertThat(properties).hasSize(1);
    assertThat(properties).containsKey("mockProperty");
    assertThat(properties.getProperty("mockProperty")).isEqualTo("mockValue");
  }

  @Test
  public void singletonPropertiesWithNullKey() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesUtils.singletonProperties(null, "value"))
      .withMessage("Property name is required")
      .withNoCause();
  }

  @Test
  public void singletonPropertiesWithNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesUtils.singletonProperties("key", null))
      .withMessage("Value for property [key] is required")
      .withNoCause();
  }
}
