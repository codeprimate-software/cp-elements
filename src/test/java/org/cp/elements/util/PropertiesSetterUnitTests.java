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
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verifyNoInteractions;

import java.util.Arrays;
import java.util.Properties;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link PropertiesSetter}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.util.PropertiesSetter
 * @since 1.0.0
 */
public class PropertiesSetterUnitTests {

  private PropertiesSetter newPropertiesSetter(String propertyName) {
    return new PropertiesSetter(propertyName) { };
  }

  @Test
  public void constructPropertiesSetterIsCorrect() {

    PropertiesSetter propertiesSetter = new PropertiesSetter("mockKey") { };

    assertThat(propertiesSetter.getPropertyName()).isEqualTo("mockKey");
  }

  @Test
  public void constructPropertiesSetterWithInvalidPropertyNameThrowsIllegalArgumentException() {

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> newPropertiesSetter(propertyName))
        .withMessage("Property name [%s] must be specified", propertyName)
        .withNoCause());
  }

  @Test
  public void setWithPropertyNameIsCorrect() {

    PropertiesSetter propertiesSetter = PropertiesSetter.set("mockKey");

    assertThat(propertiesSetter).isNotNull();
    assertThat(propertiesSetter.getPropertyName()).isEqualTo("mockKey");
  }

  @Test
  public void setWithInvalidPropertyNameThrowsIllegalArgumentException() {

    Arrays.stream(ArrayUtils.asArray("  ", "", null)).forEach(propertyName ->
      assertThatIllegalArgumentException()
        .isThrownBy(() -> PropertiesSetter.set(propertyName))
        .withMessage("Property name [%s] must be specified", propertyName)
        .withNoCause());
  }

  @Test
  public void getPropertiesWhenSetIsCorrect() {

    Properties mockProperties = mock(Properties.class);

    assertThat(PropertiesSetter.set("mockKey").of(mockProperties).getProperties()).isSameAs(mockProperties);

    verifyNoInteractions(mockProperties);
  }

  @Test
  public void getPropertiesWhenUnsetThrowsIllegalStateException() {

    assertThatIllegalStateException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").getProperties())
      .withMessage("Properties were not initialized")
      .withNoCause();
  }

  @Test
  public void ofNonNullProperties() {

    Properties mockProperties = mock(Properties.class);

    PropertiesSetter propertiesSetter = PropertiesSetter.set("mockKey");

    assertThat(propertiesSetter).isNotNull();
    assertThat(propertiesSetter.of(mockProperties)).isSameAs(propertiesSetter);

    verifyNoInteractions(mockProperties);
  }

  @Test
  public void ofNullPropertiesThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").of(null))
      .withMessage("Properties are required")
      .withNoCause();
  }

  @Test
  public void toPropertyValueIsCorrect() {

    Properties properties = new Properties();

    assertThat(properties).doesNotContainKey("mockKey");

    PropertiesSetter.set("mockKey").of(properties).to("test");

    assertThat(properties).containsKey("mockKey");
    assertThat(properties.getProperty("mockKey")).isEqualTo("test");
  }

  @Test
  public void toNullPropertyValueThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").of(new Properties()).to(null))
      .withMessage("Property value is required")
      .withNoCause();
  }

  @Test
  public void toPropertyValueWhenPropertiesIsUnsetThrowsIllegalStateException() {

    assertThatIllegalStateException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").to("test"))
      .withMessage("Properties were not initialized")
      .withNoCause();
  }

  @Test
  public void withSourceProperties() {

    Properties source = new Properties();
    Properties target = new Properties();

    source.setProperty("mockKey", "test");

    assertThat(source.getProperty("mockKey")).isEqualTo("test");
    assertThat(target).doesNotContainKey("mockKey");

    PropertiesSetter.set("mockKey").of(target).with(source);

    assertThat(target).containsKey("mockKey");
    assertThat(target.getProperty("mockKey")).isEqualTo("test");
  }

  @Test
  public void withNullSourceProperties() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").of(new Properties()).with(null))
      .withMessage("Source Properties are required")
      .withNoCause();
  }

  @Test
  public void withNullTargetProperties() {

    Properties source = PropertiesUtils.singletonProperties("mockKey", "test");

    assertThat(source).isNotNull();
    assertThat(source).containsKey("mockKey");

    assertThatIllegalStateException()
      .isThrownBy(() -> PropertiesSetter.set("mockKey").with(source))
      .withMessage("Properties were not initialized")
      .withNoCause();
  }

  @Test
  public void withSourcePropertiesWhenPropertyNameNotPresentThrowsIllegalStateException() {

    Properties source = new Properties();
    Properties target = new Properties();

    assertThat(source).doesNotContainKey("nonExistingKey");
    assertThat(target).doesNotContainKey("nonExistingKey");

    assertThatIllegalStateException()
      .isThrownBy(() -> PropertiesSetter.set("nonExistingKey").of(target).with(source))
      .withMessage("Source Properties does not contain property name [nonExistingKey]")
      .withNoCause();

    assertThat(source).doesNotContainKey("nonExistingKey");
    assertThat(target).doesNotContainKey("nonExistingKey");
  }
}
