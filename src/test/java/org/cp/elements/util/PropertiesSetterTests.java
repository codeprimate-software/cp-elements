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
import static org.cp.elements.util.PropertiesSetter.set;

import java.util.Properties;

import org.cp.elements.test.TestUtils;
import org.junit.Test;

/**
 * Unit Tests for {@link PropertiesSetter}.
 *
 * @author John J. Blum
 * @see java.util.Properties
 * @see org.junit.Test
 * @see org.cp.elements.util.PropertiesSetter
 * @since 1.0.0
 */
public class PropertiesSetterTests {

  @Test
  public void setWithValidPropertyNameIsSuccessful() {

    PropertiesSetter propertiesSetter = set("key");

    assertThat(propertiesSetter).isNotNull();
    assertThat(propertiesSetter.getPropertyName()).isEqualTo("key");
  }

  protected void assertSetWithIllegalPropertyNameThrowsIllegalArgumentException(String propertyName) {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> set(propertyName),
      () -> "Property name must be specified");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setWithBlankPropertyNamesThrowsIllegalArgumentException() {
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException("  ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setWithEmptyPropertyNamesThrowsIllegalArgumentException() {
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void setWithNullPropertyNamesThrowsIllegalArgumentException() {
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException(null);
  }

  @Test(expected = IllegalStateException.class)
  public void getPropertiesWhenUnsetThrowsIllegalStateException() {
    TestUtils.doIllegalStateExceptionThrowingOperation(() -> set("key").getProperties(),
      () -> "Properties were not specified");
  }

  @Test
  public void ofNonNullProperties() {

    Properties expected = new Properties();
    Properties actual = set("key").of(expected).getProperties();

    assertThat(actual).isSameAs(expected);
  }

  @Test(expected = IllegalArgumentException.class)
  public void ofNullPropertiesThrowsIllegalArgumentException() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> set("key").of(null).getProperties(),
      () -> "Properties cannot be null");
  }

  @Test
  public void toPropertyValueIsSuccessful() {

    Properties properties = new Properties();

    assertThat(properties.containsKey("key")).isFalse();

    set("key").of(properties).to("test");

    assertThat(properties.containsKey("key")).isTrue();
    assertThat(properties.getProperty("key")).isEqualTo("test");
  }

  @Test(expected = IllegalStateException.class)
  public void toPropertyValueWhenPropertiesIsUnsetThrowsIllegalStateException() {
    TestUtils.doIllegalStateExceptionThrowingOperation(() -> set("key").to("test"),
      () -> "Properties were not specified");
  }

  @Test
  public void withSourceProperties() {

    Properties source = new Properties();
    Properties target = new Properties();

    source.setProperty("key", "test");

    assertThat(source.getProperty("key")).isEqualTo("test");
    assertThat(target.containsKey("key")).isFalse();

    set("key").of(target).with(source);

    assertThat(target.containsKey("key")).isTrue();
    assertThat(target.getProperty("key")).isEqualTo(source.getProperty("key"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void withNullSourceProperties() {
    TestUtils.doIllegalArgumentExceptionThrowingOperation(() -> set("key").of(new Properties()).with(null),
      () -> "Source properties cannot be null");
  }

  @Test(expected = IllegalStateException.class)
  public void withNullTargetProperties() {
    TestUtils.doIllegalStateExceptionThrowingOperation(() -> set("key").with(new Properties()),
      () -> "Properties were not specified");
  }
}
