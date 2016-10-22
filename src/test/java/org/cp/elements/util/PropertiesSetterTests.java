/*
 * Copyright 2016 Author or Authors.
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
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.util.Properties;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link PropertiesSetter}.
 *
 * @author John J. Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.cp.elements.util.PropertiesSetter
 * @since 1.0.0
 */
public class PropertiesSetterTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void setWithValidPropertyNameIsSuccessful() {
    PropertiesSetter propertiesSetter = set("key");

    assertThat(propertiesSetter).isNotNull();
    assertThat(propertiesSetter.getPropertyName()).isEqualTo("key");
  }

  protected void assertSetWithIllegalPropertyNameThrowsIllegalArgumentException(String propertyName) {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Property name must be specified");

    set(propertyName);
  }

  @Test
  public void setWithIllegalPropertyNamesThrowsIllegalArgumentException() {
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException(null);
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException("");
    assertSetWithIllegalPropertyNameThrowsIllegalArgumentException("  ");
  }

  @Test
  public void getPropertiesWhenUnsetThrowsIllegalStateException() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Properties were not specified");

    set("key").getProperties();
  }

  @Test
  public void ofNonNullProperties() {
    Properties expected = new Properties();
    Properties actual = set("key").of(expected).getProperties();

    assertThat(actual).isSameAs(expected);
  }

  @Test
  public void ofNullPropertiesThrowsIllegalArgumentException() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Properties cannot be null");

    set("key").of(null).getProperties();
  }

  @Test
  public void toPropertyValueIsSuccessful() {
    Properties properties = new Properties();

    assertThat(properties.containsKey("key")).isFalse();

    set("key").of(properties).to("test");

    assertThat(properties.containsKey("key")).isTrue();
    assertThat(properties.getProperty("key")).isEqualTo("test");
  }

  @Test
  public void toPropertyValueWhenPropertiesIsUnsetThrowsIllegalStateException() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Properties were not specified");

    set("key").to("test");
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

  @Test
  public void withNullSourceProperties() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Source properties cannot be null");

    set("key").of(new Properties()).with(null);
  }

  @Test
  public void withNullTargetProperties() {
    exception.expect(IllegalStateException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("Properties were not specified");

    set("key").with(new Properties());
  }
}
