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
package org.cp.elements.context.configure.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.io.File;
import java.util.HashSet;
import java.util.Properties;
import java.util.Set;
import java.util.stream.StreamSupport;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.cp.elements.context.configure.AbstractConfiguration;
import org.cp.elements.context.configure.Configuration;

/**
 * Unit Tests for {@link PropertiesConfiguration}.
 *
 * @author John J. Blum
 * @see java.io.File
 * @see java.util.Properties
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.context.configure.AbstractConfiguration
 * @see org.cp.elements.context.configure.Configuration
 * @see org.cp.elements.context.configure.support.PropertiesConfiguration
 * @since 1.0.0
 */
public class PropertiesConfigurationUnitTests {

  private Properties properties;

  private PropertiesConfiguration configuration;

  @BeforeEach
  public void setup() {

    this.properties = new Properties();
    this.properties.setProperty("boolean.property", "true");
    this.properties.setProperty("character.property", "X");
    this.properties.setProperty("double.property", String.valueOf(Math.PI));
    this.properties.setProperty("integer.property", "2");
    this.properties.setProperty("string.property", "test");
    this.configuration = new PropertiesConfiguration(properties);

    assertThat(this.configuration.getProperties()).isSameAs(this.properties);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void constructPropertiesConfigurationWithProperties() {

    Properties properties = new Properties();

    PropertiesConfiguration configuration = new PropertiesConfiguration(properties);

    assertThat(configuration).isNotNull();
    assertThat(configuration.getProperties()).isEqualTo(properties);

    Configuration.Descriptor<Properties> configurationDescriptor =
      (Configuration.Descriptor<Properties>) configuration.getDescriptor();

    assertThat(configurationDescriptor).isInstanceOf(AbstractConfiguration.PropertiesConfigurationDescriptor.class);
    assertThat(configurationDescriptor.getSource()).isEqualTo(properties);
    assertThat(configurationDescriptor.isFile()).isFalse();
    assertThat(configurationDescriptor.isProperties()).isTrue();
  }

  @Test
  public void constructPropertiesConfigurationWithNullFile() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new PropertiesConfiguration((File) null))
      .withMessage("The file to load properties from is required")
      .withNoCause();
  }

  @Test
  public void constructPropertiesConfigurationWithNullProperties() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new PropertiesConfiguration((Properties) null))
      .withMessage("The Properties used to back this Configuration is required")
      .withNoCause();
  }

  @Test
  public void isPresentReturnsTrue() {

    assertThat(this.configuration.isPresent("boolean.property")).isTrue();
    assertThat(this.configuration.isPresent("character.property")).isTrue();
    assertThat(this.configuration.isPresent("double.property")).isTrue();
    assertThat(this.configuration.isPresent("integer.property")).isTrue();
    assertThat(this.configuration.isPresent("string.property")).isTrue();
  }

  @Test
  public void isPresentReturnsFalse() {

    assertThat(this.configuration.isPresent("big.decimal.property")).isFalse();
    assertThat(this.configuration.isPresent("big.integer.property")).isFalse();
    assertThat(this.configuration.isPresent("bool.prop")).isFalse();
    assertThat(this.configuration.isPresent("date.time.prop")).isFalse();
    assertThat(this.configuration.isPresent("chr.prop")).isFalse();
    assertThat(this.configuration.isPresent("dbl.prop")).isFalse();
    assertThat(this.configuration.isPresent("int.prop")).isFalse();
    assertThat(this.configuration.isPresent("str.prop")).isFalse();
  }

  @Test
  public void doGetPropertyValueIsCorrect() {

    assertThat(this.configuration.doGetPropertyValue("boolean.property")).isEqualTo("true");
    assertThat(this.configuration.doGetPropertyValue("character.property")).isEqualTo("X");
    assertThat(this.configuration.doGetPropertyValue("double.property")).isEqualTo(String.valueOf(Math.PI));
    assertThat(this.configuration.doGetPropertyValue("integer.property")).isEqualTo("2");
    assertThat(this.configuration.doGetPropertyValue("string.property")).isEqualTo("test");
  }

  @Test
  public void doGetPropertyValueReturnsNull() {

    assertThat(this.configuration.doGetPropertyValue("bool.prop")).isNull();
    assertThat(this.configuration.doGetPropertyValue("chr.prop")).isNull();
    assertThat(this.configuration.doGetPropertyValue("dbl.prop")).isNull();
    assertThat(this.configuration.doGetPropertyValue("int.prop")).isNull();
    assertThat(this.configuration.doGetPropertyValue("str.prop")).isNull();
    assertThat(this.configuration.doGetPropertyValue("unset.property")).isNull();
  }

  @Test
  public void doGetParentPropertyValueIsCorrect() {

    Properties parentConfigurationSetting = new Properties();

    parentConfigurationSetting.setProperty("bool.prop", "false");
    parentConfigurationSetting.setProperty("char.prop", "A");
    parentConfigurationSetting.setProperty("str.prop", "mock");

    PropertiesConfiguration configuration =
      new PropertiesConfiguration(this.properties, new PropertiesConfiguration(parentConfigurationSetting));

    assertThat(configuration).isNotNull();
    assertThat(configuration.getProperties()).isSameAs(this.properties);
    assertThat(configuration.getPropertyValue("boolean.property")).isEqualTo("true");
    assertThat(configuration.getPropertyValue("bool.prop")).isEqualTo("false");
    assertThat(configuration.getPropertyValue("character.property")).isEqualTo("X");
    assertThat(configuration.getPropertyValue("char.prop")).isEqualTo("A");
    assertThat(configuration.getPropertyValue("string.property")).isEqualTo("test");
    assertThat(configuration.getPropertyValue("str.prop")).isEqualTo("mock");
    assertThat(configuration.getPropertyValue("unset.property", false)).isNull();
  }

  @Test
  public void iteratorIsCorrect() {

    Set<String> expectedPropertyNames = new HashSet<>(this.properties.stringPropertyNames());

    assertThat(expectedPropertyNames).isNotEmpty();

    StreamSupport.stream(this.configuration.spliterator(), false).forEach(actualPropertyName ->
      assertThat(expectedPropertyNames.remove(actualPropertyName)).isTrue());

    assertThat(expectedPropertyNames).isEmpty();
  }
}
