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
import static org.cp.elements.util.ArrayUtils.asIterable;
import static org.cp.elements.util.PropertiesUtils.singletonProperties;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * Unit tests for {@link Environment}.
 *
 * @author John Blum
 * @see org.junit.Rule
 * @see org.junit.Test
 * @see org.cp.elements.util.Environment
 * @since 1.0.0
 */
public class EnvironmentTests {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  @Test
  public void fromMap() {
    Map<String, String> map = new HashMap<>();

    map.put("one", "1");
    map.put("two", "2");

    Environment environment = Environment.from(map);

    assertThat(environment).isNotNull();
    assertThat(environment.isEmpty()).isFalse();
    assertThat(environment.size()).isEqualTo(map.size());

    for (String key : map.keySet()) {
      assertThat(environment.get(key)).isEqualTo(map.get(key));
    }
  }

  @Test
  public void fromProperties() {
    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "2");

    Environment environment = Environment.from(properties);

    assertThat(environment).isNotNull();
    assertThat(environment.isEmpty()).isFalse();
    assertThat(environment.size()).isEqualTo(properties.size());

    for (String key : properties.stringPropertyNames()) {
      assertThat(environment.get(key)).isEqualTo(properties.get(key));
    }
  }

  @Test
  public void fromEnvironmentVariables() {
    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.isEmpty()).isFalse();
    assertThat(environment.get("HOME")).isEqualTo(System.getProperty("user.home"));
    assertThat(environment.get("PATH")).isEqualTo(System.getenv().get("PATH"));
    assertThat(environment.get("USER")).isEqualTo(System.getProperty("user.name"));
  }

  @Test
  public void constructsEnvironment() {
    Environment environment = new Environment(PropertiesAdapter.from(new Properties()));

    assertThat(environment).isNotNull();
    assertThat(environment.isEmpty()).isTrue();
    assertThat(environment.environment()).isInstanceOf(PropertiesAdapter.class);
    assertThat(environment.systemProperties()).isInstanceOf(PropertiesAdapter.class);
  }

  @Test
  public void constructsEnvironmentWithNull() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage("The initial environment cannot be null");

    new Environment(null);
  }

  @Test
  public void isEmptyIsTrue() {
    assertThat(Environment.from(Collections.emptyMap()).isEmpty()).isTrue();
  }

  @Test
  public void isEmptyIsFalse() {
    assertThat(Environment.from(singletonProperties("one", "1")).isEmpty()).isFalse();
  }

  @Test
  public void isSetGetAndGetAsIsCorrect() {
    Map<String, String> map = new HashMap<>();

    map.put("booleanProperty", "true");
    map.put("characterProperty", "X");
    map.put("doubleProperty", "3.14159");
    map.put("integerProperty", "2");
    map.put("stringProperty", "string");
    map.put("unsetProperty", null);

    Environment environment = Environment.from(map);

    assertThat(environment).isNotNull();
    assertThat(environment.size()).isEqualTo(map.size());
    assertThat(environment.isSet("booleanProperty")).isTrue();
    assertThat(environment.get("booleanProperty")).isEqualTo("true");
    assertThat(environment.getAs("booleanProperty", Boolean.class)).isTrue();
    assertThat(environment.isSet("characterProperty")).isTrue();
    assertThat(environment.get("characterProperty")).isEqualTo("X");
    assertThat(environment.getAs("characterProperty", Character.class)).isEqualTo('X');
    assertThat(environment.isSet("doubleProperty")).isTrue();
    assertThat(environment.get("doubleProperty")).isEqualTo("3.14159");
    assertThat(environment.getAs("doubleProperty", Double.class)).isEqualTo(3.14159d);
    assertThat(environment.isSet("integerProperty")).isTrue();
    assertThat(environment.get("integerProperty")).isEqualTo("2");
    assertThat(environment.getAs("integerProperty", Integer.class)).isEqualTo(2);
    assertThat(environment.isSet("stringProperty")).isTrue();
    assertThat(environment.get("stringProperty")).isEqualTo("string");
    assertThat(environment.getAs("stringProperty", String.class)).isEqualTo("string");
    assertThat(environment.isSet("unsetProperty")).isFalse();
    assertThat(environment.get("unsetProperty")).isNull();
    assertThat(environment.getAs("unsetProperty", String.class)).isNull();
  }

  @Test
  public void copyToMap() {
    Map<String, String> env = new HashMap<>();

    env.put("one", "1");
    env.put("two", "2");
    env.put("three", "3");

    Map<String, String> map = new HashMap<>();

    map.put("one", "1");
    map.put("two", "4");

    Environment environment = Environment.from(env);

    assertThat(environment).isNotNull();
    assertThat(environment.size()).isEqualTo(env.size());

    Map<String, String> mapCopy = environment.copyTo(map);

    assertThat(mapCopy).isSameAs(map);
    assertThat(mapCopy).isNotSameAs(env);
    assertThat(mapCopy).isEqualTo(env);
  }

  @Test
  public void copyToProperties() {
    Properties env = new Properties();

    env.setProperty("one", "1");
    env.setProperty("two", "2");
    env.setProperty("three", "3");

    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "4");

    Environment environment = Environment.from(env);

    assertThat(environment).isNotNull();
    assertThat(environment.size()).isEqualTo(env.size());

    Properties propertiesCopy = environment.copyTo(properties);

    assertThat(propertiesCopy).isSameAs(properties);
    assertThat(propertiesCopy).isNotSameAs(env);
    assertThat(propertiesCopy).isEqualTo(env);
  }

  @Test
  public void iterationIsSuccessful() {
    Properties properties = new Properties();

    properties.setProperty("one", "1");
    properties.setProperty("two", "2");
    properties.setProperty("three", "3");

    Environment environment = Environment.from(properties);

    assertThat(environment).isNotNull();
    assertThat(environment.size()).isEqualTo(properties.size());

    assertThat(CollectionUtils.toSet(environment)).containsAll(asIterable("one", "two", "three"));
  }

  @Test
  public void sizeIsZero() {
    assertThat(Environment.from(Collections.emptyMap()).size()).isEqualTo(0);
  }

  @Test
  public void sizeIsOne() {
    assertThat(Environment.from(singletonProperties("one", "1")).size()).isEqualTo(1);
  }

  @Test
  public void equalEnvironmentsIsTrue() {
    Environment environmentOne = Environment.from(Collections.singletonMap("one", "1"));
    Environment environmentTwo = Environment.from(singletonProperties("one", "1"));

    assertThat(environmentOne).isNotNull();
    assertThat(environmentTwo).isNotNull();
    assertThat(environmentOne.equals(environmentTwo)).isTrue();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsItselfIsTrue() {
    Environment environment = Environment.fromEnvironmentVariables();

    assertThat(environment).isNotNull();
    assertThat(environment.equals(environment)).isTrue();
  }

  @Test
  public void equalsObjectIsFalse() {
    assertThat(Environment.fromEnvironmentVariables().equals(new Object())).isFalse();
  }

  @Test
  @SuppressWarnings("all")
  public void equalsNullIsFalse() {
    assertThat(Environment.fromEnvironmentVariables().equals(null)).isFalse();
  }

  @Test
  public void hashCodeIsCorrect() {
    Environment environmentMap = Environment.from(Collections.singletonMap("key", "test"));
    Environment environmentProperties = Environment.from(singletonProperties("key", "test"));
    Environment environmentVariables = Environment.fromEnvironmentVariables();

    assertThat(environmentMap.hashCode()).isNotEqualTo(0);
    assertThat(environmentMap.hashCode()).isEqualTo(environmentProperties.hashCode());
    assertThat(environmentMap.hashCode()).isNotEqualTo(environmentVariables.hashCode());
  }
}
