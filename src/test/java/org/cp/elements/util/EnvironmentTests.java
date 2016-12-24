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
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.nullValue;

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

  /*
  @Test
  public void copyToMap() {
  }

  @Test
  public void copyToProperties() {
  }

  @Test
  public void iteration() {
  }
  */
}
