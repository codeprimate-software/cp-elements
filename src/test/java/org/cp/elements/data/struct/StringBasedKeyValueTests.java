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

package org.cp.elements.data.struct;

import static org.assertj.core.api.Assertions.assertThat;

import org.junit.Test;

/**
 * Unit tests for {@link StringBasedKeyValue}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.StringBasedKeyValue
 * @since 1.0.0
 */
public class StringBasedKeyValueTests {

  @Test
  public void getDefaultValueAsBoolean() {

    StringBasedKeyValue blankKeyValue = new TestStringBasedKeyValue("key", "  ");
    StringBasedKeyValue emptyKeyValue = new TestStringBasedKeyValue("key", "");
    StringBasedKeyValue nullKeyValue = new TestStringBasedKeyValue("key");

    assertThat(blankKeyValue.getValueAs(Boolean.class, true)).isTrue();
    assertThat(emptyKeyValue.getValueAs(Boolean.class, false)).isFalse();
    assertThat(nullKeyValue.getValueAs(Boolean.class, true)).isTrue();
  }

  @Test
  public void getDefaultValueAsCharacter() {

    StringBasedKeyValue blankKeyValue = new TestStringBasedKeyValue("key", "  ");
    StringBasedKeyValue emptyKeyValue = new TestStringBasedKeyValue("key", "");
    StringBasedKeyValue nullKeyValue = new TestStringBasedKeyValue("key");

    assertThat(blankKeyValue.getValueAs(Character.class, 'A')).isEqualTo('A');
    assertThat(emptyKeyValue.getValueAs(Character.class, 'B')).isEqualTo('B');
    assertThat(nullKeyValue.getValueAs(Character.class, 'C')).isEqualTo('C');
  }

  @Test
  public void getDefaultValueAsDouble() {

    StringBasedKeyValue blankKeyValue = new TestStringBasedKeyValue("key", "  ");
    StringBasedKeyValue emptyKeyValue = new TestStringBasedKeyValue("key", "");
    StringBasedKeyValue nullKeyValue = new TestStringBasedKeyValue("key");

    assertThat(blankKeyValue.getValueAs(Double.class, 2.0d)).isEqualTo(2.0d);
    assertThat(emptyKeyValue.getValueAs(Double.class, 3.14159d)).isEqualTo(3.14159d);
    assertThat(nullKeyValue.getValueAs(Double.class, Math.PI)).isEqualTo(Math.PI);
  }

  @Test
  public void getDefaultValueAsInteger() {

    StringBasedKeyValue blankKeyValue = new TestStringBasedKeyValue("key", "  ");
    StringBasedKeyValue emptyKeyValue = new TestStringBasedKeyValue("key", "");
    StringBasedKeyValue nullKeyValue = new TestStringBasedKeyValue("key");

    assertThat(blankKeyValue.getValueAs(Integer.class, 0)).isEqualTo(0);
    assertThat(emptyKeyValue.getValueAs(Integer.class, 1)).isEqualTo(1);
    assertThat(nullKeyValue.getValueAs(Integer.class, 2)).isEqualTo(2);
  }

  @Test
  public void getDefaultValueAsString() {

    StringBasedKeyValue blankKeyValue = new TestStringBasedKeyValue("key", "  ");
    StringBasedKeyValue emptyKeyValue = new TestStringBasedKeyValue("key", "");
    StringBasedKeyValue nullKeyValue = new TestStringBasedKeyValue("key");

    assertThat(blankKeyValue.getValueAs(String.class, "test")).isEqualTo("test");
    assertThat(emptyKeyValue.getValueAs(String.class, "test")).isEqualTo("test");
    assertThat(nullKeyValue.getValueAs(String.class, "test")).isEqualTo("test");
  }

  @Test
  public void getNullValueAsString() {

    StringBasedKeyValue keyValue = new TestStringBasedKeyValue("key");

    assertThat(keyValue.getValue().orElse(null)).isNull();
    assertThat(keyValue.getValueAs(String.class).orElse(null)).isNull();
  }

  @Test
  public void getValueAsBoolean() {

    StringBasedKeyValue falseKeyValue = new TestStringBasedKeyValue("key", "false");
    StringBasedKeyValue trueKeyValue = new TestStringBasedKeyValue("key", "true");

    assertThat(trueKeyValue.getValue().orElse(null)).isEqualTo("true");
    assertThat(falseKeyValue.getValue().orElse(null)).isEqualTo("false");
    assertThat(trueKeyValue.getValueAs(Boolean.class).orElse(false)).isTrue();
    assertThat(falseKeyValue.getValueAs(Boolean.class).orElse(true)).isFalse();
  }

  @Test
  public void getValueAsCharacter() {
    assertThat(new TestStringBasedKeyValue("key", "X").getValue().orElse(null)).isEqualTo("X");
    assertThat(new TestStringBasedKeyValue("key", "X").getValueAs(Character.class).orElse(null)).isEqualTo('X');
  }

  @Test
  public void getValueAsDouble() {

    StringBasedKeyValue keyValue = new TestStringBasedKeyValue("key", "3.14159");

    assertThat(keyValue.getValue().orElse(null)).isEqualTo("3.14159");
    assertThat(keyValue.getValueAs(Double.class).orElse(null)).isEqualTo(3.14159d);
  }

  @Test
  public void getValueAsInteger() {

    StringBasedKeyValue keyValue = new TestStringBasedKeyValue("key", "2");

    assertThat(keyValue.getValue().orElse(null)).isEqualTo("2");
    assertThat(keyValue.getValueAs(Integer.class).orElse(null)).isEqualTo(2);
  }

  @Test
  public void getValueAsString() {

    StringBasedKeyValue keyValue = new TestStringBasedKeyValue("key", "string");

    assertThat(keyValue.getValue().orElse(null)).isEqualTo("string");
    assertThat(keyValue.getValueAs(String.class).orElse(null)).isEqualTo("string");
  }

  static class TestStringBasedKeyValue extends StringBasedKeyValue {

    TestStringBasedKeyValue(String key) {
      super(key);
    }

    TestStringBasedKeyValue(String key, String value) {
      super(key, value);
    }
  }
}
