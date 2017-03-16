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

package org.cp.elements.data.struct;

import static org.assertj.core.api.Assertions.assertThat;
import static org.cp.elements.data.struct.KeyValue.newKeyValue;

import java.util.Optional;

import org.junit.Test;

/**
 * Unit tests for {@link KeyValue}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.data.struct.KeyValue
 * @since 1.0.0
 */
public class KeyValueTests {

  @Test
  public void newKeyValueWithKey() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void neKeyValueWithKeyAndValue() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("testValue");
  }

  @Test
  public void constructKeyValueIsSuccessful() {
    KeyValue<Object, Object> keyValue = new KeyValue<>("testKey", "testValue");

    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue("default")).isEqualTo("testValue");
  }

  @Test
  public void constructKeyValueWithKeyAndNoValueIsSuccessful() {
    KeyValue<Object, Object> keyValue = new KeyValue<>("testKey");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void constructKeyValueWithKeyAndNullValueIsSuccessful() {
    KeyValue<Object, Object> keyValue = new KeyValue<>("testKey", null);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructKeyValueWithNullKeyThrowsIllegalArgumentException() {
    try {
      new KeyValue<>(null);
    }
    catch (IllegalArgumentException expected) {
      assertThat(expected).hasMessage("Key must not be null");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void isSetReturnsTrue() {
    assertThat(newKeyValue("testKey", "testValue").isSet()).isTrue();
  }

  @Test
  public void isSetReturnsFalse() {
    assertThat(newKeyValue("testKey").isSet()).isFalse();
    assertThat(newKeyValue("testKey", null).isSet()).isFalse();
  }

  @Test
  public void getValueWithDefaultWhenNullUsesDefault() {
    assertThat(newKeyValue("testKey").getValue("default")).isEqualTo("default");
    assertThat(newKeyValue("testKey", null).getValue("default")).isEqualTo("default");
  }

  @Test
  public void getValueWithDefaultWhenNotNullUsesValue() {
    assertThat(newKeyValue("testKey", "testValue").getValue("default")).isEqualTo("testValue");
  }

  @Test
  public void keyValueEqualsItself() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue).isEqualTo(keyValue);
  }

  @Test
  public void keyValueWithNullValueEqualsItself() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey", null);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue).isEqualTo(keyValue);
  }

  @Test
  public void keyValuesAreEqual() {
    KeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValue");
    KeyValue<Object, Object> keyValueTwo = newKeyValue("testKey", "testValue");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne).isEqualTo(keyValueTwo);
  }

  @Test
  public void keyValuesAreNotEqual() {
    KeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValueOne");
    KeyValue<Object, Object> keyValueTwo = newKeyValue("testKey", "testValueTwo");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne).isNotEqualTo(keyValueTwo);
  }

  @Test
  public void keyValueIsNotEqualToTestKeyValue() {
    KeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValue");
    KeyValue<Object, Object> keyValueTwo = new TestKeyValue<>("testKey", "testValue");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne.getKey()).isEqualTo(keyValueTwo.getKey());
    assertThat(keyValueOne.getValue("default")).isEqualTo(keyValueTwo.getValue("default"));
    assertThat(keyValueOne).isNotEqualTo(keyValueTwo);
  }

  @Test
  public void hashCodeIsSuccessful() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.hashCode()).isNotZero();
    assertThat(keyValue.hashCode()).isPositive();
  }

  @Test
  public void toStringIsSuccessful() {
    KeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.toString()).isEqualTo("testKey = testValue");
  }

  @SuppressWarnings("unused")
  static final class TestKeyValue<K, V> extends KeyValue<K, V> {

    TestKeyValue(K key) {
      super(key);
    }

    TestKeyValue(K key, V value) {
      super(key, value);
    }
  }
}
