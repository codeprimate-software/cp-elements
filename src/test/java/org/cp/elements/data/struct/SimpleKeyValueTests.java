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
import static org.cp.elements.data.struct.SimpleKeyValue.newKeyValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.Optional;

import org.cp.elements.lang.Constants;
import org.junit.Test;

/**
 * Unit tests for {@link SimpleKeyValue}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see SimpleKeyValue
 * @since 1.0.0
 */
public class SimpleKeyValueTests {

  @Test
  public void newKeyValueWithKey() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void neKeyValueWithKeyAndValue() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("testValue");
  }

  @Test
  @SuppressWarnings("unchecked")
  public void fromMapEntry() {

    Map.Entry<Object, Object> mockMapEntry = mock(Map.Entry.class);

    when(mockMapEntry.getKey()).thenReturn("TestKey");
    when(mockMapEntry.getValue()).thenReturn("TestValue");

    SimpleKeyValue<Object, Object> keyValue = SimpleKeyValue.from(mockMapEntry);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("TestKey");
    assertThat(keyValue.getValue().orElse(null)).isEqualTo("TestValue");
  }

  @Test(expected = IllegalArgumentException.class)
  public void fromNullMapEntry() {

    try {
      SimpleKeyValue.from(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Map.Entry is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
  }

  @Test
  public void constructKeyValueIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = new SimpleKeyValue<>("testKey", "testValue");

    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue("default")).isEqualTo("testValue");
  }

  @Test
  public void constructKeyValueWithKeyAndNoValueIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = new SimpleKeyValue<>("testKey");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void constructKeyValueWithKeyAndNullValueIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = new SimpleKeyValue<>("testKey", null);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructKeyValueWithNullKeyThrowsIllegalArgumentException() {

    try {
      new SimpleKeyValue<>(null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("Key is required");
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
  public void asMapEntry() {

    SimpleKeyValue<Object, Object> keyValue = SimpleKeyValue.newKeyValue("TestKey", "TestValue");

    assertThat(keyValue).isNotNull();

    Map.Entry<Object, Object> mapEntry = keyValue.asMapEntry();

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("TestKey");
    assertThat(mapEntry.getValue()).isEqualTo("TestValue");
  }

  @Test(expected = UnsupportedOperationException.class)
  public void asImmutableMapEntry() {

    Map.Entry<Object, Object> mapEntry = SimpleKeyValue.<Object, Object>newKeyValue("TestKey", "TestValue").asMapEntry();

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("TestKey");
    assertThat(mapEntry.getValue()).isEqualTo("TestValue");

    try {
      mapEntry.setValue("MockValue");
    }
    catch (UnsupportedOperationException expected) {

      assertThat(expected).hasMessage(Constants.OPERATION_NOT_SUPPORTED);
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      assertThat(mapEntry.getKey()).isEqualTo("TestKey");
      assertThat(mapEntry.getValue()).isEqualTo("TestValue");
    }
  }

  @Test
  public void keyValueEqualsItself() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue).isEqualTo(keyValue);
  }

  @Test
  public void keyValueWithNullValueEqualsItself() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey", null);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue).isEqualTo(keyValue);
  }

  @Test
  public void keyValuesAreEqual() {

    SimpleKeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValue");
    SimpleKeyValue<Object, Object> keyValueTwo = newKeyValue("testKey", "testValue");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne).isEqualTo(keyValueTwo);
  }

  @Test
  public void keyValuesAreNotEqual() {

    SimpleKeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValueOne");
    SimpleKeyValue<Object, Object> keyValueTwo = newKeyValue("testKey", "testValueTwo");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne).isNotEqualTo(keyValueTwo);
  }

  @Test
  public void keyValueIsNotEqualToTestKeyValue() {

    SimpleKeyValue<Object, Object> keyValueOne = newKeyValue("testKey", "testValue");
    SimpleKeyValue<Object, Object> keyValueTwo = new TestKeyValue<>("testKey", "testValue");

    assertThat(keyValueOne).isNotNull();
    assertThat(keyValueTwo).isNotNull();
    assertThat(keyValueOne).isNotSameAs(keyValueTwo);
    assertThat(keyValueOne.getKey()).isEqualTo(keyValueTwo.getKey());
    assertThat(keyValueOne.getValue("default")).isEqualTo(keyValueTwo.getValue("default"));
    assertThat(keyValueOne).isNotEqualTo(keyValueTwo);
  }

  @Test
  public void hashCodeIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.hashCode()).isNotZero();
    assertThat(keyValue.hashCode()).isPositive();
  }

  @Test
  public void toStringIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = newKeyValue("testKey", "testValue");

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.toString()).isEqualTo("testKey = testValue");
  }

  @SuppressWarnings("unused")
  static final class TestKeyValue<K, V> extends SimpleKeyValue<K, V> {

    TestKeyValue(K key) {
      super(key);
    }

    TestKeyValue(K key, V value) {
      super(key, value);
    }
  }
}
