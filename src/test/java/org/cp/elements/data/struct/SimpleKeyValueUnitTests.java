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
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.cp.elements.data.struct.SimpleKeyValue.newKeyValue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Map;
import java.util.Optional;

import org.cp.elements.lang.Constants;
import org.junit.Test;

/**
 * Unit Tests for {@link SimpleKeyValue}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see java.util.Optional
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.SimpleKeyValue
 * @since 1.0.0
 */
public class SimpleKeyValueUnitTests {

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

    when(mockMapEntry.getKey()).thenReturn("mockKey");
    when(mockMapEntry.getValue()).thenReturn("mockValue");

    SimpleKeyValue<Object, Object> keyValue = SimpleKeyValue.from(mockMapEntry);

    assertThat(keyValue).isNotNull();
    assertThat(keyValue.getKey()).isEqualTo("mockKey");
    assertThat(keyValue.getValue().orElse(null)).isEqualTo("mockValue");
  }

  @Test
  public void fromNullMapEntry() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleKeyValue.from(null))
      .withMessage("Map.Entry is required")
      .withNoCause();
  }

  @Test
  public void newKeyValueWithNullKeyAndNoValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleKeyValue.newKeyValue(null))
      .withMessage("Key is required")
      .withNoCause();
  }

  @Test
  public void newKeyValueWithNullKeyAndNonNullValue() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> SimpleKeyValue.newKeyValue(null, "mock"))
      .withMessage("Key is required")
      .withNoCause();
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

    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void constructKeyValueWithKeyAndNullValueIsSuccessful() {

    SimpleKeyValue<Object, Object> keyValue = new SimpleKeyValue<>("testKey", null);

    assertThat(keyValue.getKey()).isEqualTo("testKey");
    assertThat(keyValue.getValue()).isInstanceOf(Optional.class);
    assertThat(keyValue.getValue("default")).isEqualTo("default");
  }

  @Test
  public void constructKeyValueWithNullKeyThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> new SimpleKeyValue<>(null))
      .withMessage("Key is required")
      .withNoCause();
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
