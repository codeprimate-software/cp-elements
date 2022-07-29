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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Map;
import java.util.Optional;

import org.junit.Test;

/**
 * Unit Tests for {@link KeyValue}.
 *
 * @author John Blum
 * @see java.util.Map
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.struct.KeyValue
 * @since 1.0.0
 */
public class KeyValueUnitTests {

  @Test
  public void isSetReturnsTrue() {

    KeyValue<?, ?> mockKeyValue = mock(KeyValue.class);

    doReturn(Optional.of("test")).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).isSet();

    assertThat(mockKeyValue.isSet()).isTrue();

    verify(mockKeyValue, times(1)).isSet();
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }

  @Test
  public void isSetReturnsFalse() {

    KeyValue<?, ?> mockKeyValue = mock(KeyValue.class);

    doReturn(Optional.empty()).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).isSet();

    assertThat(mockKeyValue.isSet()).isFalse();

    verify(mockKeyValue, times(1)).isSet();
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }

  @Test
  public void getValueDefaultsToOptionalEmpty() {

    KeyValue<?, ?> mockKeyValue = mock(KeyValue.class);

    doCallRealMethod().when(mockKeyValue).getValue();

    Optional<?> value = mockKeyValue.getValue();

    assertThat(value).isNotNull();
    assertThat(value).isNotPresent();
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getValueWithDefaultValueReturnsMappedValue() {

    KeyValue<Object, Object> mockKeyValue = mock(KeyValue.class);

    doReturn(Optional.of("mock")).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).getValue(any());

    assertThat(mockKeyValue.getValue("test")).isEqualTo("mock");

    verify(mockKeyValue, times(1)).getValue(eq("test"));
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void getValueWithDefaultValueReturnsDefaultValue() {

    KeyValue<Object, Object> mockKeyValue = mock(KeyValue.class);

    doReturn(Optional.empty()).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).getValue(any());

    assertThat(mockKeyValue.getValue("test")).isEqualTo("test");

    verify(mockKeyValue, times(1)).getValue(eq("test"));
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void asMapEntryWithKeyAndValue() {

    KeyValue<Object, Object> mockKeyValue = mock(KeyValue.class);

    doReturn("myKey").when(mockKeyValue).getKey();
    doReturn(Optional.of("mock")).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).getValue(any());
    doCallRealMethod().when(mockKeyValue).asMapEntry();

    Map.Entry<Object, Object> mapEntry = mockKeyValue.asMapEntry();

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("myKey");
    assertThat(mapEntry.getValue()).isEqualTo("mock");

    verify(mockKeyValue, times(1)).asMapEntry();
    verify(mockKeyValue, times(1)).getKey();
    verify(mockKeyValue, times(1)).getValue(isNull());
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }

  @Test
  @SuppressWarnings("unchecked")
  public void asMapEntryWithKeyOnly() {

    KeyValue<Object, Object> mockKeyValue = mock(KeyValue.class);

    doReturn("myKey").when(mockKeyValue).getKey();
    doReturn(Optional.empty()).when(mockKeyValue).getValue();
    doCallRealMethod().when(mockKeyValue).getValue(any());
    doCallRealMethod().when(mockKeyValue).asMapEntry();

    Map.Entry<Object, Object> mapEntry = mockKeyValue.asMapEntry();

    assertThat(mapEntry).isNotNull();
    assertThat(mapEntry.getKey()).isEqualTo("myKey");
    assertThat(mapEntry.getValue()).isNull();

    verify(mockKeyValue, times(1)).asMapEntry();
    verify(mockKeyValue, times(1)).getKey();
    verify(mockKeyValue, times(1)).getValue(isNull());
    verify(mockKeyValue, times(1)).getValue();
    verifyNoMoreInteractions(mockKeyValue);
  }
}
