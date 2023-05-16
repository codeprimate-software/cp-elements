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
package org.cp.elements.data.serialization;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.Serializable;
import java.nio.ByteBuffer;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Serializer}.
 *
 * @author John Blum
 * @see java.io.Serializable
 * @see java.nio.ByteBuffer
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.data.serialization.Serializer
 * @since 1.0.0
 */
public class SerializerUnitTests {

  @Test
  public void isSerializableWithNonSerializableObject() {

    Object target = spy(new Object());

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).isSerializable(any());

    assertThat(serializer.isSerializable(target)).isFalse();

    verify(serializer, times(1)).isSerializable(eq(target));
    verifyNoMoreInteractions(serializer);
    verifyNoInteractions(target);
  }

  @Test
  public void isSerializableWithNullIsNullSafe() {

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).isSerializable(any());

    assertThat(serializer.isSerializable(null)).isFalse();

    verify(serializer, times(1)).isSerializable(isNull());
    verifyNoMoreInteractions(serializer);
  }

  @Test
  public void isSerializableWithSerializableObject() {

    Serializable mockSerializable = mock(Serializable.class);

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).isSerializable(any());

    assertThat(serializer.isSerializable(mockSerializable)).isTrue();

    verify(serializer, times(1)).isSerializable(eq(mockSerializable));
    verifyNoMoreInteractions(serializer);
    verifyNoInteractions(mockSerializable);
  }

  @Test
  public void toByteArrayWithSerializableObjectIsSuccessful() {

    byte[] testBytes = "TEST".getBytes();

    ByteBuffer buffer = ByteBuffer.wrap(testBytes);

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).toByteArray(any());
    doReturn(buffer).when(serializer).serialize(eq("TEST"));

    byte[] serializedBytes = serializer.toByteArray("TEST");

    assertThat(serializedBytes).isNotNull();
    assertThat(serializedBytes).isEqualTo(testBytes);
    assertThat(new String(testBytes)).isEqualTo("TEST");

    verify(serializer, times(1)).toByteArray(eq("TEST"));
    verify(serializer, times(1)).serialize(eq("TEST"));
    verifyNoMoreInteractions(serializer);
  }

  @Test
  public void toByteArrayWithSerializableObjectReturningReadOnlyByteBufferIsSuccessful() {

    byte[] testBytes = "MOCK".getBytes();

    ByteBuffer buffer = ByteBuffer.wrap(testBytes).asReadOnlyBuffer();

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).toByteArray(any());
    doReturn(buffer).when(serializer).serialize(eq("MOCK"));

    byte[] serializedBytes = serializer.toByteArray("MOCK");

    assertThat(serializedBytes).isNotNull();
    assertThat(serializedBytes).isEqualTo(testBytes);
    assertThat(new String(testBytes)).isEqualTo("MOCK");

    verify(serializer, times(1)).toByteArray(eq("MOCK"));
    verify(serializer, times(1)).serialize(eq("MOCK"));
    verifyNoMoreInteractions(serializer);
  }

  @Test
  public void toByteArrayWithNullTargetObjectThrowsIllegalArgumentException() {

    Serializer serializer = mock(Serializer.class);

    doCallRealMethod().when(serializer).toByteArray(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> serializer.toByteArray(null))
      .withMessage("Object to serialize is required")
      .withNoCause();

    verify(serializer, times(1)).toByteArray(isNull());
    verifyNoMoreInteractions(serializer);
  }
}
