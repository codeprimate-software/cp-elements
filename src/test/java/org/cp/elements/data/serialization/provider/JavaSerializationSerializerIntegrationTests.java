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
package org.cp.elements.data.serialization.provider;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;

import java.io.EOFException;
import java.io.NotSerializableException;
import java.io.Serializable;
import java.nio.ByteBuffer;

import org.junit.jupiter.api.Test;

import org.cp.elements.data.serialization.DeserializationException;
import org.cp.elements.data.serialization.SerializationException;
import org.cp.elements.lang.Identifiable;
import org.cp.elements.lang.ThrowableAssertions;
import org.cp.elements.security.model.User;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Integration Tests for {@link JavaSerializationSerializer}.
 *
 * @author John Blum
 * @see java.io.Serializable
 * @see java.nio.ByteBuffer
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.data.serialization.provider.JavaSerializationSerializer
 * @since 1.0.0
 */
public class JavaSerializationSerializerIntegrationTests {

  @Test
  public void serializesAndDeserializesObject() {

    TestUser jonDoe = TestUser.as("jonDoe").identifiedBy(2);

    ByteBuffer jonDoeBytes = JavaSerializationSerializer.INSTANCE.serialize(jonDoe);

    assertThat(jonDoeBytes).isNotNull();
    assertThat(jonDoeBytes.limit()).isGreaterThan(0);

    TestUser deserializedJonDoe = JavaSerializationSerializer.INSTANCE.deserialize(jonDoeBytes);

    assertThat(deserializedJonDoe).isNotNull();
    assertThat(deserializedJonDoe).isEqualTo(jonDoe);
    assertThat(deserializedJonDoe).isNotSameAs(jonDoe);
    assertThat(deserializedJonDoe.getId()).isEqualTo(2);
  }

  @Test
  public void serializeNonSerializableObject() {

    NonSerializableObject target = new NonSerializableObject();

    ThrowableAssertions.assertThatThrowableOfType(SerializationException.class)
      .isThrownBy(args -> JavaSerializationSerializer.INSTANCE.serialize(target))
      .havingMessage("Failed to serialize object [%s]", target)
      .causedBy(NotSerializableException.class)
      .havingMessage("%s", target.getClass().getName())
      .withNoCause();
  }

  @Test
  public void serializeNullObjectThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> JavaSerializationSerializer.INSTANCE.serialize(null))
      .withMessage("The object to serialize is required")
      .withNoCause();
  }

  @Test
  public void deserializeNullByteBufferThrowsIllegalArgumentException() {

    assertThatIllegalArgumentException()
      .isThrownBy(() -> JavaSerializationSerializer.INSTANCE.deserialize(null))
      .withMessage("The ByteBuffer containing the bytes of the object to deserialize is required")
      .withNoCause();
  }

  @Test
  public void deserializeEmptyByteBuffer() {

    ByteBuffer bytes = ByteBuffer.allocate(0);

    ThrowableAssertions.assertThatThrowableOfType(DeserializationException.class)
      .isThrownBy(args -> JavaSerializationSerializer.INSTANCE.deserialize(bytes))
      .havingMessage("Failed to deserialize object from bytes")
      .causedBy(EOFException.class)
      .withNoCause();
  }

  static class NonSerializableObject { }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser implements User<Integer>, Serializable {

    @Setter
    private Integer id;

    private final String name;

    @Override
    @SuppressWarnings("unchecked")
    public <IDX extends Identifiable<Integer>> IDX identifiedBy(Integer id) {
      setId(id);
      return (IDX) this;
    }
  }
}
