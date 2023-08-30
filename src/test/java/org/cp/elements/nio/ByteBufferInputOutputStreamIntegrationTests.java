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
package org.cp.elements.nio;

import static org.assertj.core.api.Assertions.assertThat;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.nio.ByteBuffer;
import java.util.UUID;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Identifiable;
import org.cp.elements.security.model.User;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Setter;
import lombok.ToString;

/**
 * Integration Tests combining the {@link ByteBufferInputStream} and {@link ByteBufferOutputStream}
 *
 * @author John Blum
 * @see java.io.InputStream
 * @see java.io.OutputStream
 * @see java.nio.ByteBuffer
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.nio.ByteBufferInputStream
 * @see org.cp.elements.nio.ByteBufferOutputStream
 * @since 1.0.0
 */
public class ByteBufferInputOutputStreamIntegrationTests {

  @Test
  public void fromMixedPrimitiveDataOutputToInputWorksCorrectly() throws IOException {

    ByteBufferOutputStream byteBufferOutputStream = ByteBufferOutputStream.into(ByteBuffer.allocate(256));

    DataOutputStream outputStream = new DataOutputStream(byteBufferOutputStream);

    outputStream.writeBoolean(true);
    outputStream.writeChar('x');
    outputStream.writeInt(42);
    outputStream.writeDouble(3.14159d);
    outputStream.writeUTF("test");
    outputStream.flush();
    outputStream.close();

    ByteBufferInputStream byteBufferInputStream = ByteBufferInputStream.from(byteBufferOutputStream.getByteBuffer());

    DataInputStream inputStream = new DataInputStream(byteBufferInputStream);

    assertThat(inputStream.readBoolean()).isTrue();
    assertThat(inputStream.readChar()).isEqualTo('x');
    assertThat(inputStream.readInt()).isEqualTo(42);
    assertThat(inputStream.readDouble()).isEqualTo(3.14159d);
    assertThat(inputStream.readUTF()).isEqualTo("test");

    inputStream.close();
  }

  @Test
  public void fromMixedPrimitiveDataAndObjectOutputToInputWorksCorrectly() throws Exception {

    TestUser expectedUser = TestUser.as("John Wick").identifiedBy(UUID.randomUUID());

    ByteBufferOutputStream byteBufferOutputStream = ByteBufferOutputStream.into(ByteBuffer.allocate(512));

    DataOutputStream dataOutputStream = new DataOutputStream(byteBufferOutputStream);

    ObjectOutputStream objectOutputStream = new ObjectOutputStream(byteBufferOutputStream);

    dataOutputStream.writeInt(69);
    dataOutputStream.flush();
    objectOutputStream.writeObject(expectedUser);
    objectOutputStream.flush();
    dataOutputStream.writeUTF("mock");
    dataOutputStream.flush();
    objectOutputStream.close();
    dataOutputStream.close();

    ByteBufferInputStream byteBufferInputStream = ByteBufferInputStream.from(byteBufferOutputStream.getByteBuffer());

    DataInputStream dataInputStream = new DataInputStream(byteBufferInputStream);

    ObjectInputStream objectInputStream = new ObjectInputStream(byteBufferInputStream);

    assertThat(dataInputStream.readInt()).isEqualTo(69);

    TestUser actualUser = (TestUser) objectInputStream.readObject();

    assertThat(actualUser).isNotNull();
    assertThat(actualUser).isNotSameAs(expectedUser);
    assertThat(actualUser).isEqualTo(expectedUser);
    assertThat(actualUser.getId()).isEqualTo(expectedUser.getId());
    assertThat(actualUser.getName()).isEqualTo(expectedUser.getName());
    assertThat(dataInputStream.readUTF()).isEqualTo("mock");

    objectInputStream.close();
    dataInputStream.close();
  }

  @Test
  public void fromObjectOutputToObjectInputWorksCorrectly() throws IOException, ClassNotFoundException {

    TestUser expectedUser = TestUser.as("Johnathan Doeboy").identifiedBy(UUID.randomUUID());

    ByteBufferOutputStream byteBufferOutputStream = ByteBufferOutputStream.into(ByteBuffer.allocate(1024));

    ObjectOutputStream outputStream = new ObjectOutputStream(byteBufferOutputStream);

    outputStream.writeObject(expectedUser);
    outputStream.flush();
    outputStream.close();

    ByteBufferInputStream byteBufferInputStream = ByteBufferInputStream.from(byteBufferOutputStream.getByteBuffer());

    ObjectInputStream inputStream = new ObjectInputStream(byteBufferInputStream);

    TestUser actualUser = (TestUser) inputStream.readObject();

    inputStream.close();

    assertThat(actualUser).isNotNull();
    assertThat(actualUser).isNotSameAs(expectedUser);
    assertThat(actualUser).isEqualTo(expectedUser);
    assertThat(actualUser.getId()).isEqualTo(expectedUser.getId());
    assertThat(actualUser.getName()).isEqualTo(expectedUser.getName());
  }

  @Test
  public void fromStringOutputToStringInputWorksCorrectly() throws IOException {

    String data = "TEST";

    byte[] dataBytes = data.getBytes();

    ByteBufferOutputStream byteBufferOutputStream = ByteBufferOutputStream.into(ByteBuffer.allocate(1024));

    OutputStream outputStream = new DataOutputStream(byteBufferOutputStream);

    outputStream.write(dataBytes);
    outputStream.flush();
    outputStream.close();

    ByteBufferInputStream byteBufferInputStream = ByteBufferInputStream.from(byteBufferOutputStream.getByteBuffer());

    InputStream inputStream = new DataInputStream(byteBufferInputStream);

    byte[] buffer = new byte[dataBytes.length];

    assertThat(inputStream.read(buffer)).isGreaterThan(0);

    inputStream.close();

    assertThat(new String(buffer)).isEqualTo("TEST");
  }

  @Getter
  @EqualsAndHashCode(of = "name")
  @ToString(of = "name")
  @RequiredArgsConstructor(staticName = "as")
  static class TestUser implements User<UUID>, Serializable {

    @lombok.NonNull
    private final String name;

    @Setter
    private UUID id;

    @Override
    @SuppressWarnings("unchecked")
    public <IDX extends Identifiable<UUID>> IDX identifiedBy(UUID id) {
      setId(id);
      return (IDX) this;
    }
  }
}
