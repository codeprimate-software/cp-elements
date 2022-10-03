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

import static org.cp.elements.lang.ElementsExceptionsFactory.newDeserializationException;
import static org.cp.elements.lang.ElementsExceptionsFactory.newSerializationException;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.ByteBuffer;

import org.cp.elements.data.serialization.Serializer;
import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.nio.ByteBufferInputStream;
import org.cp.elements.nio.ByteBufferOutputStream;

/**
 * {@link Serializer} implementation that used Java Serialization.
 *
 * @author John Blum
 * @see java.io.ObjectInputStream
 * @see java.io.ObjectOutputStream
 * @see org.cp.elements.data.serialization.Serializer
 * @see org.cp.elements.nio.ByteBufferInputStream
 * @see org.cp.elements.nio.ByteBufferOutputStream
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class JavaSerializationSerializer implements Serializer {

  private static final int DEFAULT_BUFFER_SIZE = 4096;

  public static final JavaSerializationSerializer INSTANCE = new JavaSerializationSerializer();

  @Override
  public @NotNull ByteBuffer serialize(@NotNull Object target) {

    Assert.notNull(target, "The object to serialize is required");

    ByteBufferOutputStream byteBufferOutput =
      new ByteBufferOutputStream(ByteBuffer.allocate(DEFAULT_BUFFER_SIZE));

    try (ObjectOutputStream objectOutput = new ObjectOutputStream(byteBufferOutput)) {
      objectOutput.writeObject(target);
      objectOutput.flush();
      return byteBufferOutput.getByteBuffer();
    }
    catch (IOException cause) {
      throw newSerializationException(cause, String.format("Failed to serialize object [%s]", target));
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public @NotNull <T> T deserialize(@NotNull ByteBuffer bytes) {

    Assert.notNull(bytes, "The ByteBuffer containing the bytes of the object to deserialize is required");

    try (ObjectInputStream objectInput = new ObjectInputStream(new ByteBufferInputStream(bytes))) {
      return (T) objectInput.readObject();
    }
    catch (IOException | ClassNotFoundException cause) {
      throw newDeserializationException(cause, "Failed to deserialize object from bytes");
    }
  }
}
