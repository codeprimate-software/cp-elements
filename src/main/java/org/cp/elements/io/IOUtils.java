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
package org.cp.elements.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.Closeable;
import java.io.EOFException;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.ObjectStreamClass;
import java.io.OutputStream;

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * Abstract utility class used to execute basic input and output (I/O) operations.
 *
 * @author John J. Blum
 * @see java.io.ByteArrayInputStream
 * @see java.io.ByteArrayOutputStream
 * @see java.io.Closeable
 * @see java.io.InputStream
 * @see java.io.ObjectInputStream
 * @see java.io.ObjectOutputStream
 * @see java.io.ObjectStreamClass
 * @see java.io.OutputStream
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class IOUtils {

  protected static final int DEFAULT_BUFFER_SIZE = 32768;

  /**
   * Attempts to close the {@link Closeable object}, ignoring any {@link IOException} that may by thrown as a result
   * of the {@link Closeable#close()} operation.
   *
   * @param target {@link Closeable object} to close.
   * @return a boolean value indicating if the {@link Closeable#close()} operation was successful.
   * @see java.io.Closeable
   */
  @NullSafe
  public static boolean close(@Nullable Closeable target) {

    if (target != null) {
      try {
        target.close();
        return true;
      }
      catch (IOException ignore) { }
    }

    return false;
  }

  /**
   * Copies the contents of the source {@link InputStream} to the target {@link OutputStream}.
   *
   * @param in {@link InputStream} used as the source to copy bytes from; must not be {@literal null}.
   * @param out {@link OutputStream} used as the target to copy bytes to; must not be {@literal null}.
   * @throws IOException if this copy operation results in an I/O error.
   * @see java.io.InputStream
   * @see java.io.OutputStream
   */
  public static void copy(@NotNull InputStream in, @NotNull OutputStream out) throws IOException {

    Assert.notNull(in, "InputStream is required");
    Assert.notNull(out, "OutputStream is required");

    byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];

    for (int length = in.read(buffer); length > 0; length = in.read(buffer)) {
      out.write(buffer, 0, length);
      out.flush();
    }
  }

  /**
   * Performs the given, required Input/Output (I/O) operation in a safe manner, handling any {@link IOException}
   * that may be thrown while performing the I/O.
   *
   * @param operation {@link IoExceptionThrowingOperation} to perform; must not be {@literal null}.
   * @return a boolean value indicating whether the I/O operation was successful.
   * @throws IllegalArgumentException if the given I/O operation is {@literal null}.
   * @see org.cp.elements.io.IOUtils.IoExceptionThrowingOperation
   */
  public static boolean doSafeIo(@NotNull IoExceptionThrowingOperation operation) {

    Assert.notNull(operation, "I/O operation is required");

    try {
      operation.doIo();
      return true;
    }
    catch (IOException ignore) {
      return false;
    }
  }

  /**
   * Deserializes the given, required byte array into an {@link Object} of the declared {@link Class type}.
   *
   * @param <T> {@link Class type} of {@link Object} deserialized from the given bytes.
   * @param serializedObjectBytes byte array containing the bytes of the object to deserialize;
   * must not be {@literal null}.
   * @return an {@link Object} deserialized from the given array of bytes.
   * @throws ClassNotFoundException if the {@link Class type} of the serialized {@link Object} cannot be resolved.
   * @throws EOFException if the byte array does not contain the complete contents of a serialized {@link Object}.
   * @throws IOException if an I/O error occurs during deserialization.
   * @throws IllegalArgumentException if the byte array is {@literal null}.
   * @see #deserialize(byte[], ClassLoader)
   * @see #serialize(Object)
   * @see java.io.ByteArrayInputStream
   * @see java.io.ObjectInputStream
   * @see java.io.Serializable
   */
  @SuppressWarnings("unchecked")
  public static <T> T deserialize(byte[] serializedObjectBytes) throws ClassNotFoundException, IOException {

    Assert.notNull(serializedObjectBytes, "An array of bytes to deserialize as an Object is required");

    try (ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(serializedObjectBytes))) {
      return (T) in.readObject();
    }
  }

  /**
   * Deserializes the given, required byte array into an {@link Object} of the declared {@link Class type}
   * that is resolvable from the given {@link ClassLoader}.
   *
   * @param <T> {@link Class type} of {@link Object} deserialized from the given bytes.
   * @param serializedObjectBytes byte array containing the bytes of the object to deserialize;
   * must not be {@literal null}.
   * @param classLoader Java {@link ClassLoader} used to resolve the {@link Class type}
   * of the serialized {@link Object}.
   * @return an {@link Object} deserialized from the given array of bytes.
   * @throws ClassNotFoundException if the {@link Class type} of the serialized {@link Object} cannot be resolved
   * using the given Java {@link ClassLoader}.
   * @throws EOFException if the byte array does not contain the complete contents of a serialized {@link Object}.
   * @throws IOException if an I/O error occurs during deserialization.
   * @throws IllegalArgumentException if the byte array is {@literal null}.
   * @see org.cp.elements.io.IOUtils.ClassLoaderObjectInputStream
   * @see #deserialize(byte[])
   * @see #serialize(Object)
   * @see java.io.ByteArrayInputStream
   * @see java.io.ObjectInputStream
   * @see java.io.Serializable
   * @see java.lang.ClassLoader
   */
  @SuppressWarnings("unchecked")
  public static <T> T deserialize(byte[] serializedObjectBytes, ClassLoader classLoader)
      throws ClassNotFoundException, IOException {

    Assert.notNull(serializedObjectBytes, "An array of bytes to deserialize as an Object is required");

    try (ObjectInputStream in =
           new ClassLoaderObjectInputStream(new ByteArrayInputStream(serializedObjectBytes), classLoader)) {

      return (T) in.readObject();
    }
  }

  /**
   * Serializes the given {@link java.io.Serializable} {@link Object} into an array of bytes.
   *
   * @param obj {@link java.io.Serializable} {@link Object} to serialize into an array of bytes.
   * @return the byte array of the serialized {@link Object}.
   * @throws IOException if an I/O error occurs during serialization.
   * @see #deserialize(byte[])
   * @see java.io.ByteArrayOutputStream
   * @see java.io.ObjectOutputStream
   * @see java.io.Serializable
   */
  public static byte[] serialize(Object obj) throws IOException {

    ByteArrayOutputStream out = new ByteArrayOutputStream();

    try (ObjectOutputStream objectOutput = new ObjectOutputStream(out)) {
      objectOutput.writeObject(obj);
      objectOutput.flush();
      return out.toByteArray();
    }
  }

  /**
   * Reads the contents of the given, required {@link InputStream} into a byte array.
   *
   * @param in {@link InputStream} to read content from; must not be {@literal null}.
   * @return a byte array containing the contents of the given {@link InputStream}.
   * @throws IOException if an I/O error occurs while reading from the {@link InputStream}.
   * @throws IllegalArgumentException if the {@link InputStream} is {@literal null}.
   * @see java.io.ByteArrayOutputStream
   * @see java.io.InputStream
   */
  @NullSafe
  public static byte[] toByteArray(@NotNull InputStream in) throws IOException {

    Assert.notNull(in, "InputStream is required");

    ByteArrayOutputStream out = new ByteArrayOutputStream(in.available());

    byte[] buffer = new byte[DEFAULT_BUFFER_SIZE];

    int bytesRead;

    while ((bytesRead = in.read(buffer)) != -1) {
      out.write(buffer, 0, bytesRead);
      out.flush();
    }

    return out.toByteArray();
  }

  /**
   * {@link ObjectInputStream} implementation that resolves the {@link Class type} of the {@link Object}
   * being deserialized with the configured Java {@link ClassLoader}.
   *
   * @see java.lang.ClassLoader
   * @see java.lang.ClassLoader#getSystemClassLoader()
   * @see java.lang.Thread#getContextClassLoader()
   * @see java.io.ObjectInputStream
   */
  protected static class ClassLoaderObjectInputStream extends ObjectInputStream {

    protected static final ClassLoader DEFAULT_CLASS_LOADER = Thread.currentThread().getContextClassLoader();

    private final ClassLoader classLoader;

    /**
     * Constructs a new instance of {@link ClassLoaderObjectInputStream} initialized with the given,
     * required {@link InputStream} from which the {@link Object} will be read and deserialized along with
     * the given {@link ClassLoader} used to resolve the {@link Class type} of the serialized {@link Object}.
     *
     * @param in {@link InputStream} source for the serialized {@link Object} bytes; must not be {@literal null}.
     * @param classLoader {@link ClassLoader} used to resolve the {@link Class type} of the serialized {@link Object}.
     * @throws IOException if an I/O error occurs during initialization.
     * @see java.lang.ClassLoader
     * @see java.io.InputStream
     */
    protected ClassLoaderObjectInputStream(@NotNull InputStream in, @Nullable ClassLoader classLoader)
        throws IOException {

      super(in);
      this.classLoader = classLoader != null ? classLoader : DEFAULT_CLASS_LOADER;
    }

    /**
     * Returns a reference to the configured {@link ClassLoader} used to resolve the {@link Class type}
     * of the serialized {@link Object}.
     *
     * @return a reference to the configured {@link ClassLoader} used to resolve the {@link Class type}
     * of the serialized {@link Object}.
     * @see java.lang.ClassLoader
     */
    protected @NotNull ClassLoader getClassLoader() {
      return this.classLoader;
    }

    @Override
    protected @NotNull Class<?> resolveClass(@NotNull ObjectStreamClass descriptor) throws ClassNotFoundException {
      return Class.forName(descriptor.getName(), false, getClassLoader());
    }
  }

  /**
   * Java {@link FunctionalInterface} defining a operation that throws an {@link IOException}.
   */
  @FunctionalInterface
  public interface IoExceptionThrowingOperation {

    /**
     * Performs the IO operation with the possibility of throwing an {@link IOException}.
     *
     * @throws IOException if an IO error occurs during the operation.
     */
    void doIo() throws IOException;

  }
}
