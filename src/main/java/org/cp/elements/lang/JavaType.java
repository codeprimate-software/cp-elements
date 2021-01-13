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

package org.cp.elements.lang;

import static org.cp.elements.lang.ClassUtils.assignableTo;
import static org.cp.elements.lang.ClassUtils.getName;
import static org.cp.elements.lang.ElementsExceptionsFactory.newTypeNotFoundException;
import static org.cp.elements.util.stream.StreamUtils.stream;

import java.math.BigDecimal;
import java.math.BigInteger;

/**
 * The {@link JavaType} enum represents various Java {@link Class} types.
 *
 * @author John Blum
 * @see java.lang.Class
 * @see org.cp.elements.lang.ClassUtils
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public enum JavaType {

  BIG_DECIMAL(BigDecimal.class),
  BIG_INTEGER(BigInteger.class),
  BOOLEAN(Boolean.class),
  BOOLEAN_TYPE(Boolean.TYPE),
  BYTE(Byte.class),
  BYTE_TYPE(Byte.TYPE),
  CHARACTER(Character.class),
  CHARACTER_TYPE(Character.TYPE),
  DOUBLE(Double.class),
  DOUBLE_TYPE(Double.TYPE),
  FLOAT(Float.class),
  FLOAT_TYPE(Float.TYPE),
  INTEGER(Integer.class),
  INTEGER_TYPE(Integer.TYPE),
  LONG(Long.class),
  LONG_TYPE(Long.TYPE),
  NUMBER(Number.class),
  SHORT(Short.class),
  SHORT_TYPE(Short.TYPE),
  STRING(String.class),
  THREAD(Thread.class),
  THROWABLE(Throwable.class);

  /**
   * Determines whether the given {@link Object} is a {@link JavaType}.
   *
   * @param obj {@link Object} to evaluate as a {@link JavaType}.
   * @return a boolean value indicating whether the given {@link Object} is an instance of a {@link JavaType}.
   * @see java.lang.Class#isInstance(Object)
   * @see java.lang.Object
   * @see #values()
   * @see #getType()
   */
  public static boolean isJavaType(Object obj) {
    return stream(values()).anyMatch(javaType -> javaType.getType().isInstance(obj));
  }

  /**
   * Determines whether the given {@link Class} type is a {@link JavaType}.
   *
   * @param type {@link Class} to evaluate as a {@link JavaType}.
   * @return a boolean value indicating whether the given {@link Class} type is a {@link JavaType}.
   * @see org.cp.elements.lang.ClassUtils#assignableTo(Class, Class)
   * @see java.lang.Class
   * @see #values()
   */
  public static boolean isJavaType(Class<?> type) {
    return stream(values()).anyMatch(javaType -> (type != null && assignableTo(type, javaType.getType())));
  }

  /**
   * Returns a {@link JavaType} enumerated value for the given {@link Class} type.
   *
   * @param type {@link Class} of the {@link JavaType} to return.
   * @return a {@link JavaType} enumerated value for the given {@link Class} type.
   * @throws TypeNotFoundException if no {@link JavaType} matches the given {@link Class} type.
   * @see java.lang.Class
   */
  public static JavaType valueOf(Class<?> type) {

    return stream(values()).filter(javaType -> javaType.getType().equals(type)).findFirst()
      .orElseThrow(() -> newTypeNotFoundException("No JavaType found for class type [%s]", getName(type)));
  }

  private final Class type;

  /**
   * Constructs a new instance of the {@link JavaType} enumerated value initialized with
   * the given Java {@link Class} type.
   *
   * @param type {@link Class} representing a Java {@link Class Type}.
   * @see java.lang.Class
   */
  JavaType(Class type) {

    Assert.notNull(type, "Class type cannot be null");

    this.type = type;
  }

  /**
   * Return the actual {@link Class} type of the {@link JavaType} enumerated value.
   *
   * @return the actual {@link Class} type of the {@link JavaType} enumerated value.
   * @see java.lang.Class
   */
  public Class getType() {
    return this.type;
  }

  /**
   * Returns a {@link String} representation (view) of this {@link JavaType}.
   *
   * @return a {@link String} snapshot containing the current state of this {@link JavaType}.
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString() {
    return getType().getName();
  }
}
