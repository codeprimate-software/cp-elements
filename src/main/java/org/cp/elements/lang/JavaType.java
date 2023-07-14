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

import static org.cp.elements.lang.ElementsExceptionsFactory.newTypeNotFoundException;

import java.math.BigDecimal;
import java.math.BigInteger;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.util.stream.StreamUtils;

/**
 * {@link Enum Enumeration} of various Java [primitive] {@link Class types}.
 *
 * @author John Blum
 * @see java.lang.Class
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
  THROWABLE(Throwable.class),
  URI(java.net.URI.class),
  URL(java.net.URL.class),
  CLOCK(java.time.Clock.class),
  DURATION(java.time.Duration.class),
  INSTANT(java.time.Instant.class),
  LOCAL_DATE(java.time.LocalDate.class),
  LOCAL_DATE_TIME(java.time.LocalDateTime.class),
  LOCAL_TIME(java.time.LocalTime.class),
  MONTH_DAY(java.time.MonthDay.class),
  OFFSET_DATE_TIME(java.time.OffsetDateTime.class),
  OFFSET_TIME(java.time.OffsetTime.class),
  PERIOD(java.time.Period.class),
  YEAR(java.time.Year.class),
  YEAR_MONTH(java.time.YearMonth.class),
  ZONED_DATE_TIME(java.time.ZonedDateTime.class),
  ZONED_ID(java.time.ZoneId.class),
  ZONED_OFFSET(java.time.ZoneOffset.class);

  /**
   * Determines whether the given {@link Object} is a {@link JavaType}.
   *
   * @param target {@link Object} to evaluate as a {@link JavaType}.
   * @return a boolean value indicating whether the given {@link Object} is an instance of a {@link JavaType}.
   * @see java.lang.Class#isInstance(Object)
   * @see java.lang.Object
   * @see #values()
   */
  public static boolean isJavaType(@Nullable Object target) {
    return StreamUtils.stream(values()).anyMatch(javaType -> javaType.getType().isInstance(target));
  }

  /**
   * Determines whether the given {@link Class type} is a {@link JavaType}.
   *
   * @param type {@link Class} to evaluate as a {@link JavaType}.
   * @return a boolean value indicating whether the given {@link Class type} is a {@link JavaType}.
   * @see org.cp.elements.lang.ClassUtils#assignableTo(Class, Class)
   * @see java.lang.Class
   * @see #values()
   */
  public static boolean isJavaType(@Nullable Class<?> type) {

    return type != null && StreamUtils.stream(values())
      .anyMatch(javaType -> ClassUtils.assignableTo(type, javaType.getType()));
  }

  /**
   * Factory method used to return a {@link JavaType} enumerated value for the given {@link Class type}.
   *
   * @param type {@link Class} of the {@link JavaType} to return.
   * @return a {@link JavaType} enumerated value for the given {@link Class type}.
   * @throws TypeNotFoundException if no {@link JavaType} matches the given {@link Class type}.
   * @see java.lang.Class
   */
  public static @NotNull JavaType valueOf(@Nullable Class<?> type) {

    return StreamUtils.stream(values())
      .filter(javaType -> javaType.getType().equals(type))
      .findFirst()
      .orElseThrow(() -> newTypeNotFoundException("No JavaType found for class type [%s]", ClassUtils.getName(type)));
  }

  private final Class<?> type;

  /**
   * Constructs a new the {@link JavaType} enumerated value initialized with
   * the given Java {@link Class type}.
   *
   * @param type {@link Class} representing the Java {@link Class type}.
   * @see java.lang.Class
   */
  JavaType(@NotNull Class<?> type) {
    this.type = ObjectUtils.requireObject(type, "Class type is required");
  }

  /**
   * Return the actual Java {@link Class type} of the {@link JavaType} enumerated value.
   *
   * @return the actual Java {@link Class type} of the {@link JavaType} enumerated value.
   * @see java.lang.Class
   */
  public @NotNull Class<?> getType() {
    return this.type;
  }

  /**
   * Returns a {@link String} representation of this {@link JavaType}.
   *
   * @return a {@link String} describing this {@link JavaType}.
   * @see java.lang.Object#toString()
   */
  @Override
  public @NotNull String toString() {
    return getType().getName();
  }
}
