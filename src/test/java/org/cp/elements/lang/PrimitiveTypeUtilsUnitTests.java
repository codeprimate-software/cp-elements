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

import static org.assertj.core.api.Assertions.assertThat;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.function.Function;

import org.cp.elements.security.model.User;
import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link PrimitiveTypeUtils}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.PrimitiveTypeUtils
 * @since 1.0.0
 */
public class PrimitiveTypeUtilsUnitTests {

  @Test
  @SuppressWarnings("rawtypes")
  public void fromPrimitiveTypeToWrapperType() {

    Function<Class, Class> primitiveToWrapperType = PrimitiveTypeUtils.primitiveToWrapperType();

    assertThat(primitiveToWrapperType).isNotNull();
    assertThat(primitiveToWrapperType.apply(boolean.class)).isEqualTo(Boolean.class);
    assertThat(primitiveToWrapperType.apply(Boolean.TYPE)).isEqualTo(Boolean.class);
    assertThat(primitiveToWrapperType.apply(byte.class)).isEqualTo(Byte.class);
    assertThat(primitiveToWrapperType.apply(Byte.TYPE)).isEqualTo(Byte.class);
    assertThat(primitiveToWrapperType.apply(short.class)).isEqualTo(Short.class);
    assertThat(primitiveToWrapperType.apply(Short.TYPE)).isEqualTo(Short.class);
    assertThat(primitiveToWrapperType.apply(int.class)).isEqualTo(Integer.class);
    assertThat(primitiveToWrapperType.apply(Integer.TYPE)).isEqualTo(Integer.class);
    assertThat(primitiveToWrapperType.apply(long.class)).isEqualTo(Long.class);
    assertThat(primitiveToWrapperType.apply(Long.TYPE)).isEqualTo(Long.class);
    assertThat(primitiveToWrapperType.apply(float.class)).isEqualTo(Float.class);
    assertThat(primitiveToWrapperType.apply(Float.TYPE)).isEqualTo(Float.class);
    assertThat(primitiveToWrapperType.apply(double.class)).isEqualTo(Double.class);
    assertThat(primitiveToWrapperType.apply(Double.TYPE)).isEqualTo(Double.class);
    assertThat(primitiveToWrapperType.apply(char.class)).isEqualTo(Character.class);
    assertThat(primitiveToWrapperType.apply(Character.TYPE)).isEqualTo(Character.class);
  }

  @Test
  public void fromPrimitiveTypeWithBigInteger() {
    assertThat(PrimitiveTypeUtils.primitiveToWrapperType().apply(BigInteger.class)).isEqualTo(BigInteger.class);
  }

  @Test
  public void fromPrimitiveTypeWithNullIsNullSafe() {
    assertThat(PrimitiveTypeUtils.primitiveToWrapperType().apply(null)).isEqualTo(Object.class);
  }

  @Test
  public void fromPrimitiveTypeWithObjectClass() {
    assertThat(PrimitiveTypeUtils.primitiveToWrapperType().apply(Object.class)).isEqualTo(Object.class);
  }

  @Test
  public void fromPrimitiveTypeWithStringClass() {
    assertThat(PrimitiveTypeUtils.primitiveToWrapperType().apply(String.class)).isEqualTo(String.class);
  }

  @Test
  public void fromPrimitiveTypeWithUserClass() {
    assertThat(PrimitiveTypeUtils.primitiveToWrapperType().apply(User.class)).isEqualTo(User.class);
  }

  @Test
  @SuppressWarnings("rawtypes")
  public void fromWrapperTypeToPrimitiveType() {

    Function<Class, Class> wrapperToPrimitiveType = PrimitiveTypeUtils.wrapperToPrimitiveType();

    assertThat(wrapperToPrimitiveType).isNotNull();
    assertThat(wrapperToPrimitiveType.apply(Boolean.class)).isEqualTo(boolean.class);
    assertThat(wrapperToPrimitiveType.apply(Boolean.class)).isEqualTo(Boolean.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Byte.class)).isEqualTo(byte.class);
    assertThat(wrapperToPrimitiveType.apply(Byte.class)).isEqualTo(Byte.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Short.class)).isEqualTo(short.class);
    assertThat(wrapperToPrimitiveType.apply(Short.class)).isEqualTo(Short.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Integer.class)).isEqualTo(int.class);
    assertThat(wrapperToPrimitiveType.apply(Integer.class)).isEqualTo(Integer.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Long.class)).isEqualTo(long.class);
    assertThat(wrapperToPrimitiveType.apply(Long.class)).isEqualTo(Long.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Float.class)).isEqualTo(float.class);
    assertThat(wrapperToPrimitiveType.apply(Float.class)).isEqualTo(Float.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Double.class)).isEqualTo(double.class);
    assertThat(wrapperToPrimitiveType.apply(Double.class)).isEqualTo(Double.TYPE);
    assertThat(wrapperToPrimitiveType.apply(Character.class)).isEqualTo(char.class);
    assertThat(wrapperToPrimitiveType.apply(Character.class)).isEqualTo(Character.TYPE);
  }

  @Test
  public void fromWrapperTypeWithBigDecimal() {
    assertThat(PrimitiveTypeUtils.wrapperToPrimitiveType().apply(BigDecimal.class)).isEqualTo(BigDecimal.class);
  }

  @Test
  public void fromWrapperTypeWithNullIsNullSafe() {
    assertThat(PrimitiveTypeUtils.wrapperToPrimitiveType().apply(Object.class)).isEqualTo(Object.class);
  }

  @Test
  public void fromWrapperTypeWithObjectClass() {
    assertThat(PrimitiveTypeUtils.wrapperToPrimitiveType().apply(Object.class)).isEqualTo(Object.class);
  }

  @Test
  public void fromWrapperTypeWithStringClass() {
    assertThat(PrimitiveTypeUtils.wrapperToPrimitiveType().apply(String.class)).isEqualTo(String.class);
  }

  @Test
  public void fromWrapperTypeWithUserClass() {
    assertThat(PrimitiveTypeUtils.wrapperToPrimitiveType().apply(User.class)).isEqualTo(User.class);
  }
}
