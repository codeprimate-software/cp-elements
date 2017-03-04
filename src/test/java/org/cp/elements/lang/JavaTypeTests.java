/*
 * Copyright 2016 Author or Authors.
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

import org.junit.Test;

import lombok.Data;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;

/**
 * The JavaTypeTests class...
 *
 * @author John Blum
 * @since 1.0.0
 */
public class JavaTypeTests {

  @Test
  @SuppressWarnings("all")
  public void isJavaTypeWithObjectIsTrue() {
    assertThat(JavaType.isJavaType(new BigDecimal(3.14159d))).isTrue();
    assertThat(JavaType.isJavaType(new BigInteger("123456789"))).isTrue();
    assertThat(JavaType.isJavaType(false)).isTrue();
    assertThat(JavaType.isJavaType(Boolean.TRUE)).isTrue();
    assertThat(JavaType.isJavaType((byte) 16)).isTrue();
    assertThat(JavaType.isJavaType(Byte.valueOf((byte) 64))).isTrue();
    assertThat(JavaType.isJavaType('x')).isTrue();
    assertThat(JavaType.isJavaType(Character.valueOf('X'))).isTrue();
    assertThat(JavaType.isJavaType(Math.PI)).isTrue();
    assertThat(JavaType.isJavaType(Double.valueOf(3.14159d))).isTrue();
    assertThat(JavaType.isJavaType(3.14159f)).isTrue();
    assertThat(JavaType.isJavaType(Float.valueOf(3.14159f))).isTrue();
    assertThat(JavaType.isJavaType(2)).isTrue();
    assertThat(JavaType.isJavaType(Integer.valueOf(42))).isTrue();
    assertThat(JavaType.isJavaType(123456789L)).isTrue();
    assertThat(JavaType.isJavaType(Long.valueOf(1234567890L))).isTrue();
    assertThat(JavaType.isJavaType((short) 8192)).isTrue();
    assertThat(JavaType.isJavaType(Short.valueOf((short) 16384))).isTrue();
    assertThat(JavaType.isJavaType("test")).isTrue();
    assertThat(JavaType.isJavaType(new Thread("test"))).isTrue();
    assertThat(JavaType.isJavaType(new RuntimeException("test"))).isTrue();
  }

  @Test
  public void isJavaTypeWithObjectIsFalse() {
    assertThat(JavaType.isJavaType(Contact.newContact("John Blum"))).isFalse();
  }

  @Test
  public void isJavaTypeWithClassIsTrue() {
    for (JavaType javaType : JavaType.values()) {
      assertThat(JavaType.isJavaType(javaType.getType())).isTrue();
    }
  }

  @Test
  public void isJavaTypeWithClassIsFalse() {
    assertThat(JavaType.isJavaType(Contact.class)).isFalse();
  }

  @Test
  public void valueOfJavaTypeIsSuccessful() {
    for (JavaType javaType : JavaType.values()) {
      assertThat(JavaType.valueOf(javaType.getType())).isEqualTo(javaType);
    }
  }

  @Test(expected = TypeNotFoundException.class)
  public void valueOfNonJavaTypeThrowsTypeNotFoundException() {
    try {
      JavaType.valueOf(Contact.class);
    }
    catch (TypeNotFoundException expected) {
      assertThat(expected).hasMessage("No JavaType found for class type [%s]", Contact.class.getName());
      assertThat(expected).hasNoCause();
      throw expected;
    }
  }

  @Test
  public void getTypeIsCorrect() {
    assertThat(JavaType.BIG_DECIMAL.getType()).isEqualTo(BigDecimal.class);
    assertThat(JavaType.BIG_INTEGER.getType()).isEqualTo(BigInteger.class);
    assertThat(JavaType.BOOLEAN.getType()).isEqualTo(Boolean.class);
    assertThat(JavaType.BOOLEAN_TYPE.getType()).isEqualTo(Boolean.TYPE);
    assertThat(JavaType.BYTE.getType()).isEqualTo(Byte.class);
    assertThat(JavaType.BYTE_TYPE.getType()).isEqualTo(Byte.TYPE);
    assertThat(JavaType.CHARACTER.getType()).isEqualTo(Character.class);
    assertThat(JavaType.CHARACTER_TYPE.getType()).isEqualTo(Character.TYPE);
    assertThat(JavaType.DOUBLE.getType()).isEqualTo(Double.class);
    assertThat(JavaType.DOUBLE_TYPE.getType()).isEqualTo(Double.TYPE);
    assertThat(JavaType.FLOAT.getType()).isEqualTo(Float.class);
    assertThat(JavaType.FLOAT_TYPE.getType()).isEqualTo(Float.TYPE);
    assertThat(JavaType.INTEGER.getType()).isEqualTo(Integer.class);
    assertThat(JavaType.INTEGER_TYPE.getType()).isEqualTo(Integer.TYPE);
    assertThat(JavaType.LONG.getType()).isEqualTo(Long.class);
    assertThat(JavaType.LONG_TYPE.getType()).isEqualTo(Long.TYPE);
    assertThat(JavaType.NUMBER.getType()).isEqualTo(Number.class);
    assertThat(JavaType.SHORT.getType()).isEqualTo(Short.class);
    assertThat(JavaType.SHORT_TYPE.getType()).isEqualTo(Short.TYPE);
    assertThat(JavaType.STRING.getType()).isEqualTo(String.class);
    assertThat(JavaType.THREAD.getType()).isEqualTo(Thread.class);
    assertThat(JavaType.THROWABLE.getType()).isEqualTo(Throwable.class);
  }

  @Data
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {
    @NonNull String name;
  }
}
