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
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.net.URI;
import java.net.URL;

import org.junit.jupiter.api.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

/**
 * Unit Tests for {@link JavaType}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.JavaType
 * @since 1.0.0
 */
public class JavaTypeUnitTests {

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
  public void isJavaTypeWithJavaNetTypesIsTrue() throws Exception {

    assertThat(JavaType.isJavaType(java.net.URI.create("http://www.example.com"))).isTrue();
    assertThat(JavaType.isJavaType(java.net.URI.create("http://www.example.com").toURL())).isTrue();
  }

  @Test
  public void isJavaTypeWithJavaTimeTypesIsTrue() {

    assertThat(JavaType.isJavaType(java.time.Clock.systemUTC())).isTrue();
    assertThat(JavaType.isJavaType(java.time.Duration.ofMillis(1))).isTrue();
    assertThat(JavaType.isJavaType(java.time.Instant.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.LocalDate.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.LocalDateTime.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.LocalTime.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.MonthDay.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.OffsetDateTime.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.Period.of(1, 2, 4))).isTrue();
    assertThat(JavaType.isJavaType(java.time.Year.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.YearMonth.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.ZonedDateTime.now())).isTrue();
    assertThat(JavaType.isJavaType(java.time.ZoneId.systemDefault())).isTrue();
    assertThat(JavaType.isJavaType(java.time.ZoneOffset.systemDefault())).isTrue();
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

  @Test
  public void valueOfNonJavaTypeThrowsTypeNotFoundException() {

    assertThatExceptionOfType(TypeNotFoundException.class)
      .isThrownBy(() -> JavaType.valueOf(Contact.class))
      .withMessage("No JavaType found for class type [%s]", Contact.class.getName())
      .withNoCause();
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
    assertThat(JavaType.URI.getType()).isEqualTo(URI.class);
    assertThat(JavaType.URL.getType()).isEqualTo(URL.class);
  }

  @Getter
  @ToString(of = "name")
  @EqualsAndHashCode(of = "name")
  @RequiredArgsConstructor(staticName = "newContact")
  static class Contact {

    @lombok.NonNull
    private final String name;

  }
}
