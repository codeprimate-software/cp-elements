/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.lang.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatExceptionOfType;

import java.time.Instant;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.TypeNotFoundException;
import org.cp.elements.security.model.User;
import org.cp.elements.security.model.Users;
import org.cp.elements.util.stream.StreamUtils;

/**
 * Unit Tests for {@link SimpleTypeResolver}.
 *
 * @author John Blum
 * @see org.junit.jupiter.api.Test
 * @see org.cp.elements.lang.support.SimpleTypeResolver
 * @since 2.0.0
 */
public class SimpleTypeResolverUnitTests {

  @Test
  void resolveClassType() {
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(Class.class)).isEqualTo(Class.class);
  }

  @Test
  void resolveObjectType() {
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(new Object())).isEqualTo(Object.class);
  }

  @Test
  void resolveTimeType() {
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(Instant.now())).isEqualTo(Instant.class);
  }

  @Test
  void resolveWrapperType() {

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(true)).isEqualTo(Boolean.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType('x')).isEqualTo(Character.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(1)).isEqualTo(Integer.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(Math.PI)).isEqualTo(Double.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType("TEST")).isEqualTo(String.class);
  }

  @Test
  void resolveArrayType() {

    Object array = new String[] { "test" };

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(new Integer[0])).isEqualTo(Integer.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(array)).isEqualTo(String.class);
  }

  @Test
  void resolveTwoDimensionalArrayType() {

    Object array = new Character[][] { { 'a', 'b', 'c' }, { 'x', 'y', 'z' }};

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(new Double[10][5])).isEqualTo(Double.class);
    assertThat(SimpleTypeResolver.INSTANCE.resolveType(array)).isEqualTo(Character.class);
  }

  @Test
  void resolveListType() {

    List<Integer> list = List.of(1);

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(list)).isEqualTo(Integer.class);
  }

  @Test
  void resolveSetType() {

    Set<Double> set = Set.of(Math.PI);

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(set)).isEqualTo(Double.class);
  }

  @Test
  void resolveStreamType() {

    Stream<Byte> stream = Stream.of((byte) 0xCA);

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(stream)).isEqualTo(Byte.class);
  }

  @Test
  void resolveUserType() {

    Users users = Users.empty();

    assertThat(SimpleTypeResolver.INSTANCE.resolveType(users)).isEqualTo(User.class);
  }

  @Test
  void resolveTypeWithNull() {

    assertThatExceptionOfType(TypeNotFoundException.class)
      .isThrownBy(() -> SimpleTypeResolver.INSTANCE.resolveType(null))
      .withMessage("Unable to resolve type of object [null]")
      .withNoCause();
  }

  @Test
  void resolveTypeFromEmptyStream() {

    Stream<?> emptyStream = StreamUtils.empty();

    assertThatExceptionOfType(TypeNotFoundException.class)
      .isThrownBy(() -> SimpleTypeResolver.INSTANCE.resolveType(emptyStream))
      .withMessage("Unable to resolve element type from [%s]", emptyStream)
      .withNoCause();
  }
}
