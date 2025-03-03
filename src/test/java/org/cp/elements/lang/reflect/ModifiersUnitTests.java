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
package org.cp.elements.lang.reflect;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Set;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Modifiers}.
 *
 * @author John Blum
 * @see java.lang.reflect.Modifier
 * @see org.cp.elements.lang.reflect.Modifiers
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 1.0.0
 */
class ModifiersUnitTests {

  @Test
  void fromJavaModifierToModifierEnumeratedValueIsCorrect() {

    for (int javaModifier : ModifierUtils.JAVA_MODIFIERS) {

      Modifiers modifierEnum = Modifiers.from(javaModifier);

      assertThat((Object) modifierEnum).isNotNull();
      assertThat(modifierEnum.getModifier()).isEqualTo(javaModifier);
      assertThat(modifierEnum.toString()).isEqualTo(modifierEnum.name().toLowerCase());
    }
  }

  @Test
  void fromUnknownJavaModifierReturnsNull() {

    assertThat((Object) Modifiers.from(0)).isNull();
    assertThat((Object) Modifiers.from(-1)).isNull();
    assertThat((Object) Modifiers.from(Modifier.ABSTRACT | Modifier.STATIC)).isNull();
  }

  @Test
  void modifiersFromClass() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(Object.class);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(1);
    assertThat(modifiers).containsExactly(Modifiers.PUBLIC);
  }

  @Test
  void modifiersFromClassMemberWithNoModifiers() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(DummyClass.class);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).isEmpty();
  }

  @Test
  void modifiersFromInterface() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(Cloneable.class);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(3);
    assertThat(modifiers).containsExactlyInAnyOrder(Modifiers.ABSTRACT, Modifiers.INTERFACE, Modifiers.PUBLIC);
  }

  @Test
  void modifiersFromInterfaceMember() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(TestInterface.class);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(3);
    assertThat(modifiers).containsExactlyInAnyOrder(Modifiers.ABSTRACT, Modifiers.INTERFACE, Modifiers.STATIC);
  }

  @Test
  void modifiersFromField() throws NoSuchFieldException {

    Field testField = TestClass.class.getDeclaredField("testField");

    assertThat(testField).isNotNull();
    assertThat(testField.getName()).isEqualTo("testField");

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(testField);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(2);
    assertThat(modifiers).containsExactlyInAnyOrder(Modifiers.PRIVATE, Modifiers.VOLATILE);
  }

  @Test
  void modifiersFromMember() {

    Member mockMember = mock(Member.class);

    doReturn(Modifier.PUBLIC | Modifier.STATIC | Modifier.SYNCHRONIZED | Modifier.FINAL)
      .when(mockMember).getModifiers();

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(mockMember);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(4);
    assertThat(modifiers)
      .containsExactlyInAnyOrder(Modifiers.PUBLIC, Modifiers.STATIC, Modifiers.SYNCHRONIZED, Modifiers.FINAL);

    verify(mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(mockMember);
  }

  @Test
  void modifiersFromMethod() throws NoSuchMethodException {

    Method testMethod = TestClass.class.getDeclaredMethod("testMethod");

    assertThat(testMethod).isNotNull();
    assertThat(testMethod.getName()).isEqualTo("testMethod");

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(testMethod);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(3);
    assertThat(modifiers).containsExactlyInAnyOrder(Modifiers.PUBLIC, Modifiers.SYNCHRONIZED, Modifiers.FINAL);
  }

  @Test
  void modifiersFromNull() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(null);

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).isEmpty();
  }

  @Test
  void modifiersFromObject() {

    Set<Modifiers> modifiers = Modifiers.modifiersFrom(new TestClass());

    assertThat(modifiers).isNotNull();
    assertThat(modifiers).hasSize(2);
    assertThat(modifiers).containsExactlyInAnyOrder(Modifiers.PRIVATE, Modifiers.STATIC);
  }

  @Test
  void predicateTestReturnsTrue() {
    assertThat(Modifiers.PRIVATE.test(TestClass.class)).isTrue();
  }

  @Test
  void predicateTestReturnsFalse() {
    assertThat(Modifiers.INTERFACE.test(TestClass.class)).isFalse();
  }

  @Test
  void predicateAndTestReturnsTrue() {
    assertThat(Modifiers.ABSTRACT.and(Modifiers.INTERFACE).and(Modifiers.STATIC).test(TestInterface.class)).isTrue();
  }

  @Test
  void predicateAndTestReturnsFalse() {
    assertThat(Modifiers.PRIVATE.and(Modifiers.STATIC).and(Modifiers.INTERFACE).test(TestClass.class)).isFalse();
  }

  @Test
  void predicateOrTestReturnsTrue() {
    assertThat(Modifiers.PRIVATE.or(Modifiers.INTERFACE).test(TestClass.class)).isTrue();
  }

  @Test
  void predicateOrTestReturnsFalse() {
    assertThat(Modifiers.PUBLIC.or(Modifiers.FINAL).test(TestInterface.class)).isFalse();
  }

  @SuppressWarnings("all")
  class DummyClass { }

  @SuppressWarnings("all")
  private static class TestClass {

    private volatile Object testField = "test";

    public synchronized final void testMethod() { }

  }

  interface TestInterface { }

}
