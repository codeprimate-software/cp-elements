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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.junit.MockitoJUnitRunner;

/**
 * Unit Tests for {@link ModifierUtils}.
 *
 * @author John Blum
 * @see java.lang.Object
 * @see java.lang.Class
 * @see java.lang.reflect.Field
 * @see java.lang.reflect.Member
 * @see java.lang.reflect.Method
 * @see java.lang.reflect.Modifier
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.junit.MockitoJUnitRunner
 * @see org.cp.elements.lang.reflect.ModifierUtils
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ModifierUtilsUnitTests {

  @Mock
  private Member mockMember;

  @Test
  public void isAbstractReturnsTrue() {

    doReturn(Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isAbstract(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isAbstractWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PUBLIC | Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isAbstract(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isAbstractReturnsFalse() {

    doReturn(0).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isAbstract(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isFinalReturnsTrue() {

    doReturn(Modifier.FINAL).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isFinal(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isFinalWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PRIVATE | Modifier.FINAL).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isFinal(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isFinalReturnsFalse() {

    doReturn(0).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isFinal(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isInterfaceReturnsTrue() {
    assertThat(ModifierUtils.isInterface(Cloneable.class)).isTrue();
  }

  @Test
  public void isInterfaceWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PRIVATE | Modifier.INTERFACE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isInterface(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isInterfaceReturnsFalse() {
    assertThat(ModifierUtils.isInterface(Object.class)).isFalse();
  }

  @Test
  public void isNativeReturnsTrue() {

    doReturn(Modifier.NATIVE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isNative(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isNativeWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.ABSTRACT | Modifier.NATIVE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isNative(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isNativeReturnsFalse() {

    doReturn(Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isNative(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPackagePrivateReturnTrue() {

    doReturn(Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPackagePrivate(this.mockMember)).isTrue();

    verify(this.mockMember, times(3)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPackagePrivateWithPublicAccessReturnsFalse() {

    doReturn(Modifier.PUBLIC).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPackagePrivate(this.mockMember)).isFalse();

    verify(this.mockMember, times(3)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPackagePrivateWithProtectedAccessReturnsFalse() {

    doReturn(Modifier.PROTECTED).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPackagePrivate(this.mockMember)).isFalse();

    verify(this.mockMember, times(2)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPackagePrivateWithPrivateAccessReturnsFalse() {

    doReturn(Modifier.PRIVATE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPackagePrivate(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPrivateReturnsTrue() {

    doReturn(Modifier.PRIVATE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPrivate(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPrivateWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PRIVATE | Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPrivate(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPrivateReturnsFalse() {

    doReturn(Modifier.PUBLIC | Modifier.INTERFACE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPrivate(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isProtectedReturnsTrue() {

    doReturn(Modifier.PROTECTED).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isProtected(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isProtectedWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PROTECTED | Modifier.ABSTRACT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isProtected(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isProtectedReturnsFalse() {

    doReturn(Modifier.PRIVATE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isProtected(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPublicReturnsTrue() {

    doReturn(Modifier.PUBLIC).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPublic(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPublicWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PUBLIC | Modifier.INTERFACE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPublic(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isPublicReturnsFalse() {

    doReturn(Modifier.PRIVATE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isPublic(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStaticIsTrue() {

    doReturn(Modifier.STATIC).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStatic(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStaticWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.STATIC | Modifier.INTERFACE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStatic(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStaticReturnsFalse() {

    doReturn(Modifier.ABSTRACT | Modifier.INTERFACE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStatic(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStrictIsTrue() {

    doReturn(Modifier.STRICT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStrict(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStrictWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PUBLIC | Modifier.STRICT | Modifier.FINAL).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStrict(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isStrictReturnsFalse() {

    doReturn(Modifier.FINAL).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isStrict(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isSynchronizedIsTrue() {

    doReturn(Modifier.SYNCHRONIZED).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isSynchronized(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isSynchronizedWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.TRANSIENT)
      .when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isSynchronized(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isSynchronizedReturnsFalse() {

    doReturn(Modifier.STRICT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isSynchronized(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isTransientIsTrue() {

    doReturn(Modifier.TRANSIENT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isTransient(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isTransientWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.PRIVATE | Modifier.FINAL | Modifier.TRANSIENT).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isTransient(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isTransientReturnsFalse() {

    doReturn(Modifier.SYNCHRONIZED).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isTransient(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isVolatileIsTrue() {

    doReturn(Modifier.VOLATILE).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isVolatile(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isVolatileWithMultipleModifiersReturnsTrue() {

    doReturn(Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.VOLATILE)
      .when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isVolatile(this.mockMember)).isTrue();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void isVolatileReturnsFalse() {

    doReturn(Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.TRANSIENT)
      .when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.isVolatile(this.mockMember)).isFalse();

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void getModifiersForClass() {
    assertThat(ModifierUtils.getModifiers(Object.class)).isEqualTo(Modifier.PUBLIC);
  }

  @Test
  public void getModifiersForClassMember() {
    assertThat(ModifierUtils.getModifiers(TestClass.class))
      .isEqualTo(Modifier.PRIVATE | Modifier.STATIC | Modifier.ABSTRACT);
  }

  @Test
  public void getModifiersForInterface() {

    // NOTE: Interfaces are abstract by default.

    assertThat(ModifierUtils.getModifiers(Cloneable.class))
      .isEqualTo(Modifier.PUBLIC | Modifier.INTERFACE | Modifier.ABSTRACT);
  }

  @Test
  public void getModifiersForInterfaceMember() {

    // NOTE: Member interfaces are static by default.

    assertThat(ModifierUtils.getModifiers(TestInterface.class))
      .isEqualTo(Modifier.STATIC | Modifier.INTERFACE | Modifier.ABSTRACT);
  }

  @Test
  public void getModifiersForField() throws NoSuchFieldException {

    Field testField = TestClass.class.getDeclaredField("testField");

    assertThat(testField).isNotNull();
    assertThat(testField.getName()).isEqualTo("testField");
    assertThat(ModifierUtils.getModifiers(testField)).isEqualTo(Modifier.PRIVATE | Modifier.VOLATILE);
    assertThat(ModifierUtils.isPrivate(testField)).isTrue();
    assertThat(ModifierUtils.isVolatile(testField)).isTrue();
    assertThat(ModifierUtils.isSynchronized(testField)).isFalse();
  }

  @Test
  public void getModifiersForMember() {

    int modifiers = Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.VOLATILE;

    doReturn(modifiers).when(this.mockMember).getModifiers();

    assertThat(ModifierUtils.getModifiers(this.mockMember)).isEqualTo(modifiers);

    verify(this.mockMember, times(1)).getModifiers();
    verifyNoMoreInteractions(this.mockMember);
  }

  @Test
  public void getModifiersForMethod() throws NoSuchMethodException {

    Method testMethod = TestClass.class.getMethod("testMethod");

    assertThat(testMethod).isNotNull();
    assertThat(testMethod.getName()).isEqualTo("testMethod");
    assertThat(ModifierUtils.getModifiers(testMethod)).isEqualTo(Modifier.PUBLIC | Modifier.FINAL);
    assertThat(ModifierUtils.isPublic(testMethod)).isTrue();
    assertThat(ModifierUtils.isFinal(testMethod)).isTrue();
    assertThat(ModifierUtils.isStatic(testMethod)).isFalse();
  }

  @Test
  public void getModifiersForNullIsNullSafe() {
    assertThat(ModifierUtils.getModifiers(null)).isEqualTo(0);
  }
  @Test
  public void getModifiersForObject() {

    TestClass testInstance = new TestClass() { };

    //System.out.printf("MODIFIERS [%d]%n", testInstance.getClass().getModifiers());
    //System.out.printf("MODIFIERS [%s]%n", Modifiers.modifiersFrom(testInstance));

    assertThat(ModifierUtils.getModifiers(testInstance)).isNotEqualTo(TestClass.class.getModifiers());
    //assertThat(ModifierUtils.isPrivate(testInstance)).isTrue();
    //assertThat(ModifierUtils.isStatic(testInstance)).isTrue();
    //assertThat(ModifierUtils.isAbstract(testInstance)).isTrue();
    assertThat(ModifierUtils.isInterface(testInstance)).isFalse();
  }

  @SuppressWarnings("all")
  private static abstract class TestClass {

    private volatile Object testField = "test";

    public final void testMethod() { }

  }

  interface TestInterface { }

}
