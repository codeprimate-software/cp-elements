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

package org.cp.elements.lang.reflect;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Member;
import java.lang.reflect.Modifier;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

/**
 * Unit tests for {@link ModifierUtils}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.junit.runner.RunWith
 * @see org.mockito.Mock
 * @see org.mockito.Mockito
 * @see org.mockito.runners.MockitoJUnitRunner
 * @see org.cp.elements.lang.reflect.ModifierUtils
 * @since 1.0.0
 */
@RunWith(MockitoJUnitRunner.class)
public class ModifierUtilsTests {

  @Mock
  private Member mockMember;

  @Test
  public void isAbstractReturnsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.ABSTRACT);

    assertThat(ModifierUtils.isAbstract(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isAbstractHavingMultipleModifiersReturnsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.ABSTRACT);

    assertThat(ModifierUtils.isAbstract(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isAbstractReturnsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.INTERFACE);

    assertThat(ModifierUtils.isAbstract(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isFinalIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL);

    assertThat(ModifierUtils.isFinal(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isFinalHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE | Modifier.FINAL);

    assertThat(ModifierUtils.isFinal(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isFinalIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.ABSTRACT);

    assertThat(ModifierUtils.isFinal(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isInterfaceIsTrue() {
    assertThat(ModifierUtils.isInterface(Cloneable.class)).isTrue();
  }

  @Test
  public void isInterfaceHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE | Modifier.INTERFACE);

    assertThat(ModifierUtils.isInterface(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isInterfaceIsFalse() {
    assertThat(ModifierUtils.isInterface(Object.class)).isFalse();
  }

  @Test
  public void isNativeIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.NATIVE);

    assertThat(ModifierUtils.isNative(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isNativeHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.NATIVE | Modifier.ABSTRACT);

    assertThat(ModifierUtils.isNative(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isNativeIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.ABSTRACT);

    assertThat(ModifierUtils.isNative(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPackagePrivateIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.ABSTRACT);

    assertThat(ModifierUtils.isPackagePrivate(mockMember)).isTrue();

    verify(mockMember, times(3)).getModifiers();
  }

  @Test
  public void isPackagePrivateWithPublicAccessIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC);

    assertThat(ModifierUtils.isPackagePrivate(mockMember)).isFalse();

    verify(mockMember, times(3)).getModifiers();
  }

  @Test
  public void isPackagePrivateWithProtectedAccessIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PROTECTED);

    assertThat(ModifierUtils.isPackagePrivate(mockMember)).isFalse();

    verify(mockMember, times(2)).getModifiers();
  }

  @Test
  public void isPackagePrivateWithPrivateAccessIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE);

    assertThat(ModifierUtils.isPackagePrivate(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPrivateIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE);

    assertThat(ModifierUtils.isPrivate(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPrivateHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE | Modifier.ABSTRACT);

    assertThat(ModifierUtils.isPrivate(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPrivateIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.INTERFACE);

    assertThat(ModifierUtils.isPrivate(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isProtectedIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PROTECTED);

    assertThat(ModifierUtils.isProtected(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isProtectedHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PROTECTED | Modifier.ABSTRACT);

    assertThat(ModifierUtils.isProtected(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isProtectedIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE);

    assertThat(ModifierUtils.isProtected(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPublicIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC);

    assertThat(ModifierUtils.isPublic(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPublicHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.INTERFACE);

    assertThat(ModifierUtils.isPublic(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isPublicIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PRIVATE);

    assertThat(ModifierUtils.isPublic(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStaticIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.STATIC);

    assertThat(ModifierUtils.isStatic(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStaticHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.STATIC | Modifier.INTERFACE);

    assertThat(ModifierUtils.isStatic(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStaticIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.INTERFACE);

    assertThat(ModifierUtils.isStatic(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStrictIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.STRICT);

    assertThat(ModifierUtils.isStrict(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStrictHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.STRICT | Modifier.FINAL);

    assertThat(ModifierUtils.isStrict(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isStrictIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL);

    assertThat(ModifierUtils.isStrict(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isSynchronizedIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.SYNCHRONIZED);

    assertThat(ModifierUtils.isSynchronized(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isSynchronizedHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.TRANSIENT);

    assertThat(ModifierUtils.isSynchronized(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isSynchronizedIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL);

    assertThat(ModifierUtils.isSynchronized(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isTransientIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.TRANSIENT);

    assertThat(ModifierUtils.isTransient(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isTransientHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.PUBLIC | Modifier.FINAL | Modifier.TRANSIENT);

    assertThat(ModifierUtils.isTransient(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isTransientIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.STRICT);

    assertThat(ModifierUtils.isTransient(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isVolatileIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.VOLATILE);

    assertThat(ModifierUtils.isVolatile(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isVolatileHavingMultipleModifiersIsTrue() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.VOLATILE);

    assertThat(ModifierUtils.isVolatile(mockMember)).isTrue();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void isVolatileIsFalse() {
    when(mockMember.getModifiers()).thenReturn(Modifier.FINAL | Modifier.SYNCHRONIZED);

    assertThat(ModifierUtils.isVolatile(mockMember)).isFalse();

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void getModifiersForClass() {
    assertThat(ModifierUtils.getModifiers(Object.class)).isNotEqualTo(0);
  }

  @Test
  public void getModifiersForMember() {
    int modifiers = Modifier.FINAL | Modifier.SYNCHRONIZED | Modifier.VOLATILE;

    when(mockMember.getModifiers()).thenReturn(modifiers);

    assertThat(ModifierUtils.getModifiers(mockMember)).isEqualTo(modifiers);

    verify(mockMember, times(1)).getModifiers();
  }

  @Test
  public void getModifiersForNonClassNonMember() {
    assertThat(ModifierUtils.getModifiers(null)).isEqualTo(0);
  }
}
