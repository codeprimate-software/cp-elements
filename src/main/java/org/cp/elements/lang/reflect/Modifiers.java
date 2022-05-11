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

import java.lang.reflect.Modifier;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;
import org.cp.elements.lang.annotation.Nullable;

/**
 * An {@link Enum enumeration} of Java {@link Modifier Modifiers}.
 *
 * @author John Blum
 * @see java.lang.reflect.Modifier
 * @see java.util.function.Predicate
 * @since 1.0.0
 */
public enum Modifiers implements Predicate<Object> {

  ABSTRACT(Modifier.ABSTRACT),
  FINAL(Modifier.FINAL),
  INTERFACE(Modifier.INTERFACE),
  NATIVE(Modifier.NATIVE),
  PRIVATE(Modifier.PRIVATE),
  PROTECTED(Modifier.PROTECTED),
  PUBLIC(Modifier.PUBLIC),
  STATIC(Modifier.STATIC),
  STRICT(Modifier.STRICT),
  SYNCHRONIZED(Modifier.SYNCHRONIZED),
  TRANSIENT(Modifier.TRANSIENT),
  VOLATILE(Modifier.VOLATILE);

  /**
   * Factory method used to get the {@link Modifiers} enumerated value from the given Java {@link Modifier}.
   *
   * @param modifier {@link Integer} value specifying the Java {@link Modifier}.
   * @return a {@link Modifiers} enumerated value for the given Java {@link Modifier} or {@literal null}
   * if the Java {@link Modifier} is not represented as an enumerated value.
   * @see java.lang.reflect.Modifier
   */
  public static @Nullable Modifiers from(int modifier) {

    for (Modifiers modifierEnum : values()) {
      if (modifierEnum.getModifier() == modifier) {
        return modifierEnum;
      }
    }

    return null;
  }

  /**
   * Factory method used to compute the Java {@link Modifier Modifiers} from the given
   *
   * @param target {@link Object} to evaluate.
   * @return a {@link Set} of enumerated {@link Modifiers} values representing the Java {@link Modifier Modifiers}
   * declared on the given {@link Object}.
   */
  @NullSafe
  public static @NotNull Set<Modifiers> modifiersFrom(@Nullable Object target) {

    Set<Modifiers> modifiers = new HashSet<>();

    int targetModifiers = ModifierUtils.getModifiers(target);

    for (Modifiers modifier : values()) {
      if (isModifierPresentIn(modifier, targetModifiers)) {
        modifiers.add(modifier);
      }
    }

    return modifiers;
  }

  private static boolean isModifierPresentIn(@NotNull Modifiers modifier, int targetModifiers) {
    int enumeratedModifier = modifier.getModifier();
    return (enumeratedModifier & targetModifiers) == enumeratedModifier;
  }

  private final int modifier;

  Modifiers(int modifier) {
    this.modifier = modifier;
  }

  /**
   * Gets an {@link Integer} value representing the Java {@link Modifier}.
   *
   * @return an {@link Integer} value representing the Java {@link Modifier}.
   */
  public int getModifier() {
    return this.modifier;
  }

  /**
   * {@link Predicate} test used to evaluate whether this {@link Modifiers} enumerated value is present
   * in some given {@link Object}.
   *
   * @param target {@link Object} to evaluate;
   * @return the results of the {@link Predicate#test(Object)} used to determine whether this {@link Modifiers}
   * enumerated value is present in the given {@link Object}.
   * @see java.util.function.Predicate
   */
  @NullSafe
  @Override
  public boolean test(@Nullable Object target) {
    return isModifierPresentIn(this, ModifierUtils.getModifiers(target));
  }

  /**
   * Returns a {@link String} representation of this Java {@link Modifier}.
   *
   * @return a {@link String} describing this Java {@link Modifier}.
   * @see java.lang.Object#toString()
   * @see #name()
   */
  @Override
  public String toString() {
    return name().toLowerCase();
  }
}
