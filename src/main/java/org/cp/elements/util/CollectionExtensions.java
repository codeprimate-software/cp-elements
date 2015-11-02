/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.util;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cp.elements.lang.DslExtension;
import org.cp.elements.lang.StringUtils;
import org.cp.elements.lang.annotation.DSL;

/**
 * The CollectionExtensions class provides methods to write natural language expressions for performing various
 * collection-oriented operations.
 *
 * @author John J. Blum
 * @see java.util.Arrays
 * @see java.util.Collections
 * @see java.util.List
 * @see java.util.Set
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class CollectionExtensions {

  /**
   * The from operator performs conversions on the given array and it's elements.  For instance, the array can be
   * converted into an ordered List of elements, or a unique Set of elements originating from the array.
   *
   * @param <T> the type of the elements in the array.
   * @param array the array of elements all of type T to be converted.
   * @return a From operator to perform the conversions.
   * @see org.cp.elements.lang.annotation.DSL
   */
  @DSL
  @SafeVarargs
  public static <T> From<T> from(final T... array) {
    return new FromExpression<>(array);
  }

  /**
   * The From interface defines operations for conversion of an Object array to a List, Set or String.
   *
   * @param <T> the element type of objects in the List or Set.
   * @see org.cp.elements.lang.DslExtension
   */
  public interface From<T> extends DslExtension {

    /**
     * Converts an object array to a List.
     *
     * @return a List implementation containing all the elements in the given object array to the from operator.
     * @see java.util.List
     */
    List<T> toList();

    /**
     * Converts an object array to a Set.
     *
     * @return a Set implementation containing all the elements of the given object array to the from operator.
     * @see java.util.Set
     */
    Set<T> toSet();

  }

  /**
   * The FromExpression class is an implementation of the From interface, from operator.
   *
   * @param <T> the element type of items in the List or Set.
   * @see org.cp.elements.util.CollectionExtensions.From
   */
  private static final class FromExpression<T> implements From<T> {

    private final T[] array;

    @SafeVarargs
    public FromExpression(final T... array) {
      this.array = array;
    }

    @SuppressWarnings("unchecked")
    public List<T> toList() {
      return (array == null ? Collections.<T>emptyList() : (array.length == 1 ? Collections.singletonList(array[0])
        : Arrays.asList(array)));
    }

    public Set<T> toSet() {
      return (array == null ? Collections.<T>emptySet() : (array.length == 1 ? Collections.singleton(array[0])
        : new HashSet<>(toList())));
    }

    @Override
    public String toString() {
      StringBuilder buffer = new StringBuilder("[");

      if (array != null) {
        for (int index = 0; index < array.length; index++) {
          buffer.append(index > 0 ? StringUtils.COMMA_SPACE_DELIMITER : StringUtils.EMPTY_STRING);
          buffer.append(array[index]);
        }
      }

      return buffer.append("]").toString();
    }
  }

}
