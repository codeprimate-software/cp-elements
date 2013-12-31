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

package org.cp.elements.lang.support;

import org.cp.elements.lang.Filter;

/**
 * The FilterBuilder class is an implementation of the Builder design pattern for composing Filter objects with the help
 * of the ComposableFilter class.  This Builder class can be used in place of the ComposableFilter if the Composition
 * design pattern is less desirable.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class FilterBuilder<T> {

  private Filter<T> filterInstance;

  /**
   * Adds the specified Filter to the composition of Filters joined using the AND operator.
   * <p/>
   * @param filter the Filter to add to the composition joined using the AND operator.
   * @return this FilterBuilder instance.
   * @see org.cp.elements.lang.Filter
   */
  public FilterBuilder<T> addWithAnd(final Filter<T> filter) {
    filterInstance = ComposableFilter.and(filterInstance, filter);
    return this;
  }

  /**
   * Adds the specified Filter to the composition of Filters joined using the OR operator.
   * <p/>
   * @param filter the Filter to add to the composition joined using the OR operator.
   * @return this FilterBuilder instance.
   * @see org.cp.elements.lang.Filter
   */
  public FilterBuilder<T> addWithOr(final Filter<T> filter) {
    filterInstance = ComposableFilter.or(filterInstance, filter);
    return this;
  }

  /**
   * Builds a composition of Filters combined from the addWithAnd and addWithOr methods.
   * <p/>
   * @return a Filter composition.
   * @see org.cp.elements.lang.Filter
   */
  public Filter<T> build() {
    return filterInstance;
  }

}
