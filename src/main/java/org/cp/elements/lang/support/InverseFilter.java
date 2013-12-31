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

import org.cp.elements.lang.Assert;
import org.cp.elements.lang.Filter;

/**
 * The InverseFilter class negates the outcome of the target Filter wrapped by an instance of this class.
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this Filter.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.cp.elements.lang.support.DefaultFilter
 * @since 1.0.0
 */
public final class InverseFilter<T> implements Filter<T> {

  private final Filter<T> filter;

  /**
   * Constructs an instance of the InverseFilter wrapping the specified Filter object in order to negate the
   * Filter's outcome.
   * <p/>
   * @param filter the Filter object being wrapped by an instance of the InverseFilter.
   */
  public InverseFilter(final Filter<T> filter) {
    Assert.notNull(filter, "The target Filter being wrapped by the InverseFilter cannot be null!");
    this.filter = filter;
  }

  /**
   * Gets the target Filter object wrapped by this InverseFilter.  This package-private method is provided for
   * testing purposes.
   * <p/>
   * @return the Filter object wrapped by this InverseFilter.
   */
  final Filter<T> getFilter() {
    return this.filter;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   * <p/>
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return !getFilter().accept(obj);
  }

}
