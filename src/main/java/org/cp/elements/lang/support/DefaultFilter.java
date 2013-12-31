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
 * The DefaultFilter class is a Filter implementation that allows the user to pre-define the outcome of the Filter's
 * evaluation (as determined by the accept method).
 * <p/>
 * @author John J. Blum
 * @param <T> the Class type of Objects evaluated by this Filter.
 * @see org.cp.elements.lang.Filter
 * @see org.cp.elements.lang.support.ComposableFilter
 * @see org.cp.elements.lang.support.InverseFilter
 * @since 1.0.0
 */
public final class DefaultFilter<T> implements Filter<T> {

  public static final boolean DEFAULT_ACCEPT_RESULT = true;

  private final boolean acceptResult;

  /**
   * Constructs an instance of the DefaultFilter class with the default accept result of true.
   */
  public DefaultFilter() {
    this(DEFAULT_ACCEPT_RESULT);
  }

  /**
   * Constructs an instance of the DefaultFilter class with the given boolean value for the accept's methods result.
   * <p/>
   * @param acceptResult a boolean value defining the result of the accept method.
   */
  public DefaultFilter(final boolean acceptResult) {
    this.acceptResult = acceptResult;
  }

  /**
   * Determines the result of calling the accept method on any type of object.
   * <p/>
   * @return a boolean value indicating the default return result for the accept method.
   */
  final boolean isAccepting() {
    return this.acceptResult;
  }

  /**
   * Determines whether the specified object meets the criteria (rules) defined by this Filter.
   * <p/>
   * @param obj the Object being evaluated by this Filter.
   * @return a boolean value indicating whether the specified Object satisfies the criteria (rules) of this Filter.
   */
  public boolean accept(final T obj) {
    return acceptResult;
  }

}
