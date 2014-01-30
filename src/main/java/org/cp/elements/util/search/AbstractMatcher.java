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

package org.cp.elements.util.search;

/**
 * The AbstractMatcher class is abstract base class encapsulating functionality common to all Matcher implementations.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.util.search.Matcher
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class AbstractMatcher<T> implements Matcher<T> {

  /**
   * Determines whether the specified object is "accepted", or matched by the criteria defined by this Matcher.
   * <p/>
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is "accepted", or matched by the criteria
   * defined by this Matcher.
   * @see #isMatch(Object)
   */
  @Override
  public boolean accept(final T obj) {
    return isMatch(obj);
  }

  /**
   * Determines whether the specified object is an exact match to the criteria defined by this Matcher.
   * <p/>
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is an exact match to the criteria
   * defined by this Matcher.
   * @see #match(Object)
   */
  @Override
  public boolean isMatch(final T obj) {
    return (match(obj) == 0);
  }

}
