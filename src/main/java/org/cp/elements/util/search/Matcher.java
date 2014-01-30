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

import org.cp.elements.lang.Filter;

/**
 * The Matcher interface defines a contract for objects that match other objects based on criteria defined
 * by the Matcher.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.Filter
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Matcher<T> extends Filter<T> {

  /**
   * Determines whether the specified object is an exact match to the criteria defined by this Matcher.
   * <p/>
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return a boolean value indicating whether the specified object is an exact match to the criteria
   * defined by this Matcher.
   * @see #match(Object)
   */
  public boolean isMatch(T obj);

  /**
   * Determines the value of the specified object relative to the criteria defined by this Matcher.
   * <p/>
   * @param obj the Object being evaluated as a possible match to the criteria of this Matcher.
   * @return an integer indicating the object's relative value to the criteria of this Matcher.  Returns zero for an
   * exact match, a negative number if the object is undervalued, and a positive number if the object exceeds the value
   * of the criteria.
   * @see #isMatch(Object)
   */
  public int match(T obj);

}
