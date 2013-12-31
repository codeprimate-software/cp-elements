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

import org.cp.elements.lang.Renderer;

/**
 * The ToStringRenderer class is an implementation of the Renderer interface that calls Object.toString on the
 * specified Object to render.
 * <p/>
 * @author John J. Blum
 * @param <T> the class type of the object to render.
 * @see java.lang.String
 * @see org.cp.elements.lang.Renderer
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class ToStringRenderer<T> implements Renderer<T> {

  /**
   * Renders the specified Object using the Object.toString method.
   * <p/>
   * @param obj the Object to render as a String.
   * @return a String representation of the specified Object.
   * @see java.lang.String#valueOf(Object)
   */
  @Override
  public String render(final T obj) {
    return String.valueOf(obj);
  }

}
