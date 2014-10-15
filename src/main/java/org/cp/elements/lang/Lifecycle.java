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

package org.cp.elements.lang;

/**
 * The Lifecycle interface defines a contract for objects that live out a cycle of events from instantiation to
 * configuration and initialization, transitioning to a running state and finally reaching its end-of-life
 * and being consumed, or destroyed.
 * <p/>
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @see org.cp.elements.lang.Configurable
 * @see org.cp.elements.lang.Initable
 * @see java.lang.Runnable
 * @see org.cp.elements.lang.Interruptable
 * @see org.cp.elements.lang.Destroyable
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Lifecycle<T> extends Configurable<T>, Initable, Runnable, Interruptable, Destroyable {

  /**
   * Determines whether this object is currently executing.
   * <p/>
   * @return a boolean value indicating whether this object is running.
   * @see java.lang.Runnable
   */
  boolean isRunning();

}
