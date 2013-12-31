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
 * The Interruptable interface defines a contract for objects that perform some complex or long running computation
 * that can be interrupted while processing.
 * <p/>
 * @author John J. Blum
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface Interruptable {

  /**
   * Determines whether the object who's class implements this interface has been interrupted through an invocation
   * of the interrupt method.
   * <p/>
   * @return a boolean value indicating whether this object was interrupted.
   */
  public boolean isInterrupted();

  /**
   * Interrupts the object who's class implements this interface.  The interrupt could be issued from another Thread
   * while this object is performing a complex, long running and intensive computation.
   */
  public void interrupt();

}
