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

package org.cp.elements.beans.event;

import java.util.EventListener;

/**
 * The ChangeListener interface defines a contract for implementing classes interested in change events.
 * <p/>
 * @author John J. Blum
 * @see java.util.EventListener
 * @see org.cp.elements.beans.event.ChangeEvent
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public interface ChangeListener extends EventListener {

  /**
   * The ChangeListener callback method notifying the listener that a change event, such as a Object state transition
   * or change has occurred.
   * <p/>
   * @param event the ChangeEvent object encapsulating the state change event.
   * @see org.cp.elements.beans.event.ChangeEvent
   */
  public void stateChanged(ChangeEvent event);

}
