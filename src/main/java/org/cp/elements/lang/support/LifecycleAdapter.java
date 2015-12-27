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

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Lifecycle;

/**
 * The LifecyleAdapter is a abstract base class for conveniently implementing the Lifecycle interface, allowing
 * the implementations to only override those methods needed by the class.
 * <p/>
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @see org.cp.elements.lang.Lifecycle
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LifecycleAdapter<T> implements Lifecycle<T> {

  /**
   * Gets the configuration meta-data of type T used to configure this object.
   * <p/>
   * @return the configuration meta-data used to configure this object.
   */
  public T getConfiguration() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object has been properly configured.  An object is configured when it's configure method
   * is invoked with configuration parameter.
   * <p/>
   * @return a boolean value indicating whether this object has been properly configured.
   */
  public boolean isConfigured() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Configures this object with the given configuration meta-data of type T.
   * <p/>
   * @param configuration the configuration meta-data of type T used to configure this object.
   */
  public void configure(final T configuration) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object has been initialized.  This object gets initialized when it's init method
   * is invoked.
   * <p/>
   * @return a boolean value indicating whether this object has been initialized or not.
   */
  public boolean isInitialized() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Initializes this object and prepares any required resources.
   */
  public void init() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object is currently executing.
   * <p/>
   * @return a boolean value indicating whether this object is running or not.
   */
  public boolean isRunning() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Runs the implementing objects main logic.
   */
  public void run() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether the object who's class implements this interface has been interrupted through an invocation
   * of the interrupt method.
   * <p/>
   * @return a boolean value indicating whether this object was interrupted.
   */
  public boolean isInterrupted() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Interrupts the object who's class implements this interface.  The interrupt could be issues from another Thread
   * while this object is performing a complex, long running and intensive computation.
   */
  public void interrupt() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object has been destroyed.  This object is destroyed when it's destroy method
   * has been invoked.
   * <p/>
   * @return a boolean value indicating whether this object has been destroyed or not.
   */
  public boolean isDestroyed() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Destroys this object and releases any and all resources held.
   */
  public void destroy() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

}
