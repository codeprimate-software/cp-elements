/*
 * Copyright 2011-Present Author or Authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.cp.elements.lang.support;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.Lifecycle;

/**
 * The LifecyleAdapter is a abstract base class for conveniently implementing the Lifecycle interface, allowing
 * the implementations to only override those methods needed by the class.
 *
 * @author John J. Blum
 * @param <T> the type of configuration meta-data and settings.
 * @see org.cp.elements.lang.Lifecycle
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class LifecycleAdapter<T> implements Lifecycle<T> {

  /**
   * Gets the configuration meta-data of type T used to configure this object.
   *
   * @return the configuration meta-data used to configure this object.
   */
  public T getConfiguration() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object has been properly configured.  An object is configured when it's configure method
   * is invoked with configuration parameter.
   *
   * @return a boolean value indicating whether this object has been properly configured.
   */
  public boolean isConfigured() {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Configures this object with the given configuration meta-data of type T.
   *
   * @param configuration the configuration meta-data of type T used to configure this object.
   */
  public void configure(final T configuration) {
    throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
  }

  /**
   * Determines whether this object has been initialized.  This object gets initialized when it's init method
   * is invoked.
   *
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
   *
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
   *
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
   *
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
