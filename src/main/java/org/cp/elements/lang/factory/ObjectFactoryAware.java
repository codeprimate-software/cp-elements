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
package org.cp.elements.lang.factory;

/**
 * Interface defining a contract for implementing objects requiring an instance of an {@link ObjectFactory}.
 *
 * @author John J. Blum
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
@FunctionalInterface
@SuppressWarnings("unused")
public interface ObjectFactoryAware {

  /**
   * Sets a reference to an {@link ObjectFactory}.
   *
   * @param objectFactory reference to an {@link ObjectFactory}.
   * @see org.cp.elements.lang.factory.ObjectFactory
   */
  void setObjectFactory(ObjectFactory objectFactory);

}
