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
package org.cp.elements.service.loader;

import java.util.ServiceLoader;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.service.annotation.Service;

/**
 * Mock {@link Service} class used for {@link ServiceLoader#load(Class, ClassLoader) service loading}
 * and testing purposes.
 *
 * @author John Blum
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.loader.ServiceLoaderSupport
 * @since 1.0.0
 */
@Service
public interface MockService extends ServiceLoaderSupport<MockService> {

  static @NotNull MockService getInstance() {
    return new MockService() {
      @Override
      public Class<MockService> getType() {
        return MockService.class;
      }
    };
  }
}
