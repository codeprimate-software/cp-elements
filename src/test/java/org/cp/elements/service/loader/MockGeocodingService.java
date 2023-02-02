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

import java.awt.Point;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.Constants;
import org.cp.elements.service.annotation.Service;

/**
 * {@link Service} implementation for geocoding.
 *
 * @author John Blum
 * @see java.awt.Point
 * @see java.lang.FunctionalInterface
 * @see org.cp.elements.service.annotation.Service
 * @see org.cp.elements.service.loader.ServiceLoaderSupport
 * @since 1.0.1
 */
@Service
@FunctionalInterface
@SuppressWarnings("unused")
public interface MockGeocodingService {

  AtomicReference<Loader> LOADER = new AtomicReference<>();

  static Loader getLoader() {
    return LOADER.updateAndGet(it -> it != null ? it : new Loader() {});
  }

  Point geocode(String address);

  default String reverseGeocode(Point coordinates) {
    throw new UnsupportedOperationException(Constants.OPERATION_NOT_SUPPORTED);
  }

  interface Loader extends ServiceLoaderSupport<MockGeocodingService> {

    @Override
    default Class<MockGeocodingService> getType() {
      return MockGeocodingService.class;
    }
  }
}
