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
package org.cp.elements.service.loader.provider;

import java.awt.Point;

import org.cp.elements.lang.Nameable;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.service.loader.MockGeocodingService;

/**
 * {@link MockGeocodingService} service provider implementations (SPI) using Apple Maps.
 *
 * @author John Blum
 * @see org.cp.elements.lang.Nameable
 * @see org.cp.elements.service.loader.MockGeocodingService
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public class AppleMapsGeocodingService implements MockGeocodingService, Nameable<String> {

  @Override
  public String getName() {
    return "Apple Maps";
  }

  @Override
  public @NotNull Point geocode(@Nullable String address) {
    return new Point(256, 1024);
  }

  @Override
  public @NotNull String reverseGeocode(@Nullable Point coordinates) {
    return "2 Cents Ave. San Francisco, CA 90210";
  }
}
