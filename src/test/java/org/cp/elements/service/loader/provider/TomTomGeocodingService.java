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

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.Nullable;
import org.cp.elements.lang.annotation.Qualifier;
import org.cp.elements.service.loader.MockGeocodingService;

/**
 * {@link MockGeocodingService} service provider implementation (SPI) using TomTom.
 *
 * @author John Blum
 * @see org.cp.elements.lang.annotation.Qualifier
 * @see org.cp.elements.service.loader.MockGeocodingService
 * @since 1.0.1
 */
@Qualifier(name = "TomTom")
@SuppressWarnings("unused")
public class TomTomGeocodingService implements MockGeocodingService {

  @Override
  public @NotNull Point geocode(@Nullable String address) {
    return new Point(16, 64);
  }

  @Override
  public @NotNull String reverseGeocode(@Nullable Point coordinates) {
    return "1024 One Way Portland, OR 97205";
  }
}
