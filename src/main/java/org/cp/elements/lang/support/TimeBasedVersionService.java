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

import java.time.Instant;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicReference;

import org.cp.elements.lang.VersionService;
import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.concurrent.ThreadUtils;

/**
 * {@link VersionService} implementation based on {@link Instant time}.
 *
 * @author John Blum
 * @see java.time.Instant
 * @see org.cp.elements.lang.VersionService
 * @since 1.0.0
 */
public class TimeBasedVersionService implements VersionService<Instant> {

  private final AtomicReference<Instant> versionNumber = new AtomicReference<>(Instant.now());

  private final Object lock = this;

  @Override
  public @NotNull Instant newVersion() {

    return this.versionNumber.updateAndGet(currentVersionNumber -> {

      Instant newVersionNumber = Instant.now();

      while (!newVersionNumber.isAfter(currentVersionNumber)) {

        synchronized (this.lock) {
          ThreadUtils.waitFor(1L, TimeUnit.NANOSECONDS);
        }

        newVersionNumber = Instant.now();
      }

      return newVersionNumber;
    });
  }
}
