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

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Instant;
import java.time.temporal.ChronoUnit;
import java.util.Set;
import java.util.TreeSet;
import java.util.stream.IntStream;

import org.junit.Test;

import org.cp.elements.lang.VersionService;

/**
 * Unit Tests for {@link TimeBasedVersionService}.
 *
 * @author John Blum
 * @see java.time.Instant
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.TimeBasedVersionService
 * @since 1.0.0
 */
public class TimeBasedVersionServiceUnitTests {

  private static final int COUNT = 100;

  @Test
  public void newVersionIsUnique() {

    VersionService<Instant> versionService = new TimeBasedVersionService();

    assertThat(versionService.newVersion()).isNotNull();

    Set<Instant> versionNumbers = new TreeSet<>();

    IntStream.range(0, COUNT).forEach(count -> versionNumbers.add(versionService.newVersion()));

    assertThat(versionNumbers).hasSize(COUNT);

    Instant previousVersionNumber = Instant.now().minus(1, ChronoUnit.DAYS);

    for (Instant versionNumber : versionNumbers) {
      assertThat(versionNumber).isAfter(previousVersionNumber);
      previousVersionNumber = versionNumber;
    }
  }
}
