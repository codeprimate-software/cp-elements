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

import java.util.Set;
import java.util.TreeSet;
import java.util.stream.IntStream;

import org.junit.Test;

import org.cp.elements.lang.VersionService;

/**
 * Unit Tests for {@link UUIDVersionService}.
 *
 * @author John Blum
 * @see java.util.concurrent.atomic.AtomicLong
 * @see org.junit.Test
 * @see org.cp.elements.lang.support.SimpleVersionService
 * @since 1.0.0
 */
public class SimpleVersionServiceUnitTests {

  private static final int COUNT = 100;

  @Test
  public void newVersionIsUnique() {

    VersionService<Long> versionService = new SimpleVersionService();

    assertThat(versionService.newVersion()).isNotNull();

    Set<Long> versionNumbers = new TreeSet<>();

    IntStream.range(0, COUNT).forEach(count -> versionNumbers.add(versionService.newVersion()));

    assertThat(versionNumbers).hasSize(COUNT);

    long previousVersionNumber = 0L;

    for (long versionNumber : versionNumbers) {
      assertThat(versionNumber).isGreaterThan(previousVersionNumber);
      previousVersionNumber = versionNumber;
    }
  }
}
