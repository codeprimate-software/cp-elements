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
package org.cp.elements.beans.event;

import static org.assertj.core.api.Assertions.assertThat;

import java.time.Instant;
import java.time.ZoneOffset;
import java.time.temporal.ChronoUnit;

import org.junit.Test;

/**
 * Unit Tests for {@link ChangeEvent}.
 *
 * @author John J. Blum
 * @see java.time.Instant
 * @see org.junit.Test
 * @see org.cp.elements.beans.event.ChangeEvent
 */
public class ChangeEventUnitTests {

  @Test
  public void constructChangeEvent() {

    Object expectedSource = new Object();

    ChangeEvent event = new ChangeEvent(expectedSource);

    assertThat(event).isNotNull();
    assertThat(event.getSource()).isSameAs(expectedSource);
    assertThat(event.getChangeDateTime()).isBeforeOrEqualTo(Instant.now());
    assertThat(event.getChangeDateTime()).isEqualTo(event.getChangeDateTime());
  }

  @Test(expected = IllegalArgumentException.class)
  public void constructChangeEventWithNullSource() {
    new ChangeEvent(null);
  }

  @Test
  public void changeDateTimeIsImmutable() {

    ChangeEvent event = new ChangeEvent(new Object());

    assertThat(event).isNotNull();
    assertThat(event).isNotNull();

    Instant changeDateTime = event.getChangeDateTime();

    assertThat(changeDateTime).isNotNull();
    assertThat(changeDateTime).isBeforeOrEqualTo(Instant.now());

    changeDateTime.minus(5, ChronoUnit.YEARS);

    assertThat(changeDateTime.atZone(ZoneOffset.systemDefault()).getYear())
      .isEqualTo(Instant.now().atZone(ZoneOffset.systemDefault()).getYear());
    assertThat(event.getChangeDateTime()).isNotEqualTo(changeDateTime);
  }
}
