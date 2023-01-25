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
package org.cp.elements.lang;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;

import org.junit.Test;

import org.mockito.ArgumentMatchers;

/**
 * Unit Tests for {@link Auditable}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings({ "rawtypes", "unchecked" })
public class AuditableUnitTests {

  @Test
  public void createdByCallsSetCreatedByReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    doCallRealMethod().when(mockAuditable).createdBy(anyString());

    assertThat(mockAuditable.<Auditable>createdBy("MockUser")).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).createdBy(eq("MockUser"));
    verify(mockAuditable, times(1)).setCreatedBy(eq("MockUser"));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdOnCallsSetCreateOnReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    Instant now = Instant.now();

    doCallRealMethod().when(mockAuditable).createdOn(any(Instant.class));

    assertThat(mockAuditable.<Auditable>createdOn(now)).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).createdOn(eq(now));
    verify(mockAuditable, times(1)).setCreatedOn(eq(now));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdOnWithLocalDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).createdOn(any(LocalDateTime.class));

    LocalDateTime now = LocalDateTime.now();
    ZonedDateTime zonedNow = ZonedDateTime.of(now, ZoneOffset.systemDefault());

    mockAuditable.createdOn(now);

    verify(mockAuditable, times(1)).createdOn(eq(now));
    verify(mockAuditable, times(1)).createdOn(eq(zonedNow));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdOnWithNullLocalDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).createdOn(ArgumentMatchers.<LocalDateTime>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockAuditable.createdOn((LocalDateTime) null))
      .withMessage("createdOn LocalDateTime is required")
      .withNoCause();

    verify(mockAuditable, times(1)).createdOn(ArgumentMatchers.<LocalDateTime>isNull());
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdOnWithZonedDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).createdOn(any(ZonedDateTime.class));

    ZonedDateTime now = ZonedDateTime.now();

    mockAuditable.createdOn(now);

    verify(mockAuditable, times(1)).createdOn(eq(now));
    verify(mockAuditable, times(1)).createdOn(eq(now.toInstant()));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdOnWithNullZonedDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).createdOn(ArgumentMatchers.<ZonedDateTime>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockAuditable.createdOn((ZonedDateTime) null))
      .withMessage("createdOn ZonedDateTime is required")
      .withNoCause();

    verify(mockAuditable, times(1)).createdOn(ArgumentMatchers.<ZonedDateTime>isNull());
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void createdWithCallsSetCreatedWithReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    doCallRealMethod().when(mockAuditable).createdWith(anyString());

    assertThat(mockAuditable.<Auditable>createdWith("MockProcess")).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).createdWith(eq("MockProcess"));
    verify(mockAuditable, times(1)).setCreatedWith(eq("MockProcess"));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedByCallsSetModifiedByReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    doCallRealMethod().when(mockAuditable).modifiedBy(anyString());

    assertThat(mockAuditable.<Auditable>modifiedBy("TestUser")).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).modifiedBy(eq("TestUser"));
    verify(mockAuditable, times(1)).setModifiedBy(eq("TestUser"));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedOnCallsSetModifiedOnReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    Instant now = Instant.now();

    doCallRealMethod().when(mockAuditable).modifiedOn(any(Instant.class));

    assertThat(mockAuditable.<Auditable>modifiedOn(now)).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).modifiedOn(eq(now));
    verify(mockAuditable, times(1)).setModifiedOn(eq(now));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedOnWithLocalDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).modifiedOn(any(LocalDateTime.class));

    LocalDateTime now = LocalDateTime.now();
    ZonedDateTime zonedNow = ZonedDateTime.of(now, ZoneOffset.systemDefault());

    mockAuditable.modifiedOn(now);

    verify(mockAuditable, times(1)).modifiedOn(eq(now));
    verify(mockAuditable, times(1)).modifiedOn(eq(zonedNow));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedOnWithNullLocalDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).modifiedOn(ArgumentMatchers.<LocalDateTime>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockAuditable.modifiedOn((LocalDateTime) null))
      .withMessage("modifiedOn LocalDateTime is required")
      .withNoCause();

    verify(mockAuditable, times(1)).modifiedOn(ArgumentMatchers.<LocalDateTime>isNull());
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedOnWithZonedDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).modifiedOn(any(ZonedDateTime.class));

    ZonedDateTime now = ZonedDateTime.now();

    mockAuditable.modifiedOn(now);

    verify(mockAuditable, times(1)).modifiedOn(eq(now));
    verify(mockAuditable, times(1)).modifiedOn(eq(now.toInstant()));
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedOnWithNullZonedDateTime() {

    Auditable<String, String, Long> mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditable).modifiedOn(ArgumentMatchers.<ZonedDateTime>any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> mockAuditable.modifiedOn((ZonedDateTime) null))
      .withMessage("modifiedOn ZonedDateTime is required")
      .withNoCause();

    verify(mockAuditable, times(1)).modifiedOn(ArgumentMatchers.<ZonedDateTime>isNull());
    verifyNoMoreInteractions(mockAuditable);
  }

  @Test
  public void modifiedWithCallsSetModifiedWithReturnsThis() {

    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    doCallRealMethod().when(mockAuditable).modifiedWith(anyString());

    assertThat(mockAuditable.<Auditable>modifiedWith("TestProcess")).isEqualTo(mockAuditable);

    verify(mockAuditable, times(1)).modifiedWith(eq("TestProcess"));
    verify(mockAuditable, times(1)).setModifiedWith(eq("TestProcess"));
    verifyNoMoreInteractions(mockAuditable);
  }

  static abstract class AbstractAuditable<USER, PROCESS, ID extends Comparable<ID>>
    implements Auditable<USER, PROCESS, ID> { }

}
