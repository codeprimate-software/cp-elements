/*
 * Copyright 2016 Author or Authors.
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

import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertThat;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.time.LocalDateTime;

import org.junit.Test;

/**
 * Test suite of test cases testing the contract and default functionality of the {@link Auditable} interface.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Auditable
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class AuditableTests {

  @Test
  public void createdByCallsSetCreatedByReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    when(mockAuditable.createdBy(anyString())).thenCallRealMethod();

    assertThat(mockAuditable.createdBy("test"), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setCreatedBy(eq("test"));
  }

  @Test
  public void createdOnCallsSetCreateOnReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);
    LocalDateTime now = LocalDateTime.now();

    when(mockAuditable.createdOn(any(LocalDateTime.class))).thenCallRealMethod();

    assertThat(mockAuditable.createdOn(now), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setCreatedOn(eq(now));
  }

  @Test
  public void createdWithCallsSetCreatedWithReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    when(mockAuditable.createdWith(anyString())).thenCallRealMethod();

    assertThat(mockAuditable.createdWith("test"), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setCreatedWith(eq("test"));
  }

  @Test
  public void modifiedByCallsSetModifiedByReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    when(mockAuditable.modifiedBy(anyString())).thenCallRealMethod();

    assertThat(mockAuditable.modifiedBy("test"), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setModifiedBy(eq("test"));
  }

  @Test
  public void modifiedOnCallsSetModifiedOnReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);
    LocalDateTime now = LocalDateTime.now();

    when(mockAuditable.modifiedOn(any(LocalDateTime.class))).thenCallRealMethod();

    assertThat(mockAuditable.modifiedOn(now), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setModifiedOn(eq(now));
  }

  @Test
  public void modifiedWithCallsSetModifiedWithReturnsThis() {
    Auditable<String, String, Long> mockAuditable = mock(AbstractAuditable.class);

    when(mockAuditable.modifiedWith(anyString())).thenCallRealMethod();

    assertThat(mockAuditable.modifiedWith("test"), is(equalTo(mockAuditable)));

    verify(mockAuditable, times(1)).setModifiedWith(eq("test"));
  }

  abstract class AbstractAuditable<USER, PROCESS, ID extends Comparable<ID>> implements Auditable<USER, PROCESS, ID> {
  }
}
