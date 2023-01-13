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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isA;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.spy;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.withSettings;

import java.time.Instant;
import java.time.Month;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.function.Supplier;

import org.junit.Test;

import org.mockito.ArgumentMatchers;
import org.mockito.quality.Strictness;

/**
 * Unit Tests for {@link Auditor}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Auditable
 * @see org.cp.elements.lang.Auditor
 * @since 1.0.0
 */
@SuppressWarnings("unchecked")
public class AuditorUnitTests {

  private Instant toInstant(int year, Month month, int day, int hour, int minute, int second) {
    return ZonedDateTime.of(year, month.getValue(), day, hour, minute, second,
      0, ZoneId.systemDefault()).toInstant();
  }

  @Test
  public void auditAuditsAuditableObjectBySettingAllAuditProperties() {

    Instant dateTime = toInstant(2022, Month.JANUARY, 4, 16, 30, 15);

    Object processName = AuditorUnitTests.class.getName();
    Object username = "root";

    Supplier<Instant> dateTimeSupplier = () -> dateTime;
    Supplier<Object> processNameSupplier = () -> processName;
    Supplier<Object> usernameSupplier = () -> username;

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    Auditable<Object, Object, Integer> mockAuditable = mock(Auditable.class);

    doReturn(true).when(mockAuditable).isNew();
    doReturn(true).when(mockAuditable).isModified();
    doReturn(dateTimeSupplier).when(mockAuditor).getDateTime();
    doReturn(processNameSupplier).when(mockAuditor).getProcess();
    doReturn(usernameSupplier).when(mockAuditor).getUser();
    doCallRealMethod().when(mockAuditor).audit(isA(Auditable.class));
    doCallRealMethod().when(mockAuditor).requireUser(any());

    assertThat(mockAuditor.audit(mockAuditable)).isEqualTo(mockAuditable);

    verify(mockAuditor, times(1)).audit(eq(mockAuditable));
    verify(mockAuditor, times(1)).getDateTime();
    verify(mockAuditor, times(1)).getProcess();
    verify(mockAuditor, times(1)).getUser();
    verify(mockAuditor, times(1)).requireUser(eq(username));
    verify(mockAuditor, never()).isCreatedPropertiesUnset(any());
    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, times(1)).setCreatedBy(eq(username));
    verify(mockAuditable, times(1)).setCreatedOn(eq(dateTime));
    verify(mockAuditable, times(1)).setCreatedWith(eq(processName));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, times(1)).setModifiedBy(eq(username));
    verify(mockAuditable, times(1)).setModifiedOn(eq(dateTime));
    verify(mockAuditable, times(1)).setModifiedWith(eq(processName));
    verifyNoMoreInteractions(mockAuditable, mockAuditor);
  }

  @Test
  public void auditSetsCreatedPropertiesWhenTheAuditableIsNotNewAndNotModified() {

    Instant dateTime = toInstant(2022, Month.JANUARY, 4, 17, 0, 45);

    Object processName = AuditorUnitTests.class.getName();
    Object username = "jblum";

    Supplier<Instant> dateTimeSupplier = () -> dateTime;
    Supplier<Object> processNameSupplier = () -> processName;
    Supplier<Object> usernameSupplier = () -> username;

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    Auditable<Object, Object, Integer> mockAuditable = mock(Auditable.class);

    doReturn(false).when(mockAuditable).isNew();
    doReturn(false).when(mockAuditable).isModified();
    doReturn("root").when(mockAuditable).getCreatedBy();
    doReturn(dateTimeSupplier).when(mockAuditor).getDateTime();
    doReturn(processNameSupplier).when(mockAuditor).getProcess();
    doReturn(usernameSupplier).when(mockAuditor).getUser();
    doCallRealMethod().when(mockAuditor).audit(isA(Auditable.class));
    doCallRealMethod().when(mockAuditor).isCreatedPropertiesUnset(eq(mockAuditable));
    doCallRealMethod().when(mockAuditor).requireUser(any());

    assertThat(mockAuditor.audit(mockAuditable)).isEqualTo(mockAuditable);

    verify(mockAuditor, times(1)).audit(eq(mockAuditable));
    verify(mockAuditor, times(1)).getDateTime();
    verify(mockAuditor, times(1)).getProcess();
    verify(mockAuditor, times(1)).getUser();
    verify(mockAuditor, times(1)).requireUser(eq(username));
    verify(mockAuditor, times(1)).isCreatedPropertiesUnset(eq(mockAuditable));
    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(1)).getCreatedOn();
    verify(mockAuditable, times(1)).setCreatedBy(eq(username));
    verify(mockAuditable, times(1)).setCreatedOn(eq(dateTime));
    verify(mockAuditable, times(1)).setCreatedWith(eq(processName));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, never()).setModifiedBy(eq(username));
    verify(mockAuditable, never()).setModifiedOn(eq(dateTime));
    verify(mockAuditable, never()).setModifiedWith(eq(processName));
    verifyNoMoreInteractions(mockAuditable, mockAuditor);
  }

  @Test
  public void auditSetsNoProperties() {

    Instant old = toInstant(2021, Month.JANUARY, 1, 0, 0, 0);
    Instant dateTime = toInstant(2022, Month.JANUARY, 5, 17, 1, 45);

    Object processName = AuditorUnitTests.class.getName();
    Object username = "jblum";

    Supplier<Instant> dateTimeSupplier = () -> dateTime;
    Supplier<Object> processNameSupplier = () -> processName;
    Supplier<Object> usernameSupplier = () -> username;

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    Auditable<Object, Object, Integer> mockAuditable = mock(Auditable.class);

    doReturn(false).when(mockAuditable).isNew();
    doReturn(false).when(mockAuditable).isModified();
    doReturn("root").when(mockAuditable).getCreatedBy();
    doReturn(old).when(mockAuditable).getCreatedOn();
    doReturn(dateTimeSupplier).when(mockAuditor).getDateTime();
    doReturn(processNameSupplier).when(mockAuditor).getProcess();
    doReturn(usernameSupplier).when(mockAuditor).getUser();
    doCallRealMethod().when(mockAuditor).audit(isA(Auditable.class));
    doCallRealMethod().when(mockAuditor).isCreatedPropertiesUnset(eq(mockAuditable));
    doCallRealMethod().when(mockAuditor).requireUser(any());

    assertThat(mockAuditor.audit(mockAuditable)).isEqualTo(mockAuditable);

    verify(mockAuditor, times(1)).audit(eq(mockAuditable));
    verify(mockAuditor, times(1)).getDateTime();
    verify(mockAuditor, times(1)).getProcess();
    verify(mockAuditor, times(1)).getUser();
    verify(mockAuditor, times(1)).requireUser(eq(username));
    verify(mockAuditor, times(1)).isCreatedPropertiesUnset(eq(mockAuditable));
    verify(mockAuditable, times(1)).isNew();
    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(1)).getCreatedOn();
    verify(mockAuditable, never()).setCreatedBy(any());
    verify(mockAuditable, never()).setCreatedOn(any());
    verify(mockAuditable, never()).setCreatedWith(any());
    verify(mockAuditable, never()).setModifiedBy(eq(username));
    verify(mockAuditable, times(1)).isModified();
    verify(mockAuditable, never()).setModifiedOn(eq(dateTime));
    verify(mockAuditable, never()).setModifiedWith(eq(processName));
    verifyNoMoreInteractions(mockAuditable, mockAuditor);
  }

  @Test
  public void auditUntypedAuditable() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    Object mockAuditable = mock(Auditable.class);

    doCallRealMethod().when(mockAuditor).audit(isA(Object.class));

    mockAuditor.audit(mockAuditable);

    verify(mockAuditor, times(1)).audit(isA(Object.class));
    verify(mockAuditor, times(1)).audit(isA(Auditable.class));
    verifyNoMoreInteractions(mockAuditor);
    verifyNoInteractions(mockAuditable);
  }

  @Test
  public void auditObject() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).audit(isA(Object.class));

    assertThat(mockAuditor.audit("test")).isEqualTo("test");

    verify(mockAuditor, times(1)).audit(isA(Object.class));
    verify(mockAuditor, never()).audit(isA(Auditable.class));
  }

  @Test
  public void auditNull() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).audit(isA(Object.class));

    assertThat(mockAuditor.audit((Object) null)).isNull();

    verify(mockAuditor, times(1)).audit(ArgumentMatchers.<Object>isNull());
    verify(mockAuditor, never()).audit(isA(Auditable.class));
  }

  @Test(expected = IllegalArgumentException.class)
  public void auditNullAuditable() {

    Auditor<Object, Object> mockAuditor = spy(new Auditor<Object, Object>() { });

    try {
      mockAuditor.audit((Auditable<Object, Object, Integer>) null);
    }
    catch (IllegalArgumentException expected) {

      assertThat(expected).hasMessage("The Auditable object to audit is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockAuditor, times(1)).audit(isNull());
      verifyNoMoreInteractions(mockAuditor);
    }
  }

  @Test
  public void getDateTimeIsCorrect() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).getDateTime();

    Supplier<Instant> dateTimeSupplier = mockAuditor.getDateTime();

    assertThat(dateTimeSupplier).isNotNull();

    Instant dateTime = dateTimeSupplier.get();

    assertThat(dateTime).isNotNull();
    assertThat(dateTime).isBeforeOrEqualTo(Instant.now());

    verify(mockAuditor, times(1)).getDateTime();
    verifyNoMoreInteractions(mockAuditor);
  }

  @Test
  public void getProcessIsCorrect() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).getProcess();

    Supplier<?> processSupplier = mockAuditor.getProcess();

    assertThat(processSupplier).isNotNull();

    Object process = processSupplier.get();

    assertThat(process).isNull();

    verify(mockAuditor, times(1)).getProcess();
    verifyNoMoreInteractions(mockAuditor);
  }

  @Test
  public void getUserIsCorrect() {

    Auditor<?, ?> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).getUser();

    Supplier<?> userSupplier = mockAuditor.getUser();

    assertThat(userSupplier).isNotNull();

    Object user = userSupplier.get();

    assertThat(user).isNotNull();
    assertThat(user).asString().isNotEmpty();
    assertThat(user).isEqualTo(System.getProperty("user.name"));

    verify(mockAuditor, times(1)).getUser();
    verifyNoMoreInteractions(mockAuditor);
  }

  private <USER, PROCESS> void testIsCreatedPropertiesUnsetWhenCreatedPropertiesAreNotSetReturnsTrue(
      USER user, Instant dateTime) {

    Auditable<USER, PROCESS, Integer> mockAuditable =
      mock(Auditable.class, withSettings().strictness(Strictness.LENIENT));

    doReturn(user).when(mockAuditable).getCreatedBy();
    doReturn(dateTime).when(mockAuditable).getCreatedOn();
    doReturn(AuditorUnitTests.class.getName()).when(mockAuditable).getCreatedWith();

    Auditor<USER, PROCESS> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).isCreatedPropertiesUnset(any());

    assertThat(mockAuditor.isCreatedPropertiesUnset(mockAuditable)).isTrue();

    verify(mockAuditor, times(1)).isCreatedPropertiesUnset(eq(mockAuditable));
    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(user != null ? 1 : 0)).getCreatedOn();
    verifyNoMoreInteractions(mockAuditable, mockAuditor);
  }

  @Test
  public void isCreatedPropertiesUnsetWhenCreatedByAndCreatedOnPropertiesAreNotSet() {
    testIsCreatedPropertiesUnsetWhenCreatedPropertiesAreNotSetReturnsTrue(null, null);
  }

  @Test
  public void isCreatedPropertiesUnsetWhenOnlyCreatedByIsSet() {
    testIsCreatedPropertiesUnsetWhenCreatedPropertiesAreNotSetReturnsTrue("root", null);
  }

  @Test
  public void isCreatedPropertiesUnsetWhenOnlyCreatedOnIsSet() {
    testIsCreatedPropertiesUnsetWhenCreatedPropertiesAreNotSetReturnsTrue(null, Instant.now());
  }

  @Test
  public void isCreatedPropertiesUnsetWhenCreatedByAndCreatedOnAreSetReturnsFalse() {

    Auditable<Object, Object, Integer> mockAuditable =
      mock(Auditable.class, withSettings().strictness(Strictness.LENIENT));

    doReturn("root").when(mockAuditable).getCreatedBy();
    doReturn(Instant.now()).when(mockAuditable).getCreatedOn();
    doReturn(AuditorUnitTests.class.getName()).when(mockAuditable).getCreatedWith();

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).isCreatedPropertiesUnset(any());

    assertThat(mockAuditor.isCreatedPropertiesUnset(mockAuditable)).isFalse();

    verify(mockAuditor, times(1)).isCreatedPropertiesUnset(eq(mockAuditable));
    verify(mockAuditable, times(1)).getCreatedBy();
    verify(mockAuditable, times(1)).getCreatedOn();
    verifyNoMoreInteractions(mockAuditable, mockAuditor);
  }

  @Test
  public void requireUserWithUser() {

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).requireUser(any());

    assertThat(mockAuditor.requireUser("root")).isEqualTo("root");

    verify(mockAuditor, times(1)).requireUser(eq("root"));
    verifyNoMoreInteractions(mockAuditor);
  }

  @Test(expected = IllegalStateException.class)
  public void requireUserWithNull() {

    Auditor<Object, Object> mockAuditor = mock(Auditor.class);

    doCallRealMethod().when(mockAuditor).requireUser(any());

    try {
      mockAuditor.requireUser(null);
    }
    catch (IllegalStateException expected) {

      assertThat(expected).hasMessage("User is required");
      assertThat(expected).hasNoCause();

      throw expected;
    }
    finally {
      verify(mockAuditor, times(1)).requireUser(isNull());
      verifyNoMoreInteractions(mockAuditor);
    }
  }
}
