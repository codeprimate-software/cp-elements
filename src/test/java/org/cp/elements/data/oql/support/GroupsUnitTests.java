/*
 * Copyright 2017-Present Author or Authors.
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
package org.cp.elements.data.oql.support;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatIllegalArgumentException;
import static org.assertj.core.api.Assertions.assertThatIllegalStateException;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.isNull;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Iterator;
import java.util.Spliterator;
import java.util.stream.Stream;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Groups}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.support.Groups
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @since 2.0.0
 */
@SuppressWarnings("unchecked")
class GroupsUnitTests {

  @Test
  void noopGroupsGetGroupBy() {

    Groups<?> groups = Groups.noop();

    assertThat(groups).isNotNull();

    assertThatIllegalStateException()
      .isThrownBy(groups::getGroupBy)
      .withMessage("GroupBy not present")
      .withNoCause();
  }

  @Test
  void noopGroupsCompute() {

    Groups<Object> groups = Groups.noop();

    assertThat(groups).isNotNull();

    assertThatIllegalStateException()
      .isThrownBy(() -> groups.compute("TEST"))
      .withMessage("Cannot compute Group")
      .withNoCause();
  }

  @Test
  void noopGroupsGroup() {

    Groups<Object> groups = Groups.noop();

    assertThat(groups).isNotNull();
    assertThat(groups.group("TEST")).isEqualTo("TEST");
  }

  @Test
  void noopGroupsIterator() {

    Groups<Object> groups = Groups.noop();

    assertThat(groups).isNotNull();

    Iterator<Group<Object>> iterator = groups.iterator();

    assertThat(iterator).isNotNull();
    assertThat(iterator.hasNext()).isFalse();
  }

  @Test
  void groupCallsCompute() {

    Groups<Object> groups = mock(Groups.class);
    Group<Object> mockGroup = mock(Group.class);

    doReturn(mockGroup).when(groups).compute(eq("MOCK"));
    doCallRealMethod().when(groups).group(any());

    assertThat(groups.group("MOCK")).isEqualTo("MOCK");

    verify(groups, times(1)).group(eq("MOCK"));
    verify(groups, times(1)).compute(eq("MOCK"));
    verify(mockGroup, times(1)).include(eq("MOCK"));
    verifyNoMoreInteractions(groups, mockGroup);
  }

  @Test
  void groupNullTarget() {

    Groups<Object> groups = mock(Groups.class);

    doCallRealMethod().when(groups).group(any());

    assertThatIllegalArgumentException()
      .isThrownBy(() -> groups.group(null))
      .withMessage("Object to group is required")
      .withNoCause();

    verify(groups, times(1)).group(isNull());
    verifyNoMoreInteractions(groups);
  }

  @Test
  void streamGroups() {

    Spliterator<Object> mockSpliterator = mock(Spliterator.class);
    Groups<Object> groups = mock(Groups.class);

    doReturn(mockSpliterator).when(groups).spliterator();
    doCallRealMethod().when(groups).stream();

    Stream<Group<Object>> stream = groups.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();

    verify(groups, times(1)).stream();
    verify(groups, times(1)).spliterator();
    verifyNoMoreInteractions(groups);
  }
}
