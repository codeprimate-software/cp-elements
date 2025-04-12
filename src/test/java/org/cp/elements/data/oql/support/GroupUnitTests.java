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
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;

import org.cp.elements.data.oql.Oql;
import org.mockito.Mock;
import org.mockito.Mock.Strictness;
import org.mockito.junit.jupiter.MockitoExtension;

/**
 * Unit Tests for {@link Group}.
 *
 * @author John Blum
 * @see org.cp.elements.data.oql.support.Group
 * @see org.junit.jupiter.api.Test
 * @since 2.0.0
 */
@ExtendWith(MockitoExtension.class)
@SuppressWarnings("unchecked")
class GroupUnitTests {

  @Mock(strictness = Strictness.LENIENT)
  private Oql.GroupBy<Object, Object> mockGroupBy;

  @Mock(strictness = Strictness.LENIENT)
  private Grouping<Object> mockGrouping;

  @BeforeEach
  public void initializeGroupBy() {
    doReturn(this.mockGrouping).when(mockGroupBy).getGrouping();
  }

  @Test
  void withGroupByAndGroupNumber() {

    Group<Object> group = Group.with(this.mockGroupBy, 1);

    assertThat(group).isNotNull();
    assertThat(group.getGroupBy()).isEqualTo(this.mockGroupBy);
    assertThat(group.getGrouping()).isEqualTo(this.mockGrouping);
    assertThat(group.getNumber()).isEqualTo(1);

    verify(this.mockGroupBy, times(1)).getGrouping();
    verifyNoMoreInteractions(this.mockGroupBy);
    verifyNoInteractions(this.mockGrouping);
  }

  @Test
  void countWithTwoMembers() {

    Group<Object> group = Group.with(this.mockGroupBy, 2);

    assertThat(group).isNotNull();
    assertThat(group.getGroupBy()).isEqualTo(this.mockGroupBy);
    assertThat(group.getNumber()).isEqualTo(2);
    assertThat(group.include("TEST")).isEqualTo("TEST");
    assertThat(group.include("MOCK")).isEqualTo("MOCK");
    assertThat(group.getCount()).isEqualTo(2);

    verifyNoInteractions(this.mockGroupBy);
  }

  @Test
  void countWithOneMembers() {

    Group<Object> group = Group.with(this.mockGroupBy, 3);

    assertThat(group).isNotNull();
    assertThat(group.getGroupBy()).isEqualTo(this.mockGroupBy);
    assertThat(group.getNumber()).isEqualTo(3);
    assertThat(group.include("TEST")).isEqualTo("TEST");
    assertThat(group.getCount()).isOne();

    verifyNoInteractions(this.mockGroupBy);
  }

  @Test
  void countWithNoMembers() {

    Group<Object> group = Group.with(this.mockGroupBy, 4);

    assertThat(group).isNotNull();
    assertThat(group.getGroupBy()).isEqualTo(this.mockGroupBy);
    assertThat(group.getNumber()).isEqualTo(4);
    assertThat(group.getCount()).isZero();

    verifyNoInteractions(this.mockGroupBy);
  }

  @Test
  void isEmpty() {

    Group<Object> mockGroup = mock(Group.class);

    doReturn(0L).when(mockGroup).getCount();
    doCallRealMethod().when(mockGroup).isEmpty();

    assertThat(mockGroup.isEmpty()).isTrue();

    verify(mockGroup, times(1)).isEmpty();
    verify(mockGroup, times(1)).getCount();
    verifyNoMoreInteractions(mockGroup);
  }

  @Test
  void isNotEmpty() {

    Group<Object> mockGroup = mock(Group.class);

    doReturn(1L).when(mockGroup).getCount();
    doCallRealMethod().when(mockGroup).isEmpty();
    doCallRealMethod().when(mockGroup).isNotEmpty();

    assertThat(mockGroup.isEmpty()).isFalse();
    assertThat(mockGroup.isNotEmpty()).isTrue();

    verify(mockGroup, times(2)).isEmpty();
    verify(mockGroup, times(1)).isNotEmpty();
    verify(mockGroup, times(2)).getCount();
    verifyNoMoreInteractions(mockGroup);
  }

  @Test
  void isNotEmptyCallsIsEmpty() {

    Group<Object> mockGroup = mock(Group.class);

    doReturn(false).when(mockGroup).isEmpty();
    doCallRealMethod().when(mockGroup).isNotEmpty();

    assertThat(mockGroup.isNotEmpty()).isTrue();

    verify(mockGroup, times(1)).isNotEmpty();
    verify(mockGroup, times(1)).isEmpty();
    verifyNoMoreInteractions(mockGroup);
  }

  @Test
  void steam() {

    Group<Object> group = Group.with(this.mockGroupBy, 0);

    assertThat(group).isNotNull();
    assertThat(group.getGroupBy()).isEqualTo(this.mockGroupBy);
    assertThat(group.getNumber()).isEqualTo(0);

    Stream<Object> stream = group.stream();

    assertThat(stream).isNotNull();
    assertThat(stream).isEmpty();
  }
}
