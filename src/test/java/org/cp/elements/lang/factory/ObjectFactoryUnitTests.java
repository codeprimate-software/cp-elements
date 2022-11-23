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
package org.cp.elements.lang.factory;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.doCallRealMethod;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import org.junit.Test;

import lombok.EqualsAndHashCode;
import lombok.Getter;

/**
 * Unit Tests for {@link ObjectFactory}.
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.factory.ObjectFactory
 * @since 1.0.0
 */
public class ObjectFactoryUnitTests {

  @Test
  @SuppressWarnings("unchecked")
  public void createWithClassTypeAndArguments() {

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);

    doCallRealMethod().when(mockObjectFactory).create(any(Class.class), any());

    doAnswer(invocation -> User.as(invocation.getArgument(2, String.class)))
      .when(mockObjectFactory).create(eq(User.class), any(Class[].class), any());

    assertThat(mockObjectFactory.create(User.class, "bobDoe")).isEqualTo(User.as("bobDoe"));

    verify(mockObjectFactory, times(1)).create(eq(User.class), eq("bobDoe" ));
    verify(mockObjectFactory, times(1))
      .create(eq(User.class), eq(new Class[] { String.class }), eq("bobDoe"));

    verifyNoMoreInteractions(mockObjectFactory);
  }

  @Test
  public void createWithFullyQualifiedClassAndArguments() {

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);

    doCallRealMethod().when(mockObjectFactory).create(anyString(), any());
    doAnswer(invocation -> User.as(invocation.getArgument(2, String.class)))
      .when(mockObjectFactory).create(eq(User.class), any(Class[].class), any());

    assertThat(mockObjectFactory.<User>create(User.class.getName(), "jonDoe")).isEqualTo(User.as("jonDoe"));

    verify(mockObjectFactory, times(1)).create(eq(User.class.getName()), eq("jonDoe"));
    verify(mockObjectFactory, times(1))
      .create(eq(User.class), eq(new Class[] { String.class }), eq("jonDoe"));

    verifyNoMoreInteractions(mockObjectFactory);
  }

  @Test
  public void createWithFullyQualifiedClassNameParameterTypesAndArguments() {

    ObjectFactory mockObjectFactory = mock(ObjectFactory.class);

    doCallRealMethod().when(mockObjectFactory).create(anyString(), any(Class[].class), any());
    doAnswer(invocation -> User.as(invocation.getArgument(2, String.class)))
      .when(mockObjectFactory).create(eq(User.class), any(Class[].class), any());

    assertThat(mockObjectFactory.<User>create(User.class.getName(), new Class[] { String.class }, "janeDoe"))
      .isEqualTo(User.as("janeDoe"));

    verify(mockObjectFactory, times(1))
      .create(eq(User.class.getName()), eq(new Class[] { String.class }), eq("janeDoe"));
    verify(mockObjectFactory, times(1))
      .create(eq(User.class), eq(new Class[] { String.class }), eq("janeDoe"));

    verifyNoMoreInteractions(mockObjectFactory);
  }

  @Getter
  @EqualsAndHashCode
  static class User {

    static User as(String name) {
      return new User(name);
    }

    private final String name;

    User(String name) {
      this.name = name;
    }

    /**
     * @inheritDoc
     */
    @Override
    public String toString() {
      return getName();
    }
  }
}
