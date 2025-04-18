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
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.Visitable;

/**
 * Unit Tests for {@link InitableVisitor}.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.ParameterizedInitable
 * @see org.cp.elements.lang.support.InitableVisitor
 * @see org.junit.jupiter.api.Test
 * @since 1.0.0
 */
class InitableVisitorUnitTests {

  @Test
  void constructInitableVisitor() {

    InitableVisitor visitor = new InitableVisitor();

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isNull();
    assertThat(visitor.getParameters()).isNull();
  }

  @Test
  void constructInitableVisitorWithArguments() {

    Object[] expectedArguments = { true, 'x', 2, Math.PI, "test" };

    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isEqualTo(expectedArguments);
    assertThat(visitor.getParameters()).isNull();
  }

  @Test
  void constructInitableVisitorWithParameters() {

    Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isNull();
    assertThat(visitor.getParameters()).isEqualTo(expectedParameters);
    assertThat(visitor.getParameters()).isNotSameAs(expectedParameters);
  }

  @Test
  void visitNonInitableVisitable() {

    Visitable mockVisitable = mock(Visitable.class);

    new InitableVisitor().visit(mockVisitable);

    verifyNoInteractions(mockVisitable);
  }

  @Test
  void visitNullVisitableObjectIsNullSafe() {
    new InitableVisitor().visit(null);
  }

  @Test
  void visitInitableObjectCallsInit() {

    InitableVisitable mockInitable = mock(InitableVisitable.class);

    InitableVisitor visitor = new InitableVisitor(ObjectUtils.EMPTY_OBJECT_ARRAY);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isEqualTo(ObjectUtils.EMPTY_OBJECT_ARRAY);
    assertThat(visitor.getParameters()).isNull();

    visitor.visit(mockInitable);

    verify(mockInitable, times(1)).init();
    verifyNoMoreInteractions(mockInitable);
  }

  @Test
  void visitParameterizedInitableObjectCallsInit() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    InitableVisitor visitor = new InitableVisitor();

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isNull();
    assertThat(visitor.getParameters()).isNull();

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, never()).init(any(Map.class));
    verify(mockParameterizedInitable, never()).init(any(Object[].class));
    verify(mockParameterizedInitable, times(1)).init();
    verifyNoMoreInteractions(mockParameterizedInitable);
  }

  @Test
  void visitParameterizedInitableObjectCallsInitWithArguments() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    Object[] expectedArguments = { true, 'x', 1, Math.PI, "test" };

    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isEqualTo(expectedArguments);
    assertThat(visitor.getParameters()).isNull();

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, never()).init(any(Map.class));
    verify(mockParameterizedInitable, times(1)).init(expectedArguments);
    verify(mockParameterizedInitable, never()).init();
    verifyNoMoreInteractions(mockParameterizedInitable);
  }

  @Test
  void visitParameterizedInitableObjectCallsInitWithParameters() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertThat(visitor).isNotNull();
    assertThat(visitor.getArguments()).isNull();
    assertThat(visitor.getParameters()).isEqualTo(expectedParameters);
    assertThat(visitor.getParameters()).isNotSameAs(expectedParameters);

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, times(1)).init(eq(expectedParameters));
    verify(mockParameterizedInitable, never()).init(any(Object[].class));
    verify(mockParameterizedInitable, never()).init();
    verifyNoMoreInteractions(mockParameterizedInitable);
  }

  interface ParameterizedInitableVisitable extends ParameterizedInitable, Visitable { }

  interface InitableVisitable extends Initable, Visitable { }

}
