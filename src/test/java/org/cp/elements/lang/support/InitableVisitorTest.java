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

package org.cp.elements.lang.support;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.sameInstance;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.HashMap;
import java.util.Map;

import org.cp.elements.lang.Initable;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.ParameterizedInitable;
import org.cp.elements.lang.Visitable;
import org.junit.Test;

/**
 * Unit tests for {@link InitableVisitor}.
 *
 * @author John J. Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Visitable
 * @see org.cp.elements.lang.support.InitableVisitor
 * @since 1.0.0
 */
@SuppressWarnings({ "deprecation", "unused" })
public class InitableVisitorTest {

  @Test
  public void testConstruct() {

    InitableVisitor visitor = new InitableVisitor();

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertNull(visitor.getParameters());
  }

  @Test
  public void testConstructWithArguments() {

    Object[] expectedArguments = { true, 'x', 2, Math.PI, "test" };

    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertNotNull(visitor);
    assertSame(expectedArguments, visitor.getArguments());
    assertNull(visitor.getParameters());
  }

  @Test
  public void testConstructWithParameters() {

    Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertNotNull(visitor);
    assertNull(visitor.getArguments());
    assertSame(expectedParameters, visitor.getParameters());
  }

  @Test
  public void visitNonInitableVisitable() {
    new InitableVisitor().visit(mock(Visitable.class));
  }

  @Test
  public void visitInitableCallsInit() {

    InitableVisitable mockInitable = mock(InitableVisitable.class);

    InitableVisitor visitor = new InitableVisitor(ObjectUtils.EMPTY_OBJECT_ARRAY);

    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getArguments(), is(sameInstance(ObjectUtils.EMPTY_OBJECT_ARRAY)));
    assertThat(visitor.getParameters(), is(nullValue()));

    visitor.visit(mockInitable);

    verify(mockInitable, times(1)).init();
  }

  @Test
  public void visitParameterizedInitableCallsInit() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    InitableVisitor visitor = new InitableVisitor();

    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getArguments(), is(nullValue()));
    assertThat(visitor.getParameters(), is(nullValue()));

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, never()).init(any(Map.class));
    verify(mockParameterizedInitable, never()).init(any(Object[].class));
    verify(mockParameterizedInitable, times(1)).init();
  }

  @Test
  public void visitParameterizedInitableCallsInitWithArguments() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    Object[] expectedArguments = { true, 'x', 1, Math.PI, "test" };

    InitableVisitor visitor = new InitableVisitor(expectedArguments);

    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getArguments(), is(sameInstance(expectedArguments)));
    assertThat(visitor.getParameters(), is(nullValue()));

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, never()).init(any(Map.class));
    verify(mockParameterizedInitable, times(1)).init(expectedArguments);
    verify(mockParameterizedInitable, never()).init();
  }

  @Test
  public void visitParameterizedInitableCallsInitWithParameters() {

    ParameterizedInitableVisitable mockParameterizedInitable = mock(ParameterizedInitableVisitable.class);

    Map<String, String> expectedParameters = new HashMap<>(3);

    expectedParameters.put("param1", "value1");
    expectedParameters.put("param2", "value2");
    expectedParameters.put("param3", "value3");

    InitableVisitor visitor = new InitableVisitor(expectedParameters);

    assertThat(visitor, is(notNullValue()));
    assertThat(visitor.getArguments(), is(nullValue()));
    assertThat(visitor.getParameters(), is(sameInstance(expectedParameters)));

    visitor.visit(mockParameterizedInitable);

    verify(mockParameterizedInitable, times(1)).init(eq(expectedParameters));
    verify(mockParameterizedInitable, never()).init(any(Object[].class));
    verify(mockParameterizedInitable, never()).init();
  }

  protected interface ParameterizedInitableVisitable extends ParameterizedInitable, Visitable { }

  protected interface InitableVisitable extends Initable, Visitable { }

}
