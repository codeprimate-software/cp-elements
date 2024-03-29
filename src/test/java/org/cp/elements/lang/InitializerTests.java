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
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.ArgumentMatchers.same;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

/**
 * Unit Tests for {@link Initializer}.
 *
 * @author John J. Blum
 * @see org.junit.jupiter.api.Test
 * @see org.mockito.Mockito
 * @see org.cp.elements.lang.Initable
 * @see org.cp.elements.lang.Initializer
 * @see org.cp.elements.lang.ParameterizedInitable
 */
public class InitializerTests {

  @Test
  public void initInitableObject() {

    Initable mockInitable = mock(Initable.class);

    assertThat(Initializer.init(mockInitable)).isTrue();

    verify(mockInitable, times(1)).init();
  }

  @Test
  public void initNonInitableObject() {
    assertThat(Initializer.init(new Object())).isFalse();
  }

  @Test
  public void initUsingArgumentsWithInitableObject() {

    Object[] args = { "arg1", "arg2", "arg3" };

    Initable mockInitable = mock(Initable.class);

    assertThat(Initializer.init(mockInitable, args)).isTrue();

    verify(mockInitable, times(1)).init();
  }

  @Test
  public void initUsingArgumentsWithNonInitableObject() {
    assertThat(Initializer.init(new Object(), "arg1", "arg2", "arg3")).isFalse();
  }

  @Test
  public void initUsingArgumentsWithParameterizedInitableObject() {

    Object[] args = { "arg1", "arg2", "arg3" };

    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    assertThat(Initializer.init(mockParameterizedInitable, args)).isTrue();

    verify(mockParameterizedInitable, times(1))
      .init(eq("arg1"), eq("arg2"), eq("arg3"));
  }

  @Test
  public void initUsingParametersWithInitableObject() {

    Map<String, String> parameters = new HashMap<>(3);

    parameters.put("param1", "arg1");
    parameters.put("param2", "arg2");
    parameters.put("param3", "arg3");

    Initable mockInitable = mock(Initable.class);

    assertThat(Initializer.init(mockInitable, parameters)).isTrue();

    verify(mockInitable, times(1)).init();
  }

  @Test
  public void initUsingParametersWithNonInitableObject() {
    assertThat(Initializer.init(new Object(), Collections.emptyMap())).isFalse();
  }

  @Test
  public void initUsingParametersWithParameterizedInitableObject() {

    Map<String, String> parameters = new HashMap<>(3);

    parameters.put("param1", "arg1");
    parameters.put("param2", "arg2");
    parameters.put("param3", "arg3");

    ParameterizedInitable mockParameterizedInitable = mock(ParameterizedInitable.class);

    assertThat(Initializer.init(mockParameterizedInitable, parameters)).isTrue();

    verify(mockParameterizedInitable, times(1)).init(same(parameters));
  }
}
