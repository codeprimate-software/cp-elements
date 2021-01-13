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
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;

/**
 * Unit tests for {@link Renderable}
 *
 * @author John Blum
 * @see org.junit.Test
 * @see org.cp.elements.lang.Renderable
 * @see org.cp.elements.lang.Renderer
 * @since 1.0.0
 */
public class RenderableTests {

  @Test
  @SuppressWarnings("unchecked")
  public void rendererRendersRenderable() {

    Renderable mockRenderable = mock(Renderable.class);

    Renderer<Renderable> mockRenderer = mock(Renderer.class);

    when(mockRenderable.render(any(Renderer.class))).thenCallRealMethod();
    when(mockRenderer.render(any())).thenReturn("test");

    assertThat(mockRenderable.render(mockRenderer)).isEqualTo("test");

    verify(mockRenderer, times(1)).render(eq(mockRenderable));
  }
}
