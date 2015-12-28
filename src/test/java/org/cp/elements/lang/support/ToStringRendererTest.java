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

import static org.junit.Assert.assertEquals;

import org.junit.Test;

/**
 * The ToStringRendererTest class is a test suite of test cases testing the contract and functionality
 * of the ToStringRenderer class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.ToStringRenderer
 * @see org.junit.Assert
 * @see org.junit.Test
 */
public class ToStringRendererTest {

  @Test
  public void testRender() {
    assertEquals("test", new ToStringRenderer().render("test"));
  }

}
