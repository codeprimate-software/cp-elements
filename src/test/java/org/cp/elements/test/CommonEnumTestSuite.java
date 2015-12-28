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

package org.cp.elements.test;

import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.cp.elements.lang.ThrowableUtils;
import org.junit.Test;

/**
 * The CommonEnumTestSuite class is a abstract test suite containing test cases adn functionality common to all
 * test classes testing Enum types.
 *
 * @author John J. Blum
 * @see java.lang.Enum
 * @see org.cp.elements.test.AbstractMockingTestSuite
 * @see org.junit.Test
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class CommonEnumTestSuite extends AbstractMockingTestSuite {

  protected abstract Enum getEnumType();

  @Test
  public void testValueOfAbbreviation() {
  }

  @Test
  public void testValueOfName() {
  }

  @SuppressWarnings("unchecked")
  protected Object invoke(final Class type, final String methodName) {
    try {
      Method method = type.getMethod(methodName);
      method.setAccessible(true);
      return method.invoke(type);
    }
    catch (NoSuchMethodException e) {
      fail(String.format("No method with name (%1$s) exists on Enum type (%2$s)!%n%3$s", methodName, type.getName(),
        ThrowableUtils.getStackTrace(e)));
    }
    catch (IllegalAccessException e) {
      fail(String.format("Method with name (%1$s) is not accessible on Enum type (%2$s)!%n%3$s", methodName,
        type.getName(), ThrowableUtils.getStackTrace(e)));
    }
    catch (InvocationTargetException e) {
      fail(String.format("Method with name (%1$s) is not accessible on Enum type (%2$s)!%n%3$s", methodName,
        type.getName(), ThrowableUtils.getStackTrace(e)));
    }

    throw new RuntimeException(String.format("Failed to invoke method (%1$s) on Enum type (%2$s)!",
      methodName, type.getName()));
  }

}
