/*
 * Copyright (c) 2011-Present. Codeprimate, LLC and authors.  All Rights Reserved.
 * <p/>
 * This software is licensed under the Codeprimate End User License Agreement (EULA).
 * This software is proprietary and confidential in addition to an intellectual asset
 * of the aforementioned authors.
 * <p/>
 * By using the software, the end-user implicitly consents to and agrees to be in compliance
 * with all terms and conditions of the EULA.  Failure to comply with the EULA will result in
 * the maximum penalties permissible by law.
 * <p/>
 * In short, this software may not be reverse engineered, reproduced, copied, modified
 * or distributed without prior authorization of the aforementioned authors, permissible
 * and expressed only in writing.  The authors grant the end-user non-exclusive, non-negotiable
 * and non-transferable use of the software "as is" without expressed or implied WARRANTIES,
 * EXTENSIONS or CONDITIONS of any kind.
 * <p/>
 * For further information on the software license, the end user is encouraged to read
 * the EULA @ ...
 */

package org.cp.elements.test;

import static org.junit.Assert.fail;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import org.cp.elements.lang.ExceptionUtils;
import org.junit.Test;

/**
 * The CommonEnumTestSuite class is a abstract test suite containing test cases adn functionality common to all
 * test classes testing Enum types.
 * <p/>
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

  protected Object invoke(final Class type, final String methodName) {
    try {
      Method method = type.getMethod(methodName);
      method.setAccessible(true);
      return method.invoke(type);
    }
    catch (NoSuchMethodException e) {
      fail(String.format("No method with name (%1$s) exists on Enum type (%2$s)!%n%3$s", methodName, type.getName(),
        ExceptionUtils.getStackTrace(e)));
    }
    catch (IllegalAccessException e) {
      fail(String.format("Method with name (%1$s) is not accessible on Enum type (%2$s)!%n%3$s", methodName,
        type.getName(), ExceptionUtils.getStackTrace(e)));
    }
    catch (InvocationTargetException e) {
      fail(String.format("Method with name (%1$s) is not accessible on Enum type (%2$s)!%n%3$s", methodName,
        type.getName(), ExceptionUtils.getStackTrace(e)));
    }

    throw new RuntimeException(String.format("Failed to invoke method (%1$s) on Enum type (%2$s)!",
      methodName, type.getName()));
  }

}
