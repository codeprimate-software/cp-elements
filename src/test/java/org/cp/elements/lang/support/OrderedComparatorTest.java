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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.Constants;
import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Ordered;
import org.junit.Test;

/**
 * The OrderedComparatorTest class is a test suite of test cases testing the contract and functionality of
 * the OrderedComparator class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.support.OrderedComparator
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class OrderedComparatorTest {

  @Test
  public void testCompare() {
    final Priority necessary = new Priority(1, "Necessary");
    final Priority requirement = new Priority(2, "Requirement");
    final Priority niceToHave = new Priority(3, "Nice-to-Have");
    final Priority useless = new Priority(4, "Useless");

    final List<Priority> priorities = new ArrayList<>(Arrays.asList(useless, niceToHave, necessary, requirement));

    Collections.sort(priorities);

    final List<Priority> sortedPriorities = Arrays.asList(necessary, niceToHave, requirement, useless);

    assertEquals(sortedPriorities, priorities);

    Collections.sort(priorities, new OrderedComparator());

    final List<Priority> orderedPriorities = Arrays.asList(necessary, requirement, niceToHave, useless);

    assertEquals(orderedPriorities, priorities);
  }

  protected static final class Priority implements Comparable<Priority>, Ordered {

    private final int index;
    private final String name;

    public Priority(final int index, final String name) {
      assert index > 0 : "The index of the Priority must be positive!";
      assert name != null : "The name of the Priority cannot be null!";
      this.index = index;
      this.name = name;
    }

    @Override
    public int getIndex() {
      return index;
    }

    @Override
    public void setIndex(final int index) {
      throw new UnsupportedOperationException(Constants.NOT_IMPLEMENTED);
    }

    public String getName() {
      return name;
    }

    @Override
    @SuppressWarnings("all")
    public int compareTo(final Priority priority) {
      return getName().compareTo(priority.getName());
    }

    @Override
    public boolean equals(final Object obj) {
      if (obj == this) {
        return true;
      }

      if (!(obj instanceof Priority)) {
        return false;
      }

      final Priority that = (Priority) obj;

      return ObjectUtils.equals(getName(), that.getName());
    }

    @Override
    public int hashCode() {
      int hashValue = 17;
      hashValue = 37 * hashValue + ObjectUtils.hashCode(getName());
      return hashValue;
    }

    @Override
    public String toString() {
      return getName();
    }
  }

}
