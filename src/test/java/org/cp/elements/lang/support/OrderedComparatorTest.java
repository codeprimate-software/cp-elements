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

package org.cp.elements.lang.support;

import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import org.cp.elements.lang.ObjectUtils;
import org.cp.elements.lang.Ordered;
import org.cp.elements.lang.StringUtils;
import org.junit.Test;

/**
 * The OrderedComparatorTest class is a test suite of test cases testing the contract and functionality of
 * the OrderedComparator class.
 * <p/>
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
      throw new UnsupportedOperationException(StringUtils.NOT_IMPLEMENTED);
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
