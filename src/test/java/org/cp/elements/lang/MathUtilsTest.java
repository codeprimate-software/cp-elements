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

package org.cp.elements.lang;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThat;

import java.math.BigInteger;

import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

/**
 * The MathUtilsTest class is a test suite of test cases testing the contract and functionality of the MathUtils 
 * utility class.
 *
 * @author John J. Blum
 * @see org.cp.elements.lang.MathUtils
 * @see org.junit.Test
 * @since 1.0.0
 */
public class MathUtilsTest {

  @Rule
  public ExpectedException exception = ExpectedException.none();

  protected void assertEqualIntArrays(final int[] expected, final int[] actual) {
    assertEquals(expected.length, actual.length);

    for (int index = 0; index < expected.length; index++) {
      assertEquals(expected[index], actual[index]);
    }
  }

  protected int[] toIntArray(final int... numbers) {
    return numbers;
  }

  @Test
  public void testCircleArea() {
    assertEquals(78.5398d, MathUtils.circleArea(5.0d), 0.0001d);
  }

  @Test
  public void testCircleCircumference() {
    assertEquals(31.41592d, MathUtils.circleCircumference(5.0d), 0.000007d);
  }

  @Test
  public void testCircleDiameter() {
    assertEquals(10.0d, MathUtils.circleDiameter(5.0d), 0.0d);
  }

  @Test
  public void testConeVolume() {
    assertEquals(104.7d, MathUtils.coneVolume(5.0d, 4.0d), 0.02d);
  }

  @Test
  public void testCubeSurfaceArea() {
    assertEquals(150.0d, MathUtils.cubeSurfaceArea(5.0d), 0.0d);
  }

  @Test
  public void testCubeVolume() {
    assertEquals(125.0d, MathUtils.cubeVolume(5.0d), 0.0d);
  }

  @Test
  public void testCylinderSurfaceArea() {
    assertEquals(471.2388d, MathUtils.cylinderSurfaceArea(5.0d, 10.d), 0.000099d);
  }

  @Test
  public void testCylinderVolume() {
    assertEquals(314.1592d, MathUtils.cylinderVolume(5.0d, 4.0d), 0.00007d);
  }

  @Test
  public void testEllipseArea() {
    assertEquals(31.41592d, MathUtils.ellipseArea(5.0d, 2.0d), 0.000007d);
  }

  @Test
  public void testEllipsoidVolume() {
    assertEquals(251.32d, MathUtils.ellipsoidVolume(5.0d, 4.0d, 3.0d), 0.008d);
  }

  @Test
  public void testEquilateralTriangleArea() {
    assertEquals(10.825d, MathUtils.equilateralTriangleArea(5.0d), 0.0004d);
  }

  @Test
  public void factorial() {
    assertThat(MathUtils.factorial(BigInteger.ZERO), is(equalTo(BigInteger.ONE)));
    assertThat(MathUtils.factorial(BigInteger.ONE), is(equalTo(BigInteger.ONE)));
    assertThat(MathUtils.factorial(MathUtils.TWO), is(equalTo(MathUtils.TWO)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(3)).longValue(), is(equalTo(6l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(4)).longValue(), is(equalTo(24l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(5)).longValue(), is(equalTo(120l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(6)).longValue(), is(equalTo(720l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(7)).longValue(), is(equalTo(5040l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(8)).longValue(), is(equalTo(40320l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(9)).longValue(), is(equalTo(362880l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(10)).longValue(), is(equalTo(3628800l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(11)).longValue(), is(equalTo(39916800l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(12)).longValue(), is(equalTo(479001600l)));
  }

  @Test
  public void factorialHandlesNumericOverflow() {
    assertThat(MathUtils.factorial(BigInteger.valueOf(13)).longValue(), is(equalTo(6227020800l)));
    assertThat(MathUtils.factorial(BigInteger.valueOf(20)).longValue(), is(equalTo(2432902008176640000l)));
  }

  @Test
  public void factorialHandlesStackOverflow() {
    MathUtils.factorial(BigInteger.valueOf(20000));
  }

  @Test
  public void factorialOfNegativeOne() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(is(equalTo(String.format(MathUtils.NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE,
      MathUtils.NEGATIVE_ONE))));
    MathUtils.factorial(MathUtils.NEGATIVE_ONE);
  }

  @Test
  public void factorialOfNullValue() {
    exception.expect(IllegalArgumentException.class);
    exception.expectCause(is(nullValue(Throwable.class)));
    exception.expectMessage(is(equalTo("value must not be null")));

    MathUtils.factorial(null);
  }

  @Test
  public void testFibonacciNumber() {
    assertEquals(0, MathUtils.fibonacciNumber(1));
    assertEquals(1, MathUtils.fibonacciNumber(2));
    assertEquals(1, MathUtils.fibonacciNumber(3));
    assertEquals(2, MathUtils.fibonacciNumber(4));
    assertEquals(3, MathUtils.fibonacciNumber(5));
    assertEquals(5, MathUtils.fibonacciNumber(6));
    assertEquals(8, MathUtils.fibonacciNumber(7));
    assertEquals(13, MathUtils.fibonacciNumber(8));
    assertEquals(21, MathUtils.fibonacciNumber(9));
    assertEquals(514229, MathUtils.fibonacciNumber(30));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testFibonacciNumberWithIllegalArgument() {
    MathUtils.fibonacciSequence(-1);
  }

  @Test
  public void testFibonacciSequence() {
    assertEqualIntArrays(toIntArray(0), MathUtils.fibonacciSequence(1));
    assertEqualIntArrays(toIntArray(0, 1), MathUtils.fibonacciSequence(2));
    assertEqualIntArrays(toIntArray(0, 1, 1), MathUtils.fibonacciSequence(3));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3), MathUtils.fibonacciSequence(5));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13), MathUtils.fibonacciSequence(8));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144), MathUtils.fibonacciSequence(13));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181,
        6765), MathUtils.fibonacciSequence(21));
    assertEqualIntArrays(toIntArray(0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181,
      6765, 10946, 17711, 28657, 46368, 75025, 121393, 196418, 317811), MathUtils.fibonacciSequence(29));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testFibonacciSequenceWithAnIllegalArgument() {
    MathUtils.fibonacciSequence(0);
  }

  @Test
  public void testIrregularPrismVolume() {
    assertEquals(20.0d, MathUtils.irregularPrismVolume(5.0d, 4.0d), 0.0d);
  }

  @Test
  public void testMax() {
    assertEquals(Double.NaN, MathUtils.max((double[]) null), 0.0d);
    assertEquals(0.0d, MathUtils.max(0), 0.0d);
    assertEquals(0.0d, MathUtils.max(0, 0, 0), 0.0d);
    assertEquals(2.0d, MathUtils.max(0, 1, 2), 0.0d);
    assertEquals(2.0d, MathUtils.max(2, 1, 0), 0.0d);
    assertEquals(2.0d, MathUtils.max(1, 2, 0), 0.0d);
    assertEquals(1.0d, MathUtils.max(-100, -1, 1), 0.0d);
    assertEquals(1.0d, MathUtils.max(Double.NaN, -1, 1), 0.0d);
  }

  @Test
  public void testMin() {
    assertEquals(Double.NaN, MathUtils.min((double[]) null), 0.0d);
    assertEquals(0.0d, MathUtils.min(0), 0.0d);
    assertEquals(0.0d, MathUtils.min(0, 0, 0), 0.0d);
    assertEquals(0.0d, MathUtils.min(0, 1, 2), 0.0d);
    assertEquals(0.0d, MathUtils.min(2, 1, 0), 0.0d);
    assertEquals(0.0d, MathUtils.min(1, 2, 0), 0.0d);
    assertEquals(-100.0d, MathUtils.min(-100, -1, 1), 0.0d);
    assertEquals(-1.0d, MathUtils.min(Double.NaN, -1, 1), 0.0d);
  }

  @Test
  public void testMultiply() {
    assertEquals(0, MathUtils.multiply((int[]) null));
    assertEquals(0, MathUtils.multiply());
    assertEquals(0, MathUtils.multiply(0));
    assertEquals(0, MathUtils.multiply(0, 0, 0));
    assertEquals(0, MathUtils.multiply(0, 0, 1));
    assertEquals(1, MathUtils.multiply(1));
    assertEquals(1, MathUtils.multiply(1, 1, 1));
    assertEquals(6, MathUtils.multiply(1, 2, 3));
    assertEquals(64, MathUtils.multiply(2, 4, 8));
  }

  @Test
  public void testParallelogramArea() {
    assertEquals(30.0d, MathUtils.parallelogramArea(5.0d, 6.0d), 0.0d);
  }

  @Test
  public void testPyramidVolume() {
    assertEquals(10.d, MathUtils.pyramidVolume(5.0d, 6.0d), 0.0d);
  }

  @Test
  public void testPythagoreanTheorem() {
    assertEquals(5.0d, MathUtils.pythagoreanTheorem(3.0d, 4.0d), 0.0d);
  }

  @Test
  public void testRectangleArea() {
    assertEquals(40.0d, MathUtils.rectangleArea(5.0d, 8.0d), 0.0d);
  }

  @Test
  public void testRectangularPrismSurfaceArea() {
    assertEquals(94.0d, MathUtils.rectangularPrismSurfaceArea(5.0d, 4.0d, 3.0d), 0.0d);
  }

  @Test
  public void testRectanglePrismVolume() {
    assertEquals(240.0d, MathUtils.rectangularPrismVolume(5.0d, 6.0d, 8.0d), 0.0d);
  }

  @Test
  public void testRoundToNearestTenth() {
    assertEquals(Double.valueOf(0.0d), Double.valueOf(MathUtils.roundToNearestTenth(0.0d)));
    assertEquals(Double.valueOf(7.0d), Double.valueOf(MathUtils.roundToNearestTenth(7.0d)));
    assertEquals(Double.valueOf(13.1d), Double.valueOf(MathUtils.roundToNearestTenth(13.14d)));
    assertEquals(Double.valueOf(13.2d), Double.valueOf(MathUtils.roundToNearestTenth(13.19d)));
    assertEquals(Double.valueOf(19.4d), Double.valueOf(MathUtils.roundToNearestTenth(19.419d)));
    assertEquals(Double.valueOf(19.4d), Double.valueOf(MathUtils.roundToNearestTenth(19.449d)));
    assertEquals(Double.valueOf(19.6d), Double.valueOf(MathUtils.roundToNearestTenth(19.55d)));
  }

  @Test
  public void testSphereSurfaceArea() {
    assertEquals(314.1592d, MathUtils.sphereSurfaceArea(5.0d), 0.00007d);
  }

  @Test
  public void testSphereVolume() {
    assertEquals(113.09d, MathUtils.sphereVolume(3.0d), 0.008d);
  }

  @Test
  public void testSquareArea() {
    assertEquals(25.0d, MathUtils.squareArea(5.0d), 0.0d);
  }

  @Test
  public void testSum() {
    assertEquals(0, MathUtils.sum((int[]) null));
    assertEquals(0, MathUtils.sum());
    assertEquals(0, MathUtils.sum(0));
    assertEquals(0, MathUtils.sum(0, 0, 0));
    assertEquals(6, MathUtils.sum(1, 2, 3));
    assertEquals(21, MathUtils.sum(0, 1, 2, 3, 4, 5, 6));
  }

  @Test
  public void testTrapezoidArea() {
    assertEquals(25.0d, MathUtils.trapezoidArea(10.0d, 2.0d, 3.0d), 0.0d);
  }

  @Test
  public void testTriangleArea() {
    assertEquals(15.0d, MathUtils.triangleArea(5.0d, 6.0d), 0.0d);
  }
}
