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

import java.math.BigInteger;

import org.cp.elements.lang.annotation.NotNull;
import org.cp.elements.lang.annotation.NullSafe;

/**
 * {@link MathUtils} is an abstract utility class encapsulating common mathematical operations and calculations.
 *
 * @author John J. Blum
 * @see java.lang.Math
 * @see java.math.BigInteger
 * @see <a href="http://www.math.com">math.com</a>
 * @since 1.0.0
 */
@SuppressWarnings("unused")
public abstract class MathUtils {

  public static final double BASE_TEN = 10.0d;
  public static final double BASE_TWO = 2.0d;

  public static final BigInteger NEGATIVE_ONE = BigInteger.ONE.negate();
  public static final BigInteger TWO = BigInteger.valueOf(2);

  protected static final String DECIMAL_POINT = StringUtils.DOT_SEPARATOR;
  protected static final String NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE = "Value [%d] must be greater than equal to 0";

  /**
   * Calculates the area of a circle.
   *
   * {@literal A = π * radius^2}.
   *
   * @param radius {@link Double value} declaring the radius of the circle.
   * @return the area of a circle having the given radius.
   * @see #ellipseArea(double, double)
   */
  public static double circleArea(double radius) {
    return ellipseArea(radius, radius);
  }

  /**
   * Calculates the circumference of a circle.
   *
   * {@literal C = 2πr = 2 * π * radius}.
   *
   * @param radius {@link Double value} declaring the radius of the circle.
   * @return the circumference of a circle having the given radius.
   * @see java.lang.Math#PI
   */
  public static double circleCircumference(double radius) {
    return 2.0d * Math.PI * radius;
  }

  /**
   * Calculates the diameter of a circle.
   *
   * {@literal C = 2r = 2 * radius}.
   *
   * @param radius {@link Double value} declaring the radius of the circle.
   * @return the diameter of a circle having the given radius.
   */
  public static double circleDiameter(double radius) {
    return 2.0d * radius;
  }

  /**
   * Calculates the volume of a cone.
   *
   * {@literal V = π * radius^2 * height / 3.0}.
   *
   * @param radius {@link Double value} declaring the radius of the cone's base.
   * @param height {@link Double value} declaring the height of the cone.
   * @return the volume of a cone having the given radius and height.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#PI
   */
  public static double coneVolume(double radius, double height) {
    return Math.PI * Math.pow(radius, 2) * height / 3.0d;
  }

  /**
   * Counts the number of decimal places in the given {@link Double value}.
   *
   * @param value {@link Double value} for which to count the number of decimal places.
   * @return a count of the number of decimal places in the given {@link Double value}.
   */
  public static int countNumberOfDecimalPlaces(double value) {

    String stringValue = String.valueOf(value);
    int indexOfDecimalPoint = stringValue.indexOf(DECIMAL_POINT);

    return StringUtils.getDigits(stringValue.substring(indexOfDecimalPoint)).length();
  }

  /**
   * Calculates the surface area of a cube.
   *
   * {@literal A = 6.0 * sideLength^2}.
   *
   * @param sideLength {@link Double value} declaring the length of a side in the cube.
   * @return the surface area of a cube having the given side length.
   * @see #squareArea(double)
   */
  public static double cubeSurfaceArea(double sideLength) {
    return 6.0d * squareArea(sideLength);
  }

  /**
   * Calculates the volume of a cube.
   *
   * {@literal A = sideLength^3}.
   *
   * @param sideLength {@link Double value} declaring the length of a side in the cube.
   * @return the volume of a cube having the given side length.
   * @see java.lang.Math#pow(double, double)
   */
  public static double cubeVolume(double sideLength) {
    return Math.pow(sideLength, 3);
  }

  /**
   * Calculates the surface area of a cylinder.
   *
   * {@literal A = 2πrh + 2πr^2 = 2 * π * radius * height + 2 * π * radius^2}.
   *
   * @param radius {@link Double value} declaring the radius of the cylinder.
   * @param height {@link Double value} declaring the height of the cylinder.
   * @return the surface area of a cylinder having the given radius and height.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#PI
   */
  public static double cylinderSurfaceArea(double radius, double height) {
    return (2.0d * Math.PI * Math.pow(radius, 2)) + (2.0d * Math.PI * radius * height);
  }

  /**
   * Calculates the volume of a cylinder.
   *
   * {@literal V = πr^2h = π * radius^2 * height}.
   *
   * @param radius {@link Double value} declaring the radius of the cylinder's end(s).
   * @param height {@link Double value} declaring the height of the cylinder.
   * @return the volume of a cylinder having the given radius and height.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#PI
   */
  public static double cylinderVolume(double radius, double height) {
    return Math.PI * Math.pow(radius, 2) * height;
  }

  /**
   * Calculates the area of an ellipse.
   *
   * {@literal A = πab = π * length radius * height radius}.
   *
   * @param lengthRadius {@link Double value} declaring the (length) radius of the ellipse (x-axis).
   * @param heightRadius {@link Double value} declaring the (height) radius of the ellipse (y-axis).
   * @return the area of an ellipse having given the length (x) and height (y) radius.
   * @see java.lang.Math#PI
   */
  public static double ellipseArea(double lengthRadius, double heightRadius) {
    return Math.PI * lengthRadius * heightRadius;
  }

  /**
   * Calculates the volume of an ellipse.
   *
   * {@literal V = 4/3 * π * A * B * C}, Where A, B and C are the length (x-axis), height (y-axis) and width (z-axis)
   * radius values from the center reaching out to the surface of the ellipsoid in 3D space.
   *
   * @param lengthRadius {@link Double value} declaring the length radius of the ellipse (x-axis).
   * @param heightRadius {@link Double value} declaring the height radius of the ellipse (y-axis).
   * @param widthRadius {@link Double value} declaring the width radius of the ellipse (z-axis).
   * @return the volume of the ellipse having the given length, height and width radius.
   * @see java.lang.Math#PI
   */
  public static double ellipseVolume(double lengthRadius, double heightRadius, double widthRadius) {
    return 4.0d * Math.PI * lengthRadius * heightRadius * widthRadius / 3.0d;
  }

  /**
   * Calculates the area of a equilateral triangle.
   *
   * {@literal A = sqrt(3.0) / 4.0 * sideLength^2}.
   *
   * @param sideLength {@link Double value} declaring the length of a side in the equilateral triangle.
   * @return the area of an equilateral triangle having the given length of a side.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double equilateralTriangleArea(double sideLength) {
    return Math.sqrt(3.0d) / 4.0d * Math.pow(sideLength, 2);
  }

  /**
   * Calculates the factorial of the given number using an iterative algorithm and {@link BigInteger} value type
   * to avoid a {@literal StackOverflowException} and numeric overflow, respectively.
   *
   * @param value {@link BigInteger value} used to compute the factorial.
   * @return the factorial of the given {@link BigInteger number}.
   * @throws java.lang.IllegalArgumentException if the {@link BigInteger number} is {@literal null}
   * or less than {@literal 0}.
   * @see java.math.BigInteger
   */
  public static @NotNull BigInteger factorial(@NotNull BigInteger value) {

    Assert.notNull(value, "Value to compute the factorial of is required");
    Assert.isTrue(value.compareTo(BigInteger.ZERO) >= 0, NUMBER_LESS_THAN_ZERO_ERROR_MESSAGE, value);

    if (value.compareTo(TWO) <= 0) {
      return value.equals(TWO) ? TWO : BigInteger.ONE;
    }

    BigInteger result = value;
    BigInteger multiplier = value;

    for (multiplier = multiplier.add(NEGATIVE_ONE); multiplier.compareTo(BigInteger.ONE) > 0;
          multiplier = multiplier.add(NEGATIVE_ONE)) {

      result = result.multiply(multiplier);
    }

    return result;
  }

  /**
   * Gets the {@literal Fibonacci number} at {@link Integer position n} in the {@literal Fibonacci Sequence}.
   *
   * @param n {@link Integer value} declaring the {@literal nth number} in the {@literal Fibonacci Sequence}.
   * @return the {@link Integer nth number} in the {@literal Fibonacci Sequence}.
   * @throws IllegalArgumentException if position (n) is less than {@literal 1}.
   * @see #fibonacciSequence(int)
   */
  public static int fibonacciNumber(int n) {
    return fibonacciSequence(n)[n - 1];
  }

  /**
   * Calculates the {@literal Fibonacci Sequence} to the {@link Integer nth position}.
   *
   * @param n {@link Integer value} declaring the number of {@literal Fibonacci numbers}
   * in the {@literal Fibonacci Sequence} to calculate.
   * @return an {@link Integer array} containing n elements of the {@literal Fibonacci Sequence}.
   * @throws IllegalArgumentException if position (n) is less than {@literal 1}.
   * @see #fibonacciNumber(int)
   */
  public static int[] fibonacciSequence(int n) {

    Assert.argument(n, argument -> argument > 0,
      "The number of elements from the Fibonacci Sequence to calculate must be greater than equal to 0");

    int[] fibonacciNumbers = new int[n];

    for (int position = 0; position < n; position++) {
      if (position == 0) {
        fibonacciNumbers[position] = 0;
      }
      else if (position < 3) {
        fibonacciNumbers[position] = 1;
      }
      else {
        fibonacciNumbers[position] = fibonacciNumbers[position - 1] + fibonacciNumbers[position - 2];
      }
    }

    return fibonacciNumbers;
  }

  /**
   * Calculates the volume of an irregular prism.
   *
   * {@literal Volume = base * height}.
   *
   * @param base {@link Double value} declaring the base width of the prism.
   * @param height {@link Double value} declaring the height of the prism.
   * @return the volume of an irregular prism having the given base and height.
   */
  public static double irregularPrismVolume(double base, double height) {
    return base * height;
  }

  /**
   * Determines the maximum numerical value in an array of values.
   *
   * @param values array of numerical values from which to determine the maximum value.
   * @return the maximum numerical value in the given array of numerical values.
   * Returns {@link Double#NaN} if the array of values is {@literal null}.
   * @see java.lang.Math#max(double, double)
   * @see java.lang.Double#NaN
   * @see #min(double...)
   */
  @NullSafe
  public static double max(double... values) {

    double maxValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        maxValue = Double.isNaN(maxValue) ? value : Math.max(maxValue, value);
      }
    }

    return maxValue;
  }

  /**
   * Determines the minimum numerical value in an array of values.
   *
   * @param values array of numerical values from which to determine the minimum value.
   * @return the minimum numerical value in the given array of numerical values.
   * Returns {@link Double#NaN} if the array of values is {@literal null}.
   * @see java.lang.Math#min(double, double)
   * @see java.lang.Double#NaN
   * @see #max(double...)
   */
  @NullSafe
  public static double min(double... values) {

    double minValue = Double.NaN;

    if (values != null) {
      for (double value : values) {
        minValue = Double.isNaN(minValue) ? value : Math.min(minValue, value);
      }
    }

    return minValue;
  }

  /**
   * Multiplies all the numbers in an array together.
   *
   * @param numbers array of {@link Integer values} to multiply together.
   * @return a single {@link Integer value} resulting from multiply all the number in the array together.
   */
  @NullSafe
  public static int multiply(int... numbers) {

    int result = 0;

    if (numbers != null) {
      result = numbers.length > 0 ? 1 : 0;
      for (int number : numbers) {
        result *= number;
      }
    }

    return result;
  }

  /**
   * Calculates the area of a parallelogram.
   *
   * {@literal Area = base * height}.
   *
   * @param base {@link Double value} declaring the base length of the parallelogram.
   * @param height {@link Double value} declaring the height of the parallelogram.
   * @return the area of a parallelogram having the given base length and height.
   * @see #rectangleArea(double, double)
   */
  public static double parallelogramArea(double base, double height) {
    return rectangleArea(base, height);
  }

  /**
   * Calculates the volume of a pyramid.
   *
   * {@literal Volume = baseLength * baseWidth * height}.
   *
   * @param baseLength {@link Double value} declaring the base length of the pyramid.
   * @param baseWidth {@link Double value} declaring the base width of the pyramid.
   * @param height {@link Double value} declaring the height of the pyramid.
   * @return the volume of a pyramid having the given base length, base width and height.
   */
  public static double pyramidVolume(double baseLength, double baseWidth, double height) {
    return baseLength * baseWidth * height / 3.0d;
  }

  /**
   * Calculates the {@literal Pythagorean Theorem}.
   *
   * {@literal c^2 = a^2 + b^2.
   *
   * @param {@link Double value} for {@literal operand A}.
   * @param b {@link Double value} for {@literal operand B}.
   * @return the {@link Double value} of {@literal C} using the {@literal Pythagorean Theorem}.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#sqrt(double)
   */
  public static double pythagoreanTheorem(double a, double b) {
    return Math.sqrt(Math.pow(a, 2) + Math.pow(b, 2));
  }

  /**
   * Calculates the area of a rectangle.
   *
   * {@literal Area = length * height}.
   *
   * @param length {@link Double value} declaring the length of the rectangle.
   * @param height {@link Double value} declaring the height of the rectangle.
   * @return the area of a rectangle having the given length and height.
   */
  public static double rectangleArea(double length, double height) {
    return length * height;
  }

  /**
   * Calculates the surface area of a rectangular prism.
   *
   * {@literal Area = 2 * ((length * height) + (length * width) + (height * width))}.
   *
   * @param length {@link Double value} declaring the length of the rectangular prism (x-axis).
   * @param height {@link Double value} declaring the height of the rectangular prism (y-axis).
   * @param width {@link Double value} declaring the width of the rectangular prism (z-axis).
   * @return the surface area of a rectangular prism having the given length, height
   * and width (depth) of the sides.
   */
  public static double rectangularPrismSurfaceArea(double length, double height, double width) {
    return 2 * (length * height + length * width + height * width);
  }

  /**
   * Calculates the volume of a rectangular prism.
   *
   * {@literal Volume = length * width * height}.
   *
   * @param length {@link Double value} declaring the length of the rectangular prism (x-axis).
   * @param height {@link Double value} declaring the height of the rectangular prism (y-axis).
   * @param width {@link Double value} declaring the width of the rectangular prism (z-axis).
   * @return the volume of a rectangular prism having the given length, height and width.
   */
  public static double rectangularPrismVolume(double length, double height, double width) {
    return length * height * width;
  }

  /**
   * Rounds the specified {@link Double value} to the nearest tenth.
   *
   * @param value {@link Double value} to round to the nearest tenth.
   * @return the {@link Double value} rounded to the nearest tenth.
   * @see #roundToPrecision(double, int)
   */
  public static double roundToNearestTenth(double value) {
    return roundToPrecision(value, 1);
  }

  /**
   * Rounds the {@link Double value} to the nearest, given {@link Integer number of decimal places}.
   *
   * @param value {@link Double value} to round.
   * @param numberOfDecimalPlaces {@link Integer value} declaring the number of decimal places;
   * must be greater than {@literal 0}.
   * @return the given {@link Double value} rounded to nearest, given {@link Integer number of decimal places}.
   * @throws IllegalArgumentException if the {@link Integer number of decimal places}
   * is less than equal to {@literal 0}.
   * @see java.lang.Math#round(double)
   */
  public static double roundToPrecision(double value, int numberOfDecimalPlaces) {

    Assert.isTrue(numberOfDecimalPlaces > 0,
      "Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces);

    double result = value;
    double baseTenExponentialFactor = Math.pow(BASE_TEN, numberOfDecimalPlaces);

    result *= baseTenExponentialFactor;
    result = Math.round(result);
    result /= baseTenExponentialFactor;

    return result;
  }

  /**
   * Calculates the surface area of a sphere.
   *
   * {@literal A = 4πr^2 = 4 * π * radius^2}.
   *
   * @param radius {@link Double value} declaring the radius of the sphere.
   * @return the surface area of a sphere having the given radius.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#PI
   */
  public static double sphereSurfaceArea(double radius) {
    return 4.0d * Math.PI * Math.pow(radius, 2);
  }

  /**
   * Calculates the volume of a sphere.
   *
   * {@literal V = 4/3πr^3 = 4/3 * π * radius^3}.
   *
   * @param radius {@link Double value} declaring the radius of the sphere.
   * @return the volume of a sphere having the given radius.
   * @see java.lang.Math#pow(double, double)
   * @see java.lang.Math#PI
   */
  public static double sphereVolume(double radius) {
    return 4.0d * Math.PI * Math.pow(radius, 3) / 3.0d;
  }

  /**
   * Calculates the area of a square.
   *
   * {@literal A = side^2}.
   *
   * @param side {@link Double value} declaring the length of a side of the square.
   * @return the area of a square having the given length of a side.
   * @see #rectangleArea(double, double)
   */
  public static double squareArea(double side) {
    return rectangleArea(side, side);
  }

  /**
   * Calculates the sum of all {@link Integer values} in the array.
   *
   * @param numbers array of {@link Integer values} to sum.
   * @return the sum of all {@link Integer values} in the array.
   */
  @NullSafe
  public static int sum(int... numbers) {

    int sum = 0;

    if (numbers != null) {
      for (int number : numbers) {
        sum += number;
      }
    }

    return sum;
  }

  /**
   * Calculates the area of a trapezoid.
   *
   * {@literal A = (a + b) / 2 * height}.
   *
   * @param height {@link Double value} declaring the height of the trapezoid.
   * @param baseOne {@link Double value} declaring the length of one of the trapezoid's bases.
   * @param baseTwo {@link Double value} declaring the length of the other trapezoid's bases.
   * @return the area of a trapezoid having the given height and length of 2 of the bases.
   */
  public static double trapezoidArea(double height, double baseOne, double baseTwo) {
    return (baseOne + baseTwo) / 2.0d * height;
  }

  /**
   * Calculates the area of a triangle.
   *
   * {@literal A = (base * height) / 2}.
   *
   * @param base {@link Double value} declaring the length of the triangle's base.
   * @param height {@link Double value} declaring the height (highest point) of the triangle from its base.
   * @return the area of a triangle having the given base length and height.
   */
  public static double triangleArea(double base, double height) {
    return base * height / 2.0d;
  }

  /**
   * Truncates the given {@link Double value} to the given {@link Integer number of decimal places}.
   *
   * @param value {@link Double value} to truncate.
   * @param numberOfDecimalPlaces {@link Integer value} declaring the number of decimal places;
   * must be greater than {@literal 0}.
   * @return the given {@link Double value} truncated by the given {@link Integer number of decimal places}.
   * @see java.lang.Math#floor(double)
   */
  public static double truncateToPrecision(double value, int numberOfDecimalPlaces) {

    Assert.isTrue(numberOfDecimalPlaces > 0,
      "Number of decimal places [%d] must be greater than 0", numberOfDecimalPlaces);

    double result = value;
    double baseTenExponentialFactor = Math.pow(BASE_TEN, numberOfDecimalPlaces);

    result *= baseTenExponentialFactor;
    result = Math.floor(result);
    result /= baseTenExponentialFactor;

    return result;
  }
}
