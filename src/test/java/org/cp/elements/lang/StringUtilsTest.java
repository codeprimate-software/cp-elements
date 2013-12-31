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

package org.cp.elements.lang;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * The StringUtilsTest class is a test suite of test cases testing the contract and functionality of the 
 * StringUtils class.
 * <p/>
 * @author John J. Blum
 * @see org.cp.elements.lang.StringUtils
 * @see org.junit.Assert
 * @see org.junit.Test
 * @since 1.0.0
 */
public class StringUtilsTest {

  @Test
  public void testConcat() {
    assertEquals("one, two, three", StringUtils.concat("one", "two", "three"));
    assertEquals("org.cp.elements", StringUtils.concat(new String[] { "org", "cp", "elements" }, StringUtils.DOT_SEPARATOR));
    assertEquals("void, null, nil", StringUtils.concat("void", null, "nil"));
  }

  @Test
  public void testConcatWithBlankString() {
    assertEquals("  ", StringUtils.concat("  "));
  }

  @Test
  public void testConcatWithEmptyString() {
    assertEquals(StringUtils.EMPTY_STRING, StringUtils.concat(StringUtils.EMPTY_STRING));
  }

  @Test(expected = NullPointerException.class)
  public void testConcatWithNullStringArray() {
    StringUtils.concat((String[]) null);
  }

  @Test
  public void testContains() {
    assertTrue(StringUtils.contains("test", "test"));
    assertTrue(StringUtils.contains("testing", "test"));
    assertTrue(StringUtils.contains("tested", "test"));
    assertTrue(StringUtils.contains("null", "null"));
  }

  @Test
  public void testContainsWithUncontainedText() {
    assertFalse(StringUtils.contains("null", "nil"));
    assertFalse(StringUtils.contains("test", "TEST"));
    assertFalse(StringUtils.contains("TEST", "test"));
    assertFalse(StringUtils.contains("test", "testing"));
    assertFalse(StringUtils.contains(null, "test"));
    assertFalse(StringUtils.contains(null, "null"));
    assertFalse(StringUtils.contains("null", null));
  }

  @Test
  public void testContainsDigits() {
    assertTrue(StringUtils.containsDigits("0123456789"));
    assertTrue(StringUtils.containsDigits("Ol2E4SG7B9"));
    assertTrue(StringUtils.containsDigits("!1#$%*"));
    assertTrue(StringUtils.containsDigits("$100.00"));
    assertTrue(StringUtils.containsDigits("50%"));
    assertTrue(StringUtils.containsDigits("(503) 555-1234"));
    assertTrue(StringUtils.containsDigits("00"));
    assertTrue(StringUtils.containsDigits("0"));
  }

  @Test
  public void testContainsNoDigits() {
    assertFalse(StringUtils.containsDigits("abcdefghijklmnopqrstuvwxyz"));
    assertFalse(StringUtils.containsDigits("lOlOl"));
    assertFalse(StringUtils.containsDigits("$###.##"));
    assertFalse(StringUtils.containsDigits("one"));
    assertFalse(StringUtils.containsDigits("  "));
    assertFalse(StringUtils.containsDigits(""));
    assertFalse(StringUtils.containsDigits(null));
  }

  @Test
  public void testContainsLetters() {
    assertTrue(StringUtils.containsLetters("abcdefghijklmnopqrstuvwxyz"));
    assertTrue(StringUtils.containsLetters("lOlOl"));
    assertTrue(StringUtils.containsLetters("l0l0l"));
    assertTrue(StringUtils.containsLetters("1O1O1"));
    assertTrue(StringUtils.containsLetters("O0"));
    assertTrue(StringUtils.containsLetters("O"));
    assertTrue(StringUtils.containsLetters("XyZ"));
    assertTrue(StringUtils.containsLetters("ABC123"));
    assertTrue(StringUtils.containsLetters("fifty%"));
  }

  @Test
  public void testContainsNoLetters() {
    assertFalse(StringUtils.containsLetters("0123456789"));
    assertFalse(StringUtils.containsLetters("10101"));
    assertFalse(StringUtils.containsLetters("@554013"));
    assertFalse(StringUtils.containsLetters("$$$"));
    assertFalse(StringUtils.containsLetters("  "));
    assertFalse(StringUtils.containsLetters(""));
    assertFalse(StringUtils.containsLetters(null));
  }

  @Test
  public void testEqualsIgnoreCase() {
    assertTrue(StringUtils.equalsIgnoreCase("test", "test"));
    assertTrue(StringUtils.equalsIgnoreCase("test", "TEST"));
    assertTrue(StringUtils.equalsIgnoreCase("titlecase", "Titlecase"));
    assertTrue(StringUtils.equalsIgnoreCase("null", "null"));
  }

  @Test
  public void testEqualsIgnoreCaseWithUnequalStrings() {
    assertFalse(StringUtils.equalsIgnoreCase("test", "testing"));
    assertFalse(StringUtils.equalsIgnoreCase("seam", "seem"));
    assertFalse(StringUtils.equalsIgnoreCase("null", null));
    assertFalse(StringUtils.equalsIgnoreCase(null, "null"));
  }

  @Test
  public void testGetDigits() {
    assertEquals("", StringUtils.getDigits(""));
    assertEquals("", StringUtils.getDigits("  "));
    assertEquals("", StringUtils.getDigits("abc"));
    assertEquals("", StringUtils.getDigits("lOlOl"));
    assertEquals("", StringUtils.getDigits("oneTwoThree"));
    assertEquals("", StringUtils.getDigits("$###.##"));
    assertEquals("", StringUtils.getDigits(".lS%"));
    assertEquals("123", StringUtils.getDigits("123"));
    assertEquals("123", StringUtils.getDigits("abc123"));
    assertEquals("00", StringUtils.getDigits("l0l0l"));
    assertEquals("111", StringUtils.getDigits("1O1O1"));
    assertEquals("012480", StringUtils.getDigits("n0a1bc2defg4hijklmno8p0qrstuvwxzy"));
    assertEquals("12480", StringUtils.getDigits("localhost:12480"));
    assertEquals("102347712012480", StringUtils.getDigits("10.234.77.120:12480"));
    assertEquals("10050", StringUtils.getDigits("$100.50"));
    assertEquals("50", StringUtils.getDigits("50%"));
    assertEquals("50355512345", StringUtils.getDigits("(503) 555-1234 x5"));
  }

  @Test
  public void testGetLetters() {
    assertEquals("", StringUtils.getLetters(""));
    assertEquals("", StringUtils.getLetters("  "));
    assertEquals("", StringUtils.getLetters("123"));
    assertEquals("", StringUtils.getLetters("10101"));
    assertEquals("", StringUtils.getLetters("8007"));
    assertEquals("", StringUtils.getLetters("$100.50"));
    assertEquals("", StringUtils.getLetters("50%"));
    assertEquals("", StringUtils.getLetters("@$$4013"));
    assertEquals("abc", StringUtils.getLetters("abc"));
    assertEquals("abc", StringUtils.getLetters("abc123"));
    assertEquals("ABC", StringUtils.getLetters("1A2BC3"));
    assertEquals("lll", StringUtils.getLetters("l0l0l"));
    assertEquals("OO", StringUtils.getLetters("1O1O1"));
    assertEquals("localhost", StringUtils.getLetters("localhost:12480"));
    assertEquals("nabcdefghijklmnopqrstuvwxyz", StringUtils.getLetters("n0a1bc2defg4hijklmno8p0qrstuvwxyz"));
    assertEquals("x", StringUtils.getLetters("(503) 555-1234 x520"));
  }

  @Test
  public void testGetSpaces() {
    assertEquals("", StringUtils.getSpaces(0));
    assertEquals(" ", StringUtils.getSpaces(1));
    assertEquals("  ", StringUtils.getSpaces(2));
    assertEquals("   ", StringUtils.getSpaces(3));
    assertEquals("    ", StringUtils.getSpaces(4));
    assertEquals("     ", StringUtils.getSpaces(5));
    assertEquals("      ", StringUtils.getSpaces(6));
    assertEquals("       ", StringUtils.getSpaces(7));
    assertEquals("        ", StringUtils.getSpaces(8));
    assertEquals("         ", StringUtils.getSpaces(9));
    assertEquals("          ", StringUtils.getSpaces(10));
    assertEquals("           ", StringUtils.getSpaces(11));
    assertEquals("            ", StringUtils.getSpaces(12));
    assertEquals("             ", StringUtils.getSpaces(13));
    assertEquals("              ", StringUtils.getSpaces(14));
    assertEquals("               ", StringUtils.getSpaces(15));
    assertEquals("                ", StringUtils.getSpaces(16));
    assertEquals("                 ", StringUtils.getSpaces(17));
    assertEquals("                  ", StringUtils.getSpaces(18));
    assertEquals("                   ", StringUtils.getSpaces(19));
    assertEquals("                    ", StringUtils.getSpaces(20));
    assertEquals("                     ", StringUtils.getSpaces(21));
    assertEquals("                                                   ", StringUtils.getSpaces(51));
    assertEquals("                                                                                                   ",
      StringUtils.getSpaces(99));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testGetSpacesWithInvalidNumber() {
    try {
      StringUtils.getSpaces(-1);
    }
    catch (IllegalArgumentException expected) {
      assertEquals("The number (-1) must be greater than equal to 0!", expected.getMessage());
      throw expected;
    }
  }

  @Test
  public void testHasText() {
    assertTrue(StringUtils.hasText("test"));
    assertTrue(StringUtils.hasText("0123456789"));
    assertTrue(StringUtils.hasText("$@$!"));
    assertTrue(StringUtils.hasText("X"));
    assertTrue(StringUtils.hasText("-"));
    assertTrue(StringUtils.hasText("null"));
    assertTrue(StringUtils.hasText("nill"));
    assertTrue(StringUtils.hasText("empty"));
    assertTrue(StringUtils.hasText("blank"));
    assertTrue(StringUtils.hasText("_"));
  }

  @Test
  public void testHasTextWithNoText() {
    assertFalse(StringUtils.hasText(null));
    assertFalse(StringUtils.hasText(""));
    assertFalse(StringUtils.hasText(" "));
    assertFalse(StringUtils.hasText("   "));
  }

  @Test
  public void testIndexOf() {
    assertEquals(-1, StringUtils.indexOf(null, "test"));
    assertEquals(-1, StringUtils.indexOf("test", null));
    assertEquals(-1, StringUtils.indexOf("", " "));
    assertEquals(-1, StringUtils.indexOf("", "text"));
    assertEquals(-1, StringUtils.indexOf("null", "nil"));
    assertEquals(0, StringUtils.indexOf("", ""));
    assertEquals(0, StringUtils.indexOf("  ", ""));
    assertEquals(0, StringUtils.indexOf("  ", " "));
    assertEquals(0, StringUtils.indexOf("null", "null"));
    assertEquals(4, StringUtils.indexOf("This is example text!", " "));
    assertEquals(8, StringUtils.indexOf("This is example text!", "ex"));
    assertEquals(16, StringUtils.indexOf("This is example text!", "text"));
    assertEquals(17, StringUtils.indexOf("This is example text!", "ext"));
    assertEquals(-1, StringUtils.indexOf("This is example text!", "test"));
  }

  @Test
  public void testIsBlank() {
    assertTrue(StringUtils.isBlank(null));
    assertTrue(StringUtils.isBlank(""));
    assertTrue(StringUtils.isBlank(" "));
    assertTrue(StringUtils.isBlank("   "));
    assertTrue(StringUtils.isBlank("\0"));
  }

  @Test
  public void testIsBlankWithNonBlankStrings() {
    assertFalse(StringUtils.isBlank("_"));
    assertFalse(StringUtils.isBlank("___"));
    assertFalse(StringUtils.isBlank("null"));
    assertFalse(StringUtils.isBlank("nil"));
    assertFalse(StringUtils.isBlank("false"));
    assertFalse(StringUtils.isBlank("."));
    assertFalse(StringUtils.isBlank("0"));
    assertFalse(StringUtils.isBlank("0.0"));
    assertFalse(StringUtils.isBlank("space"));
    assertFalse(StringUtils.isBlank("empty"));
    assertFalse(StringUtils.isBlank("blank"));
    assertFalse(StringUtils.isBlank("test"));
  }

  @Test
  public void testIsDigits() {
    assertTrue(StringUtils.isDigits("012"));
    assertTrue(StringUtils.isDigits("123"));
    assertTrue(StringUtils.isDigits("0123456789"));
    assertTrue(StringUtils.isDigits("112358"));
    assertTrue(StringUtils.isDigits("012480"));
    assertTrue(StringUtils.isDigits("0122444488888888"));
  }

  @Test
  public void testIsDigitsWithNonDigitStrings() {
    assertFalse(StringUtils.isDigits(null));
    assertFalse(StringUtils.isDigits(""));
    assertFalse(StringUtils.isDigits("  "));
    assertFalse(StringUtils.isDigits("abc"));
    assertFalse(StringUtils.isDigits("abc123"));
    assertFalse(StringUtils.isDigits("l0l0l"));
    assertFalse(StringUtils.isDigits("B00B"));
    assertFalse(StringUtils.isDigits("$1024.64"));
    assertFalse(StringUtils.isDigits("50%"));
    assertFalse(StringUtils.isDigits("(503) 555-0123"));
    assertFalse(StringUtils.isDigits("####"));
    assertFalse(StringUtils.isDigits(" 0123 "));
  }

  @Test
  public void testIsEmpty() throws Exception {
    assertTrue(StringUtils.isEmpty(""));
  }

  @Test
  public void testIsEmptyWithNonEmptyStrings() {
    assertFalse(StringUtils.isEmpty(null));
    assertFalse(StringUtils.isEmpty(" "));
    assertFalse(StringUtils.isEmpty("   "));
    assertFalse(StringUtils.isEmpty("false"));
    assertFalse(StringUtils.isEmpty("\0"));
    assertFalse(StringUtils.isEmpty("0"));
    assertFalse(StringUtils.isEmpty("0.0"));
    assertFalse(StringUtils.isEmpty("-"));
    assertFalse(StringUtils.isEmpty("_"));
    assertFalse(StringUtils.isEmpty("x"));
    assertFalse(StringUtils.isEmpty("empty"));
  }

  @Test
  public void testIsLetters() {
    assertTrue(StringUtils.isLetters("abcdefghijklmnopqrstuvwxyz"));
    assertTrue(StringUtils.isLetters("ABC"));
    assertTrue(StringUtils.isLetters("lOlO"));
    assertTrue(StringUtils.isLetters("oneTwoThree"));
  }

  @Test
  public void testIsLettersWithNonLetterStrings() {
    assertFalse(StringUtils.isLetters(null));
    assertFalse(StringUtils.isLetters(""));
    assertFalse(StringUtils.isLetters("  "));
    assertFalse(StringUtils.isLetters("123"));
    assertFalse(StringUtils.isLetters("abc123"));
    assertFalse(StringUtils.isLetters("A1BC23"));
    assertFalse(StringUtils.isLetters("$oneHundred.fifty"));
    assertFalse(StringUtils.isLetters("fifty%"));
    assertFalse(StringUtils.isLetters("@$"));
    assertFalse(StringUtils.isLetters("$0$"));
    assertFalse(StringUtils.isLetters("localhost:8080"));
  }

  @Test
  public void testLastIndexOf() {
    assertEquals(-1, StringUtils.lastIndexOf(null, "test"));
    assertEquals(-1, StringUtils.lastIndexOf("test", null));
    assertEquals(-1, StringUtils.lastIndexOf("", " "));
    assertEquals(-1, StringUtils.lastIndexOf("", "text"));
    assertEquals(-1, StringUtils.lastIndexOf("null", "nil"));
    assertEquals(0, StringUtils.lastIndexOf("", ""));
    assertEquals(2, StringUtils.lastIndexOf("  ", ""));
    assertEquals(1, StringUtils.lastIndexOf("  ", " "));
    assertEquals(0, StringUtils.lastIndexOf("null", "null"));
    assertEquals(15, StringUtils.lastIndexOf("This is example text!", " "));
    assertEquals(17, StringUtils.lastIndexOf("This is example text!", "ex"));
    assertEquals(16, StringUtils.lastIndexOf("This is example text!", "text"));
    assertEquals(8, StringUtils.lastIndexOf("This is example text!", "exam"));
    assertEquals(-1, StringUtils.lastIndexOf("This is example text!", "test"));
  }

  @Test
  public void testLength() {
    assertEquals(0, StringUtils.length(null));
    assertEquals(0, StringUtils.length(""));
    assertEquals(1, StringUtils.length(" "));
    assertEquals(1, StringUtils.length("\0"));
    assertEquals(3, StringUtils.length("   "));
    assertEquals(3, StringUtils.length("nil"));
    assertEquals(4, StringUtils.length("null"));
    assertEquals(4, StringUtils.length("test"));
  }

  @Test
  public void testSingleSpaceString() {
    assertEquals("This is a test!", StringUtils.singleSpaceString(" This is  a          test!  "));
    assertEquals("This_is_another_test!", StringUtils.singleSpaceString("This_is_another_test!"));
    assertEquals("null", StringUtils.singleSpaceString("null"));
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithBlankString() {
    StringUtils.singleSpaceString("   ");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithEmptyString() {
    StringUtils.singleSpaceString("");
  }

  @Test(expected = IllegalArgumentException.class)
  public void testSingleSpaceStringWithNullString() {
    StringUtils.singleSpaceString(null);
  }

  @Test
  public void testSingleSpaceValues() {
    assertEquals("true false", StringUtils.singleSpaceValues(true, false));
    assertEquals("t e s t", StringUtils.singleSpaceValues('t', 'e', 's', 't'));
    assertEquals("1 0 1", StringUtils.singleSpaceValues(1, 0, 1));
    assertEquals("3.14159", StringUtils.singleSpaceValues(3.14159d));
    assertEquals("false \0 c 0 3.14159 mock", StringUtils.singleSpaceValues(false, '\0', 'c', 0, 3.14159d, "mock"));
    assertEquals("test", StringUtils.singleSpaceValues("test"));
    assertEquals("null", StringUtils.singleSpaceValues("  null "));
    assertEquals("this   is  a     test!", StringUtils.singleSpaceValues("  this", "  is ", "a", "    test!   "));
    assertEquals("this is a test with a null value", StringUtils.singleSpaceValues("this", "is", "a", "test", "with", "a", null, "value"));
  }

  @Test
  public void testToCharArray() {
    char[] charArray = StringUtils.toCharArray(null);

    assertNotNull(charArray);
    assertEquals(0, charArray.length);

    charArray = StringUtils.toCharArray("abc");

    assertNotNull(charArray);
    assertEquals('a', charArray[0]);
    assertEquals('b', charArray[1]);
    assertEquals('c', charArray[2]);
  }

  @Test
  public void testToLowerCase() {
    assertNull(StringUtils.toLowerCase(null));
    assertEquals("", StringUtils.toLowerCase(""));
    assertEquals("  ", StringUtils.toLowerCase("  "));
    assertEquals("test", StringUtils.toLowerCase("test"));
    assertEquals("test", StringUtils.toLowerCase("TEST"));
    assertEquals("captain hook", StringUtils.toLowerCase("Captain Hook"));
    assertEquals("80013@n", StringUtils.toLowerCase("80013@N"));
  }

  @Test
  public void testToUpperCase() {
    assertNull(StringUtils.toUpperCase(null));
    assertEquals("", StringUtils.toUpperCase(""));
    assertEquals("  ", StringUtils.toUpperCase("  "));
    assertEquals("TEST", StringUtils.toUpperCase("TEST"));
    assertEquals("TEST", StringUtils.toUpperCase("test"));
    assertEquals("CAPTAIN HOOK", StringUtils.toUpperCase("Captain Hook"));
    assertEquals("80013@N", StringUtils.toUpperCase("80013@n"));
  }

  @Test
  public void testTrim() {
    assertNull(StringUtils.trim(null));
    assertEquals("", StringUtils.trim(""));
    assertEquals("", StringUtils.trim(" "));
    assertEquals("", StringUtils.trim("   "));
    assertEquals("abc", StringUtils.trim("abc"));
    assertEquals("123", StringUtils.trim(" 123"));
    assertEquals("abc123", StringUtils.trim("abc123 "));
    assertEquals("xyz", StringUtils.trim(" xyz "));
    assertEquals("xyz", StringUtils.trim("   xyz "));
    assertEquals("x y z", StringUtils.trim("x y z"));
    assertEquals("x y z", StringUtils.trim("  x y z "));
    assertEquals("_TT_", StringUtils.trim("_TT_"));
    assertEquals("_ TT _", StringUtils.trim(" _ TT _ "));
    assertEquals("spaceXspace", StringUtils.trim("spaceXspace"));
  }

  @Test
  public void testTrimAll() {
    assertNull(StringUtils.trimAll(null));
    assertEquals("", StringUtils.trimAll(""));
    assertEquals("", StringUtils.trimAll("  "));
    assertEquals("abc", StringUtils.trimAll("abc"));
    assertEquals("abc", StringUtils.trimAll(" abc"));
    assertEquals("abc", StringUtils.trimAll(" abc  "));
    assertEquals("abc", StringUtils.trimAll(" a  b    c   "));
  }

}
