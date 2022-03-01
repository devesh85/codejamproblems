import org.junit.Assert;
import org.junit.Test;

import java.util.*;

import static junit.framework.TestCase.assertEquals;

public class TestIvewPrep {
    @Test
    public void testNumberOfHeaps1() throws Exception{
//        Assert.assertEquals(3, countBits(1030));
//
////        System.out.println(-16>>2);
////        System.out.println(-16>>>2);
//
//
//        System.out.println(100 >>> 100);

//
//        Assert.assertEquals(42, reverse(24));
//        Assert.assertEquals(1, reverse(1));
//        Assert.assertEquals(1, reverse(10));
//        Assert.assertEquals(53, reverse(350));
//        Assert.assertEquals(5321, reverse(1235));


        System.out.println(isPalindrome(43134));
    }




    /**
     *       11
     * XOR   01
     *      =====
     *       10
     *
     *
     *       10
     * XOR   01
     *      =====
     *       11
     *
     * Truth Table
     *  x    y    XOR  OR AND
     *  0    0     0   0   0
     *  0    1     1   1   0
     *  1    0     1   1   0
     *  1    1     0   1   1
     *
     *
     *
     * binary operation
     * left shift by 1 is like multiplying number by 2
     * right shift by 1 is like dividing the number by 2
     *
     * x & 1 is and  "and" boolean operation
     *
     *
     *    admin -- 1
     *    user --  2
     *    xyz  --  4
     *
     *    perm   45
     *
     *    45 & 2
     *
     *
     *    ======
     *    011111
     *
     *
     *    perm user + xyz + someopohet
     * @param x
     * @return
     */
    public static short countBits(int x) {
        short numBits = 0;
        while (x != 0) {
            numBits =  (short) (numBits + (x & 1)) ;
            x >>>= 1;
        }
        return numBits; }


    public static short parity(long x) {
        x = x ^  (x >>>32);
        x ^= x >>>16;
        x ^= x >>>8;
        x ^= x >>>4;
        x ^= x >>>2;
        x ^= x >>>1;
        return (short) (x & 0x1);
    }


    public static int reverse (int x){
        int result = 0;
        while(x != 0){
            int mod = x % 10;
            result = result * 10 + mod;
            x = x/10;
        }

        return result;
    }


    public static boolean isPalindrome(int x){
        if(x <0 )
            return false;

        while (x > 0){
            int numDigit = 1 + (int) Math.floor(Math.log10(x));
            double pow = Math.pow(10, numDigit - 1);
            int msdMask = ( int ) (x / pow);
            int remainder = x % 10;
            if(msdMask != remainder){
                return false;
            }
            x = ( int ) (x % pow);
            x = x /10;
        }

        return true;
    }

}


