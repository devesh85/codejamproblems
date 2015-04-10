import org.junit.Test;

import java.util.*;

import static junit.framework.TestCase.assertEquals;

/**
 * Created by devesh on 4/9/15.
 *
 * https://www.hackerrank.com/challenges/to-heap-or-not-to-heap
 */
public class TestHeapOrNotToHeap {
    int k;
    int ptr;

    List<Integer> a = new ArrayList<Integer>();

    /**
     * Find the total number of heaps that could be created using the list of numbers
     * @throws Exception
     */
    @Test
    public void testNumberOfHeaps1() throws Exception{
        Integer [] arr = {41, 32, 24, 23, 2};

        assertEquals(2, findNumberOfHeaps(Arrays.asList(arr)));
    }
    @Test
    public void testNumberOfHeaps2() throws Exception{
        Integer [] arr = {41, 2, 32, 24, 23};

        assertEquals(1, findNumberOfHeaps(Arrays.asList(arr)));
    }

    public int findNumberOfHeaps(List<Integer> arrayList){
        return 0;
    }

    @Test
    public void testHeapFirst() throws Exception{
        List<Integer> val = new ArrayList<Integer>();
        val.add(2);
        val.add(23);
        val.add(32);
        val.add(41);
        val.add(24);
        generate(val);
        System.out.println("\n");
        for(int x: a){
            System.out.print(x + " ");
        }
    }

    public void generate(List<Integer> val){
//        for(int i=0; i< val.size(); i++){
//            val.add((int) (Math.random() * 100));
//        }
        ptr = 0;

        Comparator<Integer> comp = new Comparator<Integer>() {
            @Override
            public int compare(Integer o1, Integer o2) {
                return o2 - o1;
            }
        };
        Collections.sort(val, comp);
        genHeap(val, 0);
    }

    private void genHeap(List<Integer> values, int level) {
        k = values.size();
        if(k==0){
            return;
        }
        a.add(ptr, values.get(0));
        System.out.print("\n"+values.get(0) + " Level:" + level );

        ptr++;
        if(k== 1){
            return;
        }
        System.out.print(" Children:");

        for(int i=1; i< values.size(); i++){
            System.out.print(values.get(i) + " ");
        }
        System.out.println("");
        List<Integer> left = new ArrayList<Integer>();
        List<Integer> right = new ArrayList<Integer>();
        for(int i=1; i< k-1; i++){
            if(Math.random() > 0.5){
                left.add(values.get(i));
            }else{
                right.add(values.get(i));
            }
        }
        if(left.size() % 2 == 0){
            left.add(values.get(k-1));
        }else{
            right.add(values.get(k-1));
        }
        genHeap(left, level+1);
        genHeap(right, level+1);
    }

}
