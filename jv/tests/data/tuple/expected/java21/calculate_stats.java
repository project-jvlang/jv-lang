package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    public static void main(String[] args) throws java.lang.Exception {
        final int a = 9;
        final int b = 2;
        final int c = 5;
        final int minCandidate = 2;
        final int maxCandidate = 9;
        final int total = a + b + c;
        final (Int Int Int) stats = new Tuple3_Int_Int_Int(minCandidate, maxCandidate, total);
        final (Int Int Int) __jv_tuple_0 = stats;
        final Object minValue = __jv_tuple_0._1();
        final Object maxValue = __jv_tuple_0._2();
        final Object totalSum = __jv_tuple_0._3();
        System.out.println(String.format("calculateStats: min=%s max=%s total=%s", minValue, maxValue, totalSum));
    }
}

public record Tuple3_Int_Int_Int(int _1, int _2, int _3) {}
