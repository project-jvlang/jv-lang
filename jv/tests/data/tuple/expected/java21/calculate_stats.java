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
        final Object stats = new Tuple3_Unknown_Unknown_Unknown(minCandidate, maxCandidate, total);
        final Object __jv_tuple_0 = stats;
        final Object minValue = __jv_tuple_0._1();
        final Object maxValue = __jv_tuple_0._2();
        final Object totalSum = __jv_tuple_0._3();
        System.out.println(String.format("calculateStats: min=%s max=%s total=%s", minValue, maxValue, totalSum));
    }
}

public record Tuple3_Unknown_Unknown_Unknown(Object _1, Object _2, Object _3) {}
