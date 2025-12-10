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
        final Tuple3_Int_Int_Int stats = new Tuple3_Int_Int_Int(minCandidate, maxCandidate, total);
        final Tuple3_Int_Int_Int __jv_tuple_0 = stats;
        final int minValue = __jv_tuple_0._1();
        final int maxValue = __jv_tuple_0._2();
        final int totalSum = __jv_tuple_0._3();
        System.out.println(String.format("calculateStats: min=%s max=%s total=%s", minValue, maxValue, totalSum));
    }
}

public record Tuple3_Int_Int_Int(int _1, int _2, int _3) {
    public int minCandidate() { return this._1(); }
    public int maxCandidate() { return this._2(); }
    public int total() { return this._3(); }
}
