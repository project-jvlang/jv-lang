package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    public static void main(String[] args) throws java.lang.Exception {
        final int dividend = 27;
        final int divisor = 4;
        final int divmodQuotient = dividend / divisor;
        final int divmodRemainder = dividend % divisor;
        final Tuple2_Int_Int divmodResult = new Tuple2_Int_Int(divmodQuotient, divmodRemainder);
        final Tuple2_Int_Int __jv_tuple_0 = divmodResult;
        final int quotient = __jv_tuple_0._1();
        final int remainder = __jv_tuple_0._2();
        System.out.println(String.format("divmod: quotient=%s remainder=%s", quotient, remainder));
    }
}

public record Tuple2_Int_Int(int _1, int _2) {
    public int quotient() { return this._1(); }
    public int divmodQuotient() { return this._1(); }
    public int remainder() { return this._2(); }
    public int divmodRemainder() { return this._2(); }
}
