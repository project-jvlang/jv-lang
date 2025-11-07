package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    public static void main(String[] args) throws java.lang.Exception {
        final int dividend = 27;
        final int divisor = 4;
        final int divmodQuotient = dividend / divisor;
        final int divmodRemainder = dividend % divisor;
        final Object divmodResult = new Tuple2_Unknown_Unknown(divmodQuotient, divmodRemainder);
        final Object __jv_tuple_0 = divmodResult;
        final Object quotient = __jv_tuple_0._1();
        final Object remainder = __jv_tuple_0._2();
        System.out.println(String.format("divmod: quotient=%s remainder=%s", quotient, remainder));
    }
}

public record Tuple2_Unknown_Unknown(Object _1, Object _2) {}
