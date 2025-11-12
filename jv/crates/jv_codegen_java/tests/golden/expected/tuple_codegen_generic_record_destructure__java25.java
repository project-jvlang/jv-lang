package tuples.codegen;

public final class GeneratedMain {
    public static void main(String[] args) throws Exception {
        final int firstValue = 12;
        final int secondValue = 34;
        final Tuple2_Int_Int pair = new Tuple2_Int_Int(firstValue, secondValue);
        int firstComponent;
        int secondComponent;
        firstComponent = pair._1();
        secondComponent = pair._2();
    }
}

public record Tuple2_Int_Int(int _1, int _2) {}
