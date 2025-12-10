package tuples.codegen;

public final class GeneratedMain {
    static final int firstValue = 12;
    static final int secondValue = 34;
    static final Tuple2_Int_Int pair = new Tuple2_Int_Int(firstValue, secondValue);
    static int firstComponent;
    static int secondComponent;
    
    public static void main(String[] args) throws Exception {
        firstComponent = pair._1();
        secondComponent = pair._2();
    }
}

public record Tuple2_Int_Int(int _1, int _2) {
    public int firstValue() { return this._1(); }
    public int secondValue() { return this._2(); }
}
