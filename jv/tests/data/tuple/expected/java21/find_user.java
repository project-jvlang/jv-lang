package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    public static void main(String[] args) throws java.lang.Exception {
        final User selected = new User(9, "Bob");
        final Object findUserResult = new Tuple2_Unknown_Unknown(selected, true);
        final Object __jv_tuple_0 = findUserResult;
        final Object user = __jv_tuple_0._1();
        final Object found = __jv_tuple_0._2();
        System.out.println(String.format("findUser: id=%s found=%s", user.id, found));
    }
}

public record Tuple2_Unknown_Unknown(Object _1, Object _2) {}

record User(int id, String name) {}
