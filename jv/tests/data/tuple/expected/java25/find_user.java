package tuple.integration;

import java.lang.String;

public final class GeneratedMain {
    
    public static void main(String[] args) throws java.lang.Exception {
        final User selected = new User(9, "Bob");
        final (User Bool) findUserResult = new Tuple2_User_Bool(selected, true);
        final (User Bool) __jv_tuple_0 = findUserResult;
        final Object user = __jv_tuple_0._1();
        final Object found = __jv_tuple_0._2();
        System.out.println(String.format("findUser: id=%s found=%s", user.id, found));
    }
}

public record Tuple2_User_Bool(User _1, Bool _2) {}

record User(int id, String name) {}
